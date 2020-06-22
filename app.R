library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinythemes)
library(pscl)
setwd(getwd())
mite <- na.omit(read.csv("MiteResponse.csv"))
mite_month <- mite
data.frame(mite[-125,c(2:14,17)])->mite
noout<-mite
mite<-mite[,-c(1,8,11,12)]
model_zero <- zeroinfl(abundance_mesostigmata~temperature+humidity+organic_matter+month
                       +x600+x300+pyramid+area|month+x600+x300, data=mite, dist="poisson")
noout_date <- noout
noout_date$month = paste("01", as.character(noout_date$month), "2010", sep="-")
noout_date$month = as.Date(noout_date$month, format = "%d-%B-%Y")

ui <- fluidPage(
    theme = shinytheme('united'),
    navbarPage('MiteResponse',
               tabPanel('Introduction',textOutput('introtext')),
               tabPanel('Analysis',textOutput('tempvsmeso_text'),plotOutput('tempvsmeso'),textOutput('monthvsmeso_text'),plotOutput('monthvsmeso'),
                        textOutput('timeseries_text'), plotOutput('timeseries'),
                        textOutput('pHvsmeso_intro'),
                        'Forest Land : ',plotOutput('pHvsmeso_forest'),'Pasture Land: ',plotOutput('pHvsmeso_pasture'),
                        textOutput('pHvsmeso_conclusion'),
                        textOutput('avgTempIntroText'), 
                        'Forest Land : ', plotOutput('avgTempvsmeso_forest'),'Pasture Land: ',plotOutput('avgTempvsmeso_pasture'),
                        textOutput('avgTempvsmesoConclusion'),
                        textOutput('humidityvsmeso_intro'),
                        'Forest Land : ', plotOutput('humidityvsmeso_forest'),'Pasture Land: ', plotOutput('humidityvsmeso_pasture'),
                        textOutput('humidityvsmeso_conclusion')
                        ),
                        
                        
               tabPanel('Predict Mites',
                        actionButton("runButton","Predict the Mites"),
                        tabsetPanel(
                            tabPanel("Input", rHandsontableOutput('mitetable')),
                            tabPanel("Predicted", DT::dataTableOutput("mitepredict"))
                         )
                       ),
               tabPanel('Results',textOutput('resulttext')),
               tabPanel('Conclusion',textOutput('concltext'))
    )
    )

server <- function(input, output,session) {
    values <- reactiveValues()
    output$mitetable <- renderRHandsontable({
        head(mite[,-10],4)->display
        rownames(display)<-NULL #was getting errors if we added a new record
        rhandsontable(display) %>%
            hot_col(col = "area",type = 'dropdown', source = unique(mite$area)) %>%
            hot_col(col = "pyramid",type = 'dropdown', source = unique(mite$pyramid)) %>%
            hot_col(col = "month",type = 'dropdown', source = unique(mite$month)) %>%
            hot_validate_numeric(col = "pH",min=0,max = 14)%>%
            hot_validate_numeric(col = "humidity",min=0,max = 100)%>%
            hot_validate_numeric(col = "organic_matter",min=0,max = 100)%>%
            hot_validate_numeric(col = "x600",min=0,max = 100)%>%
            hot_validate_numeric(col = "x300",min=0,max = 100)%>%
            hot_validate_numeric(col = "temperature",min=-10,max = 40)%>%
            hot_table(stretchH = 'all')
    })    
    observeEvent(input$runButton, {
        values$data <-  hot_to_r(input$mitetable)
            values$data<-cbind(round(predict(model_zero,values$data)),values$data)
    names(values$data)[1]<-'Mite'
    })
    output$tempvsmeso<-renderPlot({
        ggplot(noout, aes(x= temperature, y=abundance_mesostigmata)) + geom_point(aes(col = ecosystem)) +
            geom_smooth(method = lm, se = FALSE,aes(col=ecosystem)) + facet_wrap(~area)
    })
    output$mitepredict <- DT::renderDataTable({
        values$data
    }) 
    output$monthvsmeso<-renderPlot({
        noout %>% group_by(ecosystem, area, month) %>% summarise(abundance_mesostigmata = sum(abundance_mesostigmata, na.rm = T)) %>%
            ggplot(aes(x= month, y = abundance_mesostigmata)) + geom_col(aes(fill = area), position = "fill") + facet_wrap(~ecosystem)
    })
    output$pHvsmeso_forest<-renderPlot({
        noout %>% filter(ecosystem == "patch") %>% 
            ggplot(aes(x = pH, y=abundance_mesostigmata)) + geom_point(aes(color = pyramid)) +
            geom_smooth(method = lm, se = FALSE) + facet_wrap(~area)
    })
    output$pHvsmeso_pasture<-renderPlot({
        noout %>% filter(ecosystem == "pasture") %>% 
            ggplot(aes(x = pH, y=abundance_mesostigmata)) + geom_point(aes(color = pyramid)) +
            geom_smooth(method = lm, se = FALSE) + facet_wrap(~area)
    })
    output$avgTempvsmeso_forest <- renderPlot({
        noout %>% filter(ecosystem == "patch") %>% group_by(area, month) %>%
            summarise(avg_temp = mean(temperature, na.rm = T), abundance = sum(abundance_mesostigmata, na.rm =  T)) %>%
            ggplot(aes(x = month, y = avg_temp)) + geom_col(aes(fill = abundance)) + facet_wrap(~area)
    })
    output$avgTempvsmeso_pasture <- renderPlot({
        noout %>% filter(ecosystem == "pasture") %>% group_by(area, month) %>%
            summarise(avg_temp = mean(temperature, na.rm = T), abundance = sum(abundance_mesostigmata, na.rm =  T)) %>%
            ggplot(aes(x = month, y = avg_temp)) + geom_col(aes(fill = abundance)) + facet_wrap(~area)
    })
    output$humidityvsmeso_forest <- renderPlot({
        noout %>% filter(ecosystem == "patch") %>%
            ggplot(aes(x= humidity, y=abundance_mesostigmata)) + geom_point(aes(col = area)) + 
            geom_smooth(method = lm, se = FALSE) + facet_wrap(~area)
    })
    
    output$humidityvsmeso_pasture <- renderPlot({
        noout %>% filter(ecosystem == "pasture") %>%
            ggplot(aes(x= humidity, y=abundance_mesostigmata)) + geom_point(aes(col = area)) + 
            geom_smooth(method = lm, se = FALSE) + facet_wrap(~area)
    })
    output$timeseries <- renderPlot({
        noout_date %>%
            group_by(ecosystem, area,month) %>% summarise(abundance_mesostigmata = sum(abundance_mesostigmata, na.rm = T)) %>%
            ggplot( aes(x = month, y = abundance_mesostigmata)) + geom_line(aes(color = area)) + facet_wrap(~ecosystem)
    })
    
    output$introtext<-renderText('The case study is done by team of seven members to do data analysis on Mesostigmata mites in the soil during a year, in different ecosystems. The examination depended on the information gathered in the San Jose segment Nuevo Mundo, situated in the Blanco River bowl of the Municipality of Calera. The prairie was chosen as a differentiating biological system and thinking about that cows are one of the fundamental monetary exercises of the region. The prairie comprised of the species Pennisetum clandestinum Hochst. ExChiov. (Poaceae) and Lolium sp. (Poaceae). The separation between the biological systems was roughly 500 m.  An analysis was set up to reproduce the greenhouse effect and to record the reaction of edaphic Mesostigmata vermin during a year, in every environment. To reproduce the greenhouse effect, altered open chambers were built from past examinations (Marion et al., 1997; Aronson and Mcnulty 2009). In this case study we will be looking into a dataset collected over a period of year. During a time of one year, six samplings were completed in the principal seven day stretch of like clockwork: February, April, June, August, October and November 2010. Quickly before assortment, the dirt temperature was estimated at 10 cm profundity with a dirt thermometer. Soil tests were taken inside and outside each pyramid in all the periods. Each dirt example related to a chamber 5 cm in distance across and 15 cm profound, taken with a drill. Each example was set in a plastic pack and shipped in a fridge to the Laboratory of Ecology of Soils and Tropical Fungi, Bogotá, Colombia. In the wake of moving the soil examples to the research facility, everything was homogenized and 150g were taken for the extraction of the bugs. Expulsion of the fauna was finished by putting each example in a changed Berlese pipe, in which it remained roughly five days. The assortment jars of the pipes contained a 70% alcohol solution wherein the mites were put away. Here we will explore how the abundance of Mesostigmata mites is affected by the simulated greenhouse effect. We will also create predictive models to assess which predictors are important in the estimation of the abundance of these mites.')
    
    output$tempvsmeso_text <- renderText('We produced the following visualization to compare the variation in abundance of Mesostigmata found at different temperatures, external versus internal, for pasture and forest land. From this we see a higher percentage of Mesostigmata were found inside the pyramid rather than outside the pyramid indicating that the greenhouse simulation affected the abundance of Mesostigmata. The difference appears to be more significant for the patch ecosystem.')
    output$monthvsmeso_text<- renderText('We produced the following visualization to compare the variation in abundance of Mesostigmata found during each month, external versus internal, for pasture and forest land. From this we see a higher percentage of Mesostigmata were found inside the pyramid rather than outside the pyramid indicating that the greenhouse simulation affected the abundance of Mesostigmata.')
    
     output$timeseries_text <- renderText('The below plot is a time series analysis plot of variation in the abundance of mesostigmata in the forest and pasture 
   lands contrasting samples from the interior and exterior of the pyramid. We can see that on an average, the forest 
   land has a higher abundance of mesostigmata mites. Also, it can be noted that the samples from the interior of 
   the pyramids had a higher abundance of the mites as compared to the exterior. Seasonal variations in the abundance
   of mesostigmata mites can be seen as the months progress. ')
     
     
     output$pHvsmeso_intro <- renderText('The below plots shows the effect of pH on the variation 
    of abundance of Mesostigmata mites in the forest and pasture lands, contrasting measurements from interior and exterior
    of the pyramid')
    
    output$pHvsmeso_conclusion <- renderText('In the forest land, an increase in pH shows a slight decreasing trend in
   the abundance of mesostigmata mites in the exterior of the pyramid and a clear upward trend in the interior of the 
   pyramid. In the pasture land, the increase of pH does not indicate a significant variation in the abundance of 
   mesostigmata mites. This suggests that the effect of pH may be more prominent in the forest land than compared to 
   the pasture land.')
    
    output$avgTempIntroText <- renderText('Variation in abundance of mesostigmata with respect 
    to avg temp for each month in forest and pasture land is visualised in the below plots :')
    
    output$avgTempvsmesoConclusion <- renderText('We can see that the highest average temperature was recorded during 
    the month of February in both the forest and the pasture lands. During this month, a higher abundance of Mesostigmata
    mites were found in the interior of the pyramid than compared to the exterior for both the forest and pasture lands.
    Also, it can be noted that on an average, a higher amount of Mesostigmata mites were found in the forest land
    during the month of february, suggesting that temperature favored the growth of Mesostigmata mites in the forest land than
    compared to the pasture land.')
    
    output$humidityvsmeso_intro <- renderText('The below plots show the variation in abundance of mesostigmata mites with 
    respect to humidity, contrasting sample readings from the external and internal of the pyramids ')
    
    output$humidityvsmeso_conclusion <- renderText('In the forest land, the slope suggests that there is a steady decline in the abundance of mesostigmata mites with
   the increase in humidity but this is due to the presence of a few large observations. Ignoring them, we can
   see that there is no real effect of humidity on the abundance of mites in the forest land.
   However, in the pasture land there exists a steady increase in the abundance of mesostigmata mites with the increase
   in humidity.')
    
    output$resulttext<-renderText('We found the best set of predictors to be area, pyramid, month, humidity, organic matter, x600, x300 and temperature. To improve the model, we used these predictors to build a zero-inflation model. We found the zero inflated negative binomial to be the best prediction based on the AIC value of 881 compared to 1007. However, on comparing the Mean Absolute Errors from both models, it indicated that the Poisson regression gives a mean squared error value of 2.2913 which is better than the negative binomial model which gives a mean squared error value of 2.4228. ')
   
     output$concltext<-renderText('It was apparent that the greenhouse effect simulation affected the abundance of Megostigmata and the type of ecosystem also influenced how much the greenhouse effect simulation contributed to the abundance of Megostigmata. This was a small data set, so we chose our best set of predictors based on the AIC values and Mean Square error rather than splitting the data into testing and training data. ')
}

# Run the application 
shinyApp(ui = ui, server = server)
