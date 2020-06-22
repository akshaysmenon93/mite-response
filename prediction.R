library(tidyverse)
library(plotly)
library(leaps)
library(MASS)
library(tidyverse)
library(lme4)
library(car)
library(pscl)
library(bestglm)

data <- read.csv("MiteResponse.csv")
summary(data)   #There are four rows with missing values  

data %>% filter_all(any_vars(is.na(.))) #This finds all the NA values
#There is one row which has NA values for all columns including the response so it doesn't make sense 
#to impute it as it wouldn't be of much help in prediction.
#The other 3 rows having NA's at sp.Gamasina and sp.Uropodina and also the response as we don't need to 
#bother about the first two columns we can safely ignore and since the response has NA values in this case
#it wouldn't make much sense to impute these values either.
#Hence we decided to omit all the NA values due to these reasons.

data <- na.omit(data)

#Now we omitted all the columns that were not needed for prediction i.e the other species from the data
#We also omitted the ID as it was not much helpful in prediction.
data1 <- data.frame(data[c(2:14,17)])
summary(data1)
table(data1$abundance_mesostigmata)
#The summary as well as the table outputs show that the observation with 91 value for the response 
#could possibly be an outlier as it so far off from the rest of the observations. We omit this observation
#too as it could cause biased predictions.
data1 <- data1[-125,]

data1$month <- fct_relevel(data1$month,levels=c("february",'april','june','august','october','december'))
#So that months are plotted in chronological order

ggplot(data=data1)+
  geom_boxplot(aes(y=temperature, x=month, colour=area), position=position_dodge())
#To compare the temperatures in the interior and exterior of pyramids across the entire year

#Omit column 1 because ID has no significance in prediction


#Initial fit to use as a baseline for making comparisons after tweaking the models
model <- glm(abundance_mesostigmata~.,data=data1)
summary(model)  
AIC(model)  #The AIC of the base model is 1243.89 and multiple predictors seem to be non important


bestAICmodel<-bestglm(data1,family = poisson, IC = "AIC")
#Using the bestglm function to find the best set of predictors for the model

summary(bestAICmodel)
bestAIC <- bestAICmodel$BestModel

AIC(bestAIC) #Thus the optimal set of predictors brought the AIC down to 1082.188

#Testing for overdispersion
pchisq(deviance(bestAIC),
       df.residual(bestAIC),
       lower.tail=FALSE)    #This suggests that there may be overdispersion in the model


quasi <- glm(abundance_mesostigmata~temperature+humidity+organic_matter+month
              +                   +x600+x300+pyramid+area,data=data1,family=quasipoisson())
AIC(quasi)

#There was a very large number of zeroes in the data suggesting that there maybe zero inflation in the data too 
zobs <- data1$abundance_mesostigmata == 0
zpoi <- ppois(0,predict(bestAIC, type = "response"))
c(obs=mean(zobs), poi=mean(zpoi))
#Model predicts 24% observations to be 0 but about 34% of observations are 0. This is evidence that there may 
#be Zero inflation in the data


plot(table(data1$abundance_mesostigmata))
#This plot further supports the evidence for zero inflation as there are quite a few more zero values

#Using a zero inflated model 
model_zero <- zeroinfl(abundance_mesostigmata~temperature+humidity+organic_matter+month
                       +x600+x300+pyramid+area|month+x600+x300, data=data1, dist="poisson")
AIC(model_zero) #AIC of 1007.274
#Since there was overdispersion as well we tried negative binomial as well
model_Zeronegbin <- zeroinfl(abundance_mesostigmata~pyramid+area+organic_matter+month+temperature+humidity
                             +x600+x300|x600+x300+temperature, data=data1, dist="negbin")
AIC(model_Zeronegbin)
#A zero inflated model of Negative Binomial distribution gives the best AIC values of 881

#Making predictions using both ZIP models
pred <- predict(model_zero,type="response") 
pred1 <- predict(model_Zeronegbin,type="response")

ggplot(data=na.omit(data1))+
  geom_point(aes(x=pred,y=abundance_mesostigmata))+
  geom_abline(slope=1,intercept=0)

ggplot(data=na.omit(data1))+
  geom_point(aes(x=pred1,y=abundance_mesostigmata))+
  geom_abline(slope=1,intercept=0)

#Comparing Mean Absolute Errors from both models shows not much of difference.
#Since the dataset is so small it isn't possible to do formal testing on a test set. So we decided to choose 
#the best model based on the basis of AIC.
mean(abs(pred-data1$abundance_mesostigmata))
mean(abs(pred1-data1$abundance_mesostigmata))
