proc import out=mites datafile="/home/u45187512/ST662/MiteResponse.csv" 
	dbms=csv replace;
	getnames=yes;
run;

data mites;
	set mites;
	where abundance_mesostigmata ne . and abundance_mesostigmata ne 91;
	run;
proc genmod data= mites;
	class month pyramid area;
	model abundance_mesostigmata = temperature humidity organic_matter month 
		x600 x300 pyramid area/ dist=zip;
	zeromodel month x600 x300;
	output out=out pred=predzip;
	run;
proc sgplot data=out;
	scatter x=abundance_mesostigmata y=predzip;
	run;










