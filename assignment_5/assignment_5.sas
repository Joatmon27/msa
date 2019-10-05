
libname pw "H:\Werk\Multivariate Statistical Analysis\msa\assignment_5\";

data pw.t8_4;
	infile "H:\Werk\Multivariate Statistical Analysis\msa\assignment_5\T8-4.csv" dlm=',';
	input JPM Citibank WellsFargo RDS Exxon;
run;
/*
proc factor data=pw.t8_4 hey method=ml corr nfact=2 score out=fml;
run;
*//*
data pw.ex9_9(type=corr);
_type_='CORR';
input _name_$ taste money flavor snack energy;
cards;
taste 1.00 . . . . 
money 0.02 1.00 . . . 
flavor 0.96 0.13 1.00 . .
snack 0.42 0.71 0.5 1.00 .
energy 0.01 0.85 0.11 0.79 1.00
;
proc factor res data=pw.ex9_9
method=prin nfact=2rotate=varimax preplot plot;
var taste money flavor snack energy;
run;
*/
/*
proc factor res data=pw.t8_4
method=prin nfact=2rotate=varimax preplot plot;
var JPM Citibank WellsFargo RDS Exxon;
run;
*//*
proc factor data=pw.t8_4 hey method=ml rotate=varimax nfact=2 score out=Factor_analysis;
title "Question 4";
run;

proc gplot data=Factor_analysis;
plot factor2*factor1;
title "Question 4 - Factor Scores";
run;
*/
data pw.t8_5;
	infile "H:\Werk\Multivariate Statistical Analysis\msa\assignment_5\T8-5.csv" dlm=',';
	input Total_Pop Prof_Degree Employed Gov_Empl Med_HV;
run;

proc factor data=pw.t8_5 method=prin rotate=varimax nfact=2 score out=prin_result;
run;

proc print data=prin_result (obs=15);
title "PCA Method";
var factor1 factor2;
run;
proc gplot data=prin_result;
plot factor2*factor1;
run;

*Rotated maximum likelihood method:*;
proc factor data=pw.t8_5 hey method=ml rotate=varimax nfact=2 score out=ml_result;
run;

proc print data=ml_result (obs=15);
title "ML Method";
var factor1 factor2;
run;

proc gplot data=ml_result;
plot factor2*factor1;
run;

