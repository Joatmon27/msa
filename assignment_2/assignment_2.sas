%let path=H:\Werk\Multivariate Statistical Analysis\Assignment_2;

/* Creating a library named pw */

libname pw "H:\Werk\Multivariate Statistical Analysis\libs";


data pw.air_pol;
  infile "H:\Werk\Multivariate Statistical Analysis\Data\T1-5.dat" dlm=' ';
  input x1 x2 x3 x4 x5 x6 x7;
 run;

 proc print data=pw.air_pol;
 var x1 x2 x3 x4 x5 x6 x7;
run;

data pw.Data1 pw.Data2;
   set pw.air_pol ;
  if x7 = 3 then output pw.Data1;
  else output pw.Data2;
run;

proc print data=pw.Data1;
 var x1 x2 x3 x4 x5 x6 x7;
run;

proc print data=pw.Data2;
 var x1 x2 x3 x4 x5 x6 x7;
run;

data pw.Above50 pw.Below50;
   set pw.air_pol ;
  if x2 >= 50 then output pw.Above50;
  else if x2 < 50 then output pw.Below50;
run;

ods listing close;
ods rtf file="&path\assignment_2_ex1.rtf";

proc print data=pw.above50;
 var x1 x2 x3 x4 x5 x6 x7;
 sum x1 x2 x3 x4 x5 x6 x7;
run;

proc means data=pw.Above50;
title 'Exercise 1';
  var x3 x4 x5 x6;
run;

ods _all_ close;
ods listing;
title;
