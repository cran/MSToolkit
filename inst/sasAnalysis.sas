OPTIONS mprint ;

%MACRO SASAnalysis;

%let infile  = %scan(&sysparm,1,'#');
%let outfile = %scan(&sysparm,2,'#');
%let code    = %scan(&sysparm,3,'#');
%let subset  = %scan(&sysparm,4,'#');
%let changes = %scan(&sysparm,5,'#');
%let seed    = %scan(&sysparm,6,'#');

proc import datafile = "&infile" out = work.infile dbms = dlm replace;
delimiter=',';
getnames=yes;
DATAROW=2; 
run;
options mprint ;

data infile; set infile;
seed = &seed;
&changes;
run;

data infile; set infile;
&subset;
run;

%INCLUDE "&code" ;

proc export data=work.outfile outfile="&outfile" dbms=csv replace;
run;

%MEND;

%sasanalysis;
