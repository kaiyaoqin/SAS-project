/*version 1 (non-delimited ds)*/
filename df 'C:\Users\Jesus kid\Desktop\kqylib\New_Wireless_Fixed.txt';

data ds;
infile df;
format  acctno $13.  actdt mmddyy10.  deactdt mmddyy10.  deactreason $4.  goodcredit 1.  rateplan 1. dealertype $2.
 AGE 2. Province $2. sales dollar10.2;
input @1 acctno $13. @15 actdt mmddyy10. @26 deactdt mmddyy10. @41 deactreason $4. @53 goodcredit 1. @62 rateplan 1. @65 dealertype $2.
@74 AGE 2. @80 Province $2. sales dollar10.2;
run;

proc print data= ds (obs=10);
run;

proc contents data=ds; 
run;

data WORK.NEW;
set WORK.Ds(keep=age);
if 2<=age < 10 then age=1;
else if age >= 10 AND age LT 20 then age=2;
else if age <2 then age =16; * missing value in the original age variable will obtain a new value of 16 
(var< number.: it will includes missing value);
else age=3;
run;


/*Q1: number of unique values in acctno variable*/
proc sql;
   select count(distinct acctno) as unique_account_count
   from ds
;
quit;



/* When is the earliest andv latest activation and deactivation dates available, respectively?*/

/*sort by actdt*/
proc sort data= ds out=sort;
by actdt;
run;

/*first and last 5 observation (sorted by actdt)*/
data first_last/view= first_last;
  set sort nobs=__nobs;
  if _n_ le 5 or _n_ gt __nobs-5;
run;
proc print data=first_last;
 TITLE 'first and last 5 observation (sorted by actdt)';
run;



/*sort by deactdt (exclude missing value)*/
proc sort data= ds out=sort1;
by deactdt;
Where not missing(deactdt);
run;

/* first and last 5 observation (sorted by deactdt, exclude missing value)*/
data first_last1/view= first_last1;
  set sort1 nobs=__nobs;
  if _n_ le 5 or _n_ gt __nobs-5;
run;

proc print data=first_last1;
TITLE 'first and last 5 observation (sorted by deactdt, exclude missing value)';
run;



/*count of customers with both activation and deactivation*/
proc sql;
TITLE 'count for deactivated customers';
   select count(*) as act_deact_count
   from ds
   where actdt IS NOT NULL and deactdt is not null;
   
quit;


proc sql;
TITLE 'count for active customers';
   select count(*) as act_deact_count
   from ds
   where actdt IS NOT NULL and deactdt is null;
quit;









/*Q2: distribution of age grouped by province for customers with both act and deact*/
proc format;
value act_status
	. = 'activated'  
	other = 'deactivated';

/*dataset with activation and deactivation */
data ds2a;
set ds;
activation = put(deactdt,act_status.);
run;

proc sql;
TITLE 'max age';
select max (age) from ds;quit;

proc univariate data= ds2a;
TITLE 'age distribution for active and deactivated customers';
class activation;
var age;
histogram age/ endpoints=(0 to 99 by 5) normal kernel;
run;


proc freq data=ds2a  order=freq;
TITLE 'province distribution for active and deactivated customers';
   tables activation*province /  expected norow  nocol cellchi2 chisq nopercent
       plots=freqplot(twoway= cluster orient=vertical);
run;








/*http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a002473474.htm*/
/*Q3: Segment the customers based on age, province and sales amount:*/
proc format;
value saleFmt  
      low  -<  100 = "<100)"       
      100 -<  500 = "[100,500)"        
      500 -<  800 = "[500,800)"   
      800 - high = ">800]"        
     ;
value AgeFmt  
      low  - 20 = "<20]"       
      21 - 40 = "[21,40]"      
      41 - 60 = "[41,60]"   
      61 - high = ">61]"    
	  ;
run;



/*dataset containning sale* age  segment, and province */
data ds1;
set ds;
format sales saleFmt. age AgeFmt. ;  
run;



  ods MSOffice2K body='C:\Users\Jesus kid\Desktop\kqylib\Q3.xls';
Proc Tabulate data=ds;
TITLE '(active) freq table of province , age * sale segments ';
    Class province age sales;
    Table (province ALL), (age ALL)*(sales ALL);
	where deactdt is null and actdt is not null;
    Keylabel ALL= 'Total';
    format sales saleFmt. age AgeFmt. ;
Run;

Proc Tabulate data=ds ;
TITLE '(deactivated) freq table of province , age * sale segments ';
    Class province age sales;
    Table (province ALL), (age ALL)*(sales ALL);
	where deactdt is not null and actdt is not null;
    Keylabel ALL= 'Total';
    format sales saleFmt. age AgeFmt. ;
Run;
  ods MSOffice2K close;





/*Q4. Statistical Analysis:*/
/*1) Calculate the tenure in days for each account */

/*dataset containning tenure*/
data tenure0 (drop=fiscal_date) ;
set ds ;
format end_date MMDDYY10.;
fiscal_date = input('03/31/2001',MMDDYY10.);
tenure= intck('day', actdt, deactdt);
if deactdt=. then tenure= intck('day', actdt, fiscal_date);
run;

/*min and max tenure*/
proc sql;
TITLE 'min and max tenure ';
select min(tenure) as min, max(tenure) as max 
from tenure0;
quit;

/*tenure distribution*/
proc univariate data= tenure0 ;
TITLE 'tenure distribution ';
var tenure;
histogram tenure/ endpoints=(0 to 801 by 30) normal kernel;
run;



/*2) Calculate the number of accounts deactivated for each month.*/

/*create a table only containing deactivated cutomers with deactivated month*/
proc sql;
create table act_deact as
   select * 
   from ds
   where actdt IS NOT NULL and deactdt is not null;
quit;

data test;
set act_deact;
deact_month = put(deactdt, month.); *convert datetime to month format, save it into new category variable; 
/*put statement can convert datetime to number of days from 1960 1 1:
num_days= PUT(deactdt, $comma6.);
*/

run;



/* frequency distribution of accounts deactivated for each month.*/
/*method 1*/
Proc Tabulate data=test;
TITLE 'frequency distribution of accounts deactivated for each month (1) ';
    Class deact_month;
    Table (deact_month all);
	where deactdt is not null and actdt is not null;
    Keylabel ALL= 'Total';
Run;

/*method 2*/
proc freq data=test ;
TITLE 'frequency distribution of accounts deactivated for each month (2) ';
   tables deact_month/
       plots=freqplot(twoway= cluster orient=vertical);
run;


/*3)Forecast the number of accounts that will deactivate for the subsequent 6 months.*/
data forcast;
set ds;
format act_mmyy monyy7. deact_mmyy monyy7.;
deact_mmyy = input(put(deactdt,monyy7.), monyy7.);
act_mmyy = input(put(actdt,monyy7.), monyy7.);
run;


/*join active count and deactivated count by monthyear*/
proc sql;
   create table forcast1 as
   select distinct deact_mmyy as monthyear1, count(*) as deact_count
   from forcast
   where deactdt <> .
   group by monthyear1 
   order by monthyear1
;
   create table forcast2 as
   select distinct act_mmyy as monthyear2, count(*) as act_count
   from forcast
   where deactdt =.
   group by monthyear2
   order by monthyear2
;
 Create Table forcast3 as
    Select monthyear1, act_count, deact_count, deact_count/(deact_count+act_count) as churn_rate format=PERCENT9.2
	From forcast1 t1 inner join forcast2 t2
    on t1.monthyear1 = t2.monthyear2
    order by t1.monthyear1
    ;

quit;

Data forcast3 ;
    Retain Month_no monthyear1 act_count deact_count churn_rate;
    Set forcast3;
    Month_no = _N_;
Run;

/*method1: proc reg*/

proc sgplot data=forcast3;
	reg x=month_no y=churn_rate / transparency=0.3 
                                  markerattrs=(symbol=CircleFilled color=blue size=12px) 
                                  lineattrs=(color=red thickness=2); 
run;

proc reg data=forcast3;    
    model Churn_rate = Month_no;

run;

/*
Churn Rate = 0.0131*Month_no - 0.0092 

Next 6th month, Month_no = 25+6 = 31
Predicted Churn Rate (Next 6 months) = 0.0131*31 - 0.0092
                                     = 0.3969
Predicted deactivated customer = Predicted Churn rate*(Total_active + Total_deactivated in July 2001) or total registered customers
                               = 0.3969*(3283 + 1856) for example
                               = 2040 (approx)
*/

/*method2: proc reg*/
proc print data= forcast1;
title"count of deactivation thru out the time span of the dataset (monthly interval)";
run;

proc forecast data=forcast1 interval=month lead=6 out=forcast_6month;
id monthyear1;
var deact_count;
run;

proc print data= forcast_6month;
title"forecast of the subsequent 6 months after the end date of the original dataset";
run;




/*4) Segment the account, first by account status active·and deactivated· then by
Tenure: < 30 days, 31---60 days, 61 days--- one year, over one year. Report the
number of accounts of percent of all for each segment.*/


proc format;
value deact
	. = 'activated'  
	other = 'deactivated'  ;
value tenure 
	low  -  30 = "<30]"       
      31 -  60 = "[31,60)"        
      61 -  365 = "[61,365]"   
      366 - high = "over 1 yr"  
	 
   ;
run;

/*dataset containning activation status and tenure segment*/
data ds2;
set tenure0;
activation = put(deactdt,deact.);
tenure_seg= put(tenure,tenure.);
run;

/*frequency table including tenure segment and both activated and deactivated account*/

Proc Tabulate data=ds2  ;
title'frequency table including tenure segment and both activated and deactivated account';
    Class activation tenure_seg;
    Table (activation all),(tenure_seg all)*(n pctn)/row=float;
    Keylabel ALL='margin sum'
    n = 'Count'
    pctn = 'Percent';
Run;

/*frequency table including tenure and only deactivated account*/

Proc Tabulate data=ds2  ;
title'frequency table including tenure and only deactivated account';
    Class activation tenure_seg;
    Table (activation all),(tenure_seg all)*(n pctn)/row=float;
	where activation= 'deactivated';
    Keylabel ALL='margin sum'
    n = 'Count'
    pctn = 'Percent';
Run;


/*frequency table including tenure and only active account*/

Proc Tabulate data=ds2  ;
title'frequency table including tenure and only deactivated account';
    Class activation tenure_seg;
    Table (activation all),(tenure_seg all)*(n pctn)/row=float;
	where activation= 'activated';
    Keylabel ALL='margin sum'
    n = 'Count'
    pctn = 'Percent';
Run;



/*5)Test the general association between the tenure segments and “Good Credit”,“RatePlan ” and “DealerType.”*/

proc freq data=ds2 ;
title 'chisq freq table between the tenure segments and “Good Credit”,“RatePlan ” and “DealerType';
table tenure_seg * goodcredit/ expected norow  nocol chisq ;
table tenure_seg * rateplan/ expected norow nocol chisq ;
table tenure_seg * dealertype/ expected norow  nocol chisq;
run;



/*6) Test the general association between the activation status and “Good Credit”,“RatePlan ” and “DealerType.”*/
proc freq data=ds2 ;
title 'chisq freq table between the activation status and “Good Credit”,“RatePlan ” and “DealerType';
table activation * goodcredit/ expected norow  nocol chisq relrisk;
table activation * rateplan/ expected norow nocol chisq ;
table activation * dealertype/ expected norow  nocol chisq ;
run;



/* 7)association between the account status and the tenure segments*/

proc freq data=ds2;
title 'chisq freq table between the activation status and tenure segments';
table tenure_seg * activation/ expected cellchi2 norow  nocol chisq;
run;





/*8) Does Sales amount differ among different account status, GoodCredit, and customer age segments?*/

/*Does Sales amount differ among different account status*/
Proc Ttest Data=ds2 COCHRAN side=2 alpha=0.05 h0=0 test=diff; * t-p is lower than the 0.05, reject H0;
title'method1:t test (paramatric): compare activation status';
class activation;
var sales;
Run;

proc NPAR1WAY data=ds2 wilcoxon median;
title'method2: Wilcoxon rank-sum test(same as the Mann-Whitney U test)(non-paramatric)';
class activation;
var sales;
run;





/*Does Sales amount differ among different gredit*/
Proc Ttest Data=ds2 COCHRAN side=2 alpha=0.05 h0=0 test=diff; * t-p is lower than the 0.05, reject H0;
title'method1:t test (paramatric): compare credit groups';
class GoodCredit;
var sales;
Run;

proc NPAR1WAY data=ds2 wilcoxon median;
title'method2: Wilcoxon rank-sum test(same as the Mann-Whitney U test)(non-paramatric)';
class GoodCredit;
var sales;
run;


/*Does Sales amount differ among age segment (ANOVA)*/

/*assumption for anova:
Independence of cases ·this is an assumption of the model that simplifies the statistical analysis.
Normality ·the distributions of the residuals are normal  (NOT MET, THUS NON_PARAMETRIC ASSUMED, AND ANOVA NOT THE BEST OPTION).
Equality (or "homogeneity") of variances, called homoscedasticity.
*/

data ds3;
set ds;
format age AgeFmt. ;  
run;

proc univariate data=ds3 normal mu0=0;
title'normality check';
	ods select TestsForNormality;
	class age;
	var sales;
run;

Proc Anova Data=ds3 ; 
title'parametric model';
    Class age;
    Model sales  = age; * need to define y= numeric, and x=class in model ; 
    Means age/hovtest=levene SCHEFFE; * south and west are different
	*Scheffe's multiple-comparison procedure on all main-effect means in the MEANS statement;
	* there is also tukey,LSD, GT2;
Run;




*NON-PARAMETRIC: method 1: find the kruskal-wallis ranks of the whole data, 
then do the regular ANOVA and Posthoc test on it (abandoned the original values);
proc rank data=ds3 out=Ranked_ds3 ties=mean ;
   var sales; **there is also 'by' option in the 'rank' statement;
   ranks salesRank;
run;

proc glm data=Ranked_ds3 ;
title'NON-PARAMETRIC: method 1: anova based on rank, and test equal sales by least_sq mean';
	class age;
	model salesRank=age;
	means age / hovtest=levene SCHEFFE welch bon tukey alpha=.05 plots=none;
	lsmeans age / adjust=tukey pdiff alpha=.05  plots=meanplot;
run;

quit;






*NON-PARAMETRIC: Method 2: use Kruskal-Wallis Test (based on ranking of the data, and test the difference in sale by rank mean);
proc npar1way data=ds3 wilcoxon median dscf plots(only)=(wilcoxonboxplot medianplot);
**dscf is the pairwise comparsion based on Kruskal-Wallis ranking (Dwass-Steel-Critchlow-Fligner);
title'NON-PARAMETRIC: Method 2: use Kruskal-Wallis Test (based on ranking of the data, and test the difference in sale by rank mean';
	class age;
	var sales;
run;




