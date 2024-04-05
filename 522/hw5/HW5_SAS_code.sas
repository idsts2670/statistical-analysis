/* This program provides SAS code to complete Homework 3, Texbook question 2.13, parts a) through e) */

/* first provide the replace my path with the path to your folder where you put the shapespop data */

LIBNAME datalib 'C:\Users\slchrist\OneDrive - purdue.edu\Purdue\Teaching\Sampling 2024\Homework\HW5';

/* we can copy the data to the work folder */

data shapespop;
   set datalib.shapespop;
run;

data baseball;
   set datalib.baseball;
run;

/* you can look at the data using the table editor. Click on Tools, then Table Editor. Next click on the 
   open file icon, choose the "Work" folder, then click on the data file called shapespop */


**########################################################### Problem 1: Objects Data ############################################################################
**#################################################################################################################################################################;



/* Part (a): Select stratified sample of size 200 using shape as the strata. Be sure to enter a seed number */

*first sort the strata variable;
proc sort data=shapespop;
by shape;
run;

proc surveyselect data=shapespop method = srs sampsize = 200 out = strsamp stats seed = ;
   strata shape/ alloc = prop;
   title 'Select proportionally allocated stratified random sample of size 200';
run;

*you can permanently save your sample in your datalib folder with the following data step;
data datalib.strsamp;
   set strsamp;
run;

*see if the distribution of your sample matches the distribution in the population ;
*population data;
proc freq data=shapespop;
table shape;
run;
*sample data;
proc freq data=strsamp;
table shape;
run;

/* Part (b): Estimate and mean and total (SAS calls this a sum) and the 95% confidence intervals for both */

PROC SURVEYMEANS DATA = strsamp TOTAL=20000 MEAN CLM SUM CLSUM;
   WEIGHT SamplingWeight;
   STRATA shape;
   VAR area;
   TITLE 'Obtain the mean and total estimates and their confidence intervals for area of the objects';
run;

** the code below gets the population quantity for the mean and total area;
PROC MEANS data=shapespop MEAN SUM ;
VAR area;
run;


/* Part (c): Get the total (SUM) estimate of the number of gray objects. Also get the 95% confidence interval for the total. */

PROC SURVEYMEANS DATA = strsamp TOTAL=20000 SUM CLSUM;
   CLASS color;
   WEIGHT SamplingWeight;
   STRATA shape;
   VAR color;
   TITLE 'Obtain the total estimate of the number of gray objects';
run;

** the code below gets the population quantity for the number of gray objects;
PROC FREQ DATA = shapespop;
table color;
run;



**########################################################### Problem 2: Baseball Data ############################################################################
**#################################################################################################################################################################;

/* Part (a): select and SRS of 150 players */

proc surveyselect data=baseball method = srs sampsize = 150 out = sampBB stats seed = ;
      title 'Select a simple random sample (SRS) of size 150';
run;

*you can permanently save your sample in your datalib folder with the following data step;
data datalib.sampBB;
   set sampBB;
run;


/* Part (b): select a stratified random sample of 150 players using team as the strata. Remember to add a seed number */

*first sort the strata variable;
proc sort data=baseball;
by team;
run;

proc surveyselect data=baseball method = srs sampsize = 150 out = stratBB stats seed = ;
   strata team/ alloc = prop;
   title 'Select proportionally allocated stratified random sample of size 150';
run;

*you can permanently save your sample in your datalib folder with the following data step;
data datalib.stratBB;
   set stratBB;
run;

/******************************* Note that player salary is very skewed, so we will use logsal, which is the log of salary *******************************/

*take a look at the population distribution of player salary and logsal;

PROC UNIVARIATE DATA=baseball;
HISTOGRAM;
VAR salary logsal;
run;


/* Part (c): estimate the proportion of players who are pitchers in each sample  */

*first use the SRS;

PROC SURVEYMEANS DATA = sampBB TOTAL=797 MEAN CLM ;
   CLASS pos;
   WEIGHT SamplingWeight;
   VAR pos;
   TITLE 'Obtain the proportion (mean) of players who are pitchers and the confidence interval using the SRS sample';
run;

*next use the stratified random sample;

PROC SURVEYMEANS DATA = stratBB TOTAL=797 MEAN CLM ;
   CLASS pos;
   WEIGHT SamplingWeight;
   STRATA team;
   VAR pos;
   TITLE 'Obtain the proportion (mean) of players who are pitchers and the confidence interval using the stratified sample';
run;


** the code below gets the population quantity for the percent of players who are pitchers;
PROC FREQ data=baseball ;
TABLE pos;
run;

/* Part (d): estimate the mean of logsal, which is the log of player salary  */

*first use the SRS;

PROC SURVEYMEANS DATA = sampBB TOTAL=797 MEAN CLM ;
   WEIGHT SamplingWeight;
   VAR salary logsal;
   TITLE 'Obtain the mean estimate and the confidence interval for the mean of logsal using the SRS sample';
run;

*next use the stratified random sample;

PROC SURVEYMEANS DATA = stratBB TOTAL=797 MEAN CLM ;
   WEIGHT SamplingWeight;
   STRATA team;
   VAR salary logsal;
   TITLE 'Obtain the mean estimate and the confidence interval for the mean of logsal using the stratified sample';
run;


** the code below gets the population quantity for the mean player salary;
PROC MEANS data=baseball ;
VAR salary logsal;
run;


/* Part (e): look at the variances in player salary by teams */

* Create boxplot in the BOXPLOT procedure;

proc boxplot data = baseball;
   plot logsal * team;
run;

*also look at the variances by team;

proc means data=baseball var;
by team;
var logsal;
run;
