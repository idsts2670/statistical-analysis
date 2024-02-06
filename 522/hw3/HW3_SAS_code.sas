/* This program provides SAS code to complete Homework 3, Texbook question 2.13, parts a) through e) */

/* first provide the replace my path with the path to your folder where you put the shapespop data */

LIBNAME datalib 'C:\Users\slchrist\OneDrive - purdue.edu\Purdue\Teaching\Sampling 2024\Homework\HW3';

/* we can copy the data to the work folder */

data shapespop;
   set datalib.shapespop;
run;

/* you can look at the data using the table editor. Click on Tools, then Table Editor. Next click on the 
   open file icon, choose the "Work" folder, then click on the data file called shapespop */



/* Part (a): Select an SRS of size 200. Be sure to enter a seed number */
proc surveyselect data=shapespop method = srs sampsize = 200 out = samp stats seed = ;
   title 'Draw SRS of size 200';
run;

*you can permanently save your sample in your datalib folder with the following data step;
data datalib.samp;
   set samp;
run;

*the next step, proc contents, lets you see the contents of the data set, incluing variables ;
PROC CONTENTS data=samp;
run;

/* Part (b): Draw a histogram of the areas for the objects in the sample */

PROC SURVEYMEANS DATA = samp TOTAL=20000 ALL PLOTS (NBINS=40) = histogram;
   WEIGHT SamplingWeight;
   VAR area;
   TITLE 'Analyze object area from samp; specify 40 bins for the histogram';
run;


/* Part (c): Estimate and mean and total (SAS calls this a sum) and the 95% confidence intervals for both */

PROC SURVEYMEANS DATA = samp TOTAL=20000 MEAN CLM SUM CLSUM;
   WEIGHT SamplingWeight;
   VAR area;
   TITLE 'Obtain the mean and total estimates and their confidence intervals for area of the objects';
run;


/* Part (d): Get the total (SUM) estimate of the number of gray objects. Also get the 95% confidence interval for the total. */

PROC SURVEYMEANS DATA = samp TOTAL=20000 SUM CLSUM;
   CLASS color;
   WEIGHT SamplingWeight;
   VAR color;
   TITLE 'Obtain the total estimate of the number of gray objects';
run;


/* Part (d): Get the total (SUM) estimate of the number of circles. Also get the 95% confidence interval for the total. */

PROC SURVEYMEANS DATA = samp TOTAL=20000 SUM CLSUM;
   CLASS shape;
   WEIGHT SamplingWeight;
   VAR shape;
   TITLE 'Obtain the total estimate of the number of circles';
run;
