libname final base "/home/u62387331/STAT506/final project";
%let outpath=/home/u62387331/STAT506/final project/output;


/* Accessing Data (PG1.02) */
/* 1 */
/* 1a */
proc import datafile='/home/u62387331/STAT506/final project/country_wise_latest.csv'
	dbms=csv
	out=final.country_wise_latest;
run;

proc import datafile='/home/u62387331/STAT506/final project/covid_19_clean_complete.csv'
	dbms=csv
	out=final.covid_19_complete;
run;
/* 1b */
proc contents varnum data=final.country_wise_latest; run;
proc contents varnum data=final.covid_19_complete; run;


/* Exploring and Validating Data (PG1.03) */
/* 3  */
proc print data=final.covid_19_complete(obs=10); /* 3a, 3c*/
	var Country_Region Lat Long WHO_Region; /* 3b */
	where WHO_Region IN ("Africa","Europe"); /* 3d */
	format Lat COMMA12.1; /* 3e */
	format Long COMMA12.1; /* 3e */
run;

/* Preparing Data (PG1.04) */
/* 5 */
proc print data=final.country_wise_latest(obs=10); run;
data final.topic5;
	set final.country_wise_latest; /* 5a */
	where WHO_Region IN ("Africa","Europe"); /* 5b */
	drop Recovered; /* 5c */
	format Recovered_100_Cases 12.4; /* 5d */
	length Province_State $30.; /* 5e */
	Deaths_rate=Deaths/Confirmed; /* 5f */
	if Deaths_rate<=0.05 then low_death_rate=1; /* 5f */
run;
proc print data=final.topic5(obs=10); run;

/* 6 */
data final.topic6;
	set final.country_wise_latest;
	if Active > 10000 then high_Active_num=1; /* 6a */
	Deaths_rate=Deaths/Confirmed; /* 6b */
	if Deaths_rate <= 0.05 then low_death_rate=1; /* 6b */
	else if Deaths_rate > 0.05 then low_death_rate=0; /* 6b */
	if WHO_Region="Americas" then do; /* 6c */
		Region_America="Yes"; /* 6c */
	end;
run;
proc print data=final.topic6; run;


/* Analyzing and Reporting on Data (PG1.05) */
/* 8 */
data final.topic8; /* data modification */
	set final.country_wise_latest;
	if WHO_Region="Americas" then Group=1;
	else if WHO_Region="South-East Asia" then Group=2;
	else if WHO_Region="Africa" then Group=3;
	else if WHO_Region="Europe" then Group=4;
	else if WHO_Region="Eastern Mediterranean" then Group=5;
	else Group=6;
	length Rates $11;
	if one_week_increase_rate>=50 then Rates="High rate";
	else if one_week_increase_rate<50 and one_week_increase_rate>=10 then Rates="Medium rate";
	else if one_week_increase_rate<10 then Rates="Low rate";
run;
/* 8a */
proc freq data=final.topic8 order=freq;
	tables Group;
run;
/* 8b */
proc freq data=final.topic8;
	tables rates*Group / norow nocol;
run;

/* Controlling DATA Step Processing (PG2.01) */
/* 14 */
data Americas(drop=Rates) Europe(drop=Rates) other(drop=Rates); /* 14b */
	set final.topic8;
	select(WHO_Region);
		when ("Americas") do;
			output Americas; /* 14a(2) */
			putlog Country_Region=; /* 14c(2) */
		end;
		when ("Europe") do;
			output Europe; /* 14a(2) */
			putlog Country_Region=; /* 14c(2) */
		end;
		otherwise output other; /* 14a(1), (2) */
		end;
run;

proc print data=Americas; run;
proc print data=Europe; run;
proc print data=other; run;


/* Summarizing Data (PG2.02) */
/* 15 */
proc sort data=final.topic8 out=final.sort_topic8; /* 15d */
	by WHO_Region;
run;
proc print data=final.sort_topic8; run;

data final.topic15(keep=WHO_Region Sumdeath);
	set final.sort_topic8;
	by WHO_Region; /* 15c */
	if first.WHO_Region=1 then Sumdeath=0;
	retain Sumdeath 0; /* 15a */
 	Sumdeath=sum(Sumdeath, Deaths); /* 15b */
 	if last.WHO_Region=1; /* 15c */
 	output;
run;

proc print data=final.topic15; run;

/* Manipulating Data with Functions (PG2.03) */
/* 20 */
data final.topic20;
	set final.country_wise_latest;
	round_Recovered_100_Cases = round(Recovered_100_Cases, 0.1); /* 20a */
	ceil_Recovered_100_Cases = ceil(Recovered_100_Cases); /* 20a */
	floor_Recovered_100_Cases = floor(Recovered_100_Cases); /* 20a */
	int_Recovered_100_Cases = int(Recovered_100_Cases); /* 20a */
	largest_num = largest(1, Deaths_100_cases, Recovered_100_cases, Deaths_100_recovered); /* 20b */
	max_num = max(Deaths_100_cases, Recovered_100_cases, Deaths_100_recovered); /* 20b */
	sum_num = sum(Deaths_100_cases, Recovered_100_cases, Deaths_100_recovered); /* 20b */
	Num_Non_Missing = N(Deaths_100_cases, Recovered_100_cases, Deaths_100_recovered); /* 20c */
run;
proc print data=final.topic20; run;

/* Combining Tables (PG2.05) */
/* 24 */
proc sort data=final.country_wise_latest out=final.country_wise_latest;
	by Country_Region;
run;
proc sort data=final.covid_19_complete out=final.covid_19_singlevalue  /* data modification */
	NODUPKEY dupout=final.trash;
	by Country_Region;
run;

data final.topic24; /* 24b */
	merge final.covid_19_singlevalue(
		RENAME=(Lat=Latitude Long=Longitude)
		drop=Province_State Date Confirmed Deaths Recovered Active)
		final.country_wise_latest; /* 24a */
	by Country_Region;
run;
proc print data=final.topic24; run;


/* 25 */
data final.covid_19_missing_Americas; /* data modification */
    set final.covid_19_complete;
    if WHO_Region ne 'Americas' then output;
run;
proc sort data=final.covid_19_missing_Americas out=final.covid_19_missing_Americas;
	by Country_Region;
run;

data final.matches final.non_matches;
	merge final.covid_19_missing_Americas(IN=a) final.country_wise_latest(IN=b DROP=Confirmed Deaths Recovered Active); /* 25a-(1) */
	by Country_Region;
	if a=0 and b=1 then output final.non_matches; /* 25a-(2) */
	else if a=1 and b=0 then output final.matches; /* 25a-(2) */
	else if a=1 and b=1 then output final.matches; /* 25a-(2) */
run;
	
data final.topic25(DROP=Province_State); /* 25b-(2) */
	merge
		final.covid_19_missing_Americas(RENAME=(Country_Region=Country))
		final.country_wise_latest(RENAME=(Country_Region=Country) DROP=Confirmed Deaths Recovered Active); /* 25b-(1), b-(2) */
	by Country;
run;

proc print data=final.matches(obs=10); run;
proc print data=final.non_matches(obs=10); run;
proc print data=final.topic25(obs=10); run;

































