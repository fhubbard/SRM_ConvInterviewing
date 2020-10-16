/***************************************************************************************
PROGRAM:	Final_Analyses.sas
			
AUTHOR:  	Frost Hubbard	frosthubbard@westat.com

PURPOSE:	To analyze the data at the question level and the case level. The case level
			analyses will be for the re-administration section timings. All other analyses
			are at the case level. None of the analyses regarding interviewer demographics
			are provided here. If you are interested in these analyses, please email 
			frosthubbard@westat.com

INPUT:		PUF_question_level.sas7bdat and readmin_sect_timings.sas7bdat (a case-level file)

OUTPUT:		

NOTES:		10/15/2020 - Delivered to SRM Editors
***************************************************************************************/

options nodate FORMDLIM='_';
ods graphics on /reset=all;
goptions reset=all fontres=presentation ftext=swissb htext=1.5;

/* To produce an rtf file of the output, if so desired:*/
ods rtf startpage=never style=journal file = "\\westat.com\DFS\DVSTAT\Individual Directories\Hubbard\02_Journals\Manuscripts\PONS\04_Reports\Final_Analyses_Output_&SYSDATE..rtf";

libname conv "\\westat.com\DFS\DVSTAT\Individual Directories\Hubbard\02_Journals\Manuscripts\PONS\02_Data";
footnote "Program:	Final_Analyses.sas			Created by: F.Hubbard";

/**************************************************************************************************************************
Question Level Analyses using PUF_question_level.sas7bdat
****************************************************************************************************************************/

title "Analysis #1: Mean % Response Change by Interviewing Technique";
proc means data=conv.PUF_question_level n mean stderr lclm uclm;
	class iw_tech /*question_type*/;
	var resp_change;
	output out=mod1_means mean= n=count;
run;
title;

/*Running models with rand eff of respondent*/
proc sort data=conv.PUF_question_level;
	by resp_change descending question_type iw_tech;
run;

title "Analysis #2a: Logistic Regression with interaction: DV - Resp_Change (dichotomous); IV: Iw_Tech, Question_Type, and Iw_Tech*Question_Type";
proc logistic data=conv.PUF_question_level descending;
	class question_type (ref='Factual') iw_tech (ref = 'Stand')/param=glm;
	model resp_change = iw_tech question_type iw_tech*question_type;
	oddsratio question_type / at(iw_tech='Conv');
	slice iw_tech*question_type / sliceby(iw_tech='Conv') diff oddsratio;

	oddsratio question_type / at(iw_tech='Stand');
	slice iw_tech*question_type / sliceby(iw_tech='Stand') diff oddsratio;

	oddsratio iw_tech / at(question_type='Factual');
	slice iw_tech*question_type / sliceby(question_type='Factual') diff oddsratio;

	oddsratio iw_tech / at(iw_tech='Opinion');
	slice iw_tech*question_type / sliceby(question_type='Opinion') diff oddsratio;
run;
quit;

title "Analysis #2b (Table 1): Logistic Regression w/o interaction: DV - Resp Change (dichotomous); IV: Iw Tech and Question Type";
proc logistic data=conv.PUF_question_level descending;
	class question_type (ref='Factual') iw_tech (ref = 'Stand');
	model resp_change = iw_tech question_type;
run;
quit;
title;

title "Analysis #2c: Logistic Regression w/ interaction, excluding the How Risky is Drowsy Driving Question";
title2 "DV - Resp Change (dichotomous); IV: Iw Tech and Question Type";
proc logistic data=conv.PUF_question_level descending;
	where question ne 'How Risky is DD?';
	class question_type (ref='Factual') iw_tech (ref = 'Stand');
	model resp_change = iw_tech question_type iw_tech*question_type;
run;
quit;
title;

title "Analysis #2d: Logistic Regression w/o interaction, excluding the How Risky is Drowsy Driving Question";
title2 "DV - Resp Change (dichotomous); IV: Iw Tech and Question Type";
proc logistic data=conv.PUF_question_level descending;
	where question ne 'How Risky is DD?';
	class question_type (ref='Factual') iw_tech (ref = 'Stand');
	model resp_change = iw_tech question_type;
run;
quit;
title;

title "Analysis #2e: Logistic Regression w/o interaction: DV - Resp Change (dichotomous); IV: Iw Tech, Question Type, Education Level, Age, Gender";
proc logistic data=conv.PUF_question_level descending;
	class 	question_type (ref='Factual') iw_tech (ref = 'Stand') educ_2cat (ref = 'Coll or More') 
			age_4cat (ref = '18-44') sex_r (ref = 'Female');
	model resp_change = iw_tech question_type educ_2cat age_4cat sex_r;
run;
quit;
title;

/*Appendix B Analyses*/
%macro log_split (var=, val=);
title "Appendix B: Logistic Regression w/ interaction: Only &val cases";
proc logistic data=conv.PUF_question_level descending;
	where &var = "&val";
	class question_type (ref='Factual') iw_tech (ref = 'Stand');
	model resp_change = iw_tech question_type iw_tech*question_type;
run;
quit;
title;
%mend;

%log_split (var=educ_2cat, val = Coll or More);
%log_split (var=educ_2cat, val = Some Coll or Less);

%log_split (var=age_2cat, val = 18-59);
%log_split (var=age_2cat, val = 60+);

%log_split (var=sex_r, val = Female);
%log_split (var=sex_r, val = Male);

%macro logist_main_effects_only (var=, val=);
title "Appendix B: Logistic Regression w/o interaction: Only &val cases";
proc logistic data=conv.PUF_question_level descending;
	where &var = "&val";
	class question_type (ref='Factual') iw_tech (ref = 'Stand');
	model resp_change = iw_tech question_type/;
run;
quit;
title;
%mend;

%logist_main_effects_only (var=educ_2cat, val = Coll or More);
%logist_main_effects_only (var=educ_2cat, val = Some Coll or Less);

%logist_main_effects_only (var=age_2cat, val = 18-59);
%logist_main_effects_only (var=age_2cat, val = 60+);

%logist_main_effects_only (var=sex_r, val = Female);
%logist_main_effects_only (var=sex_r, val = Male);


title "Analysis #4: % Response Change by Interviewing Technique for Factual Questions";
proc freq data=conv.PUF_question_level;
	where question_type = 'Factual';
	table resp_change*iw_tech/chisq;
run;
title;

title "Analysis #5: % Response Change by Interviewing Technique for Opinion Questions";
proc freq data=conv.PUF_question_level;
	where question_type = 'Opinion';
	table resp_change*iw_tech/chisq;
run;
title;

title "Analysis #6a: Mean % Response Change by Iw Tech and NV Sensitivity Group";
proc means data=conv.PUF_question_level mean stderr lclm uclm maxdec=3;
	where group ne '';
	class group;
	var resp_change;
run;
title;

title "Analysis #6b: Logistic Regression: DV - Response Change; IV - Iw Tech and NV Sensistivity Group (Low NV Sensitivity - Conv Iw, High NV Sensitivity - Conv Iw; Stand Iw)";
title "Reference group: Low NV Sensitivity - Conv Iw";
proc logistic data=conv.PUF_question_level descending;
	class group (ref = "Lo - CI" param=ref);
	model resp_change = group;
run;
title;

title "Analysis #6c: Logistic Regression: DV - Response Change; IV - Iw Tech and NV Sensistivity Group (Low NV Sensitivity - Conv Iw, High NV Sensitivity - Conv Iw; Stand Iw)";
title "Reference group: Standardized Iw";
proc logistic data=conv.PUF_question_level descending;
	class group (ref = "Stand" param=ref);
	model resp_change = group;
run;
title;

title "Analysis 7 (Table 2): Type of Definition Provided in Conversational Interviews by level of NV Sensitivity";
proc freq data=conv.PUF_question_level;
	where iw_tech = 'Conv';
	table aud*(def_given preemp_given respdef_given)/chisq;
run;
title;

title "Analysis 8: % Response Change Provided in Conversational interviews by Low NV Sensitivity Interviewers by Type of Definition Given (Preemptive Strike vs Responsive Definition)"; 
proc freq data=conv.PUF_question_level;
	where iw_tech = 'Conv' and aud = 'Low' and def_type ne 'No Def Given';
	table resp_change*def_type/chisq;
run;
title;

/***********************************************************************************************************************
Section Timings Analyses using readmin_sect_timings.sas7bdat
**********************************************************************************************************************/

/*Note: 1. The Section F (Readministration Section timing variable; varname timef) also did not meet the equality of variances assumption
		required for a parametric t-test. However, a Satterwaite adjustment would have allowed us to use a t-test if that
		was the only assumption not met. Unfortunately, as the PROC UNIVARIATE output below demonstrates, the timef data
		were also not normally distributed. Thus, we used a Wilcoxson Mann-Whitney test.

		2. The Re-administration section timings excluded 41 of 490 cases that either
		   	a. indicated that they did not currently drive and thus did not receive the 4 re-administration section questions about drowsy driving (n=39) 
				OR
			b. completed fewer than 3 questions in the re-administration section (n=2).
*/


/*Demonstrating the non-normality of the re-administration section timing variable (timef)*/
title "Non-normality of Re-administration Section timing variable";
proc univariate data=conv.readmin_sect_timings normal;
	var timef;
run;
title;

title "Non-normality of Re-administration Section Timing Variable by Interviewing Technique";
proc univariate data=conv.readmin_sect_timings normal;
	class iw_tech;
	var timef;
	histogram timef;
run;
title;

title "Mean Re-adminstration Section Timing (in Minutes) by Interviewing Technique";
proc means data=conv.readmin_sect_timings;
	class iw_tech;
	var timef;
run;
title;

/*Demonstrating the lack of homogeneity of variances*/
title "Check equality of variances for timef";
proc glm data=conv.readmin_sect_timings;
	where duration_group ne '';
	class duration_group;
	model timef = duration_group;
	means duration_group / hovtest;
run;
quit;
title;

title "Analysis #3: Statistical Test of Mean Re-administration Section Timing (in Minutes) by Interviewing Technique";
title2 "Wilcoxson Mann-Whitney Test nonparametric test because groups do not have equal variances and data not normally distributed";
proc npar1way data = conv.Readmin_sect_timings wilcoxon;
	class iw_tech;
  	var timef;
run;
title;

title "Non-normality of Re-administration Section Timing Variable by Following Groups: Conv-High NV Sensitivity, Conv-Low NV Sensitivity, Standardized";
proc univariate data=conv.Readmin_sect_timings normal;
	where duration_group ne '';
	class duration_group;
	var timef;
	histogram timef;
run;
title;

title "Analysis #6: Mean Duration in Seconds of Re-administration Section by Nonverbal Sensitivity Group and Interviewing Technique";
proc means data = conv.Readmin_sect_timings n mean lclm uclm stderr min max maxdec=2;
	where duration_group ne '';
	class duration_group;
  	var timef;
run;
title;

title "Statistical Test of Mean Duration in Seconds of Re-administration Section - Conv-High NV Sensitivity vs Conv-Low NR Sensitivity";
title2 "Wilcoxson Mann-Whitney Test nonparametric test because gropus do not have equal variances nor are they normally distributed";
proc npar1way data = conv.Readmin_sect_timings wilcoxon;
	where duration_group ne 'Stand';
	class duration_group;
  	var timef;
run;
title;

title "Footnote 20 Analysis: Statistical Test of Mean Duration in Seconds of Re-administration Section - Standaridized vs Conv-Low NR Sensitivity";
title2 "Wilcoxson Mann-Whitney Test nonparametric test because gropus do not have equal variances";
proc npar1way data = conv.Readmin_sect_timings wilcoxon;
	where duration_group ne 'High - CI';
	class duration_group;
  	var timef;
run;
title;

title "Footnote 20 Analysis: Statistical Test of Mean Duration in Seconds of Re-administration Section - Standaridized vs Conv-High NR Sensitivity";
title2 "Wilcoxson Mann-Whitney Test nonparametric test because gropus do not have equal variances";
proc npar1way data = conv.Readmin_sect_timings wilcoxon;
	where duration_group ne 'Lo - CI';
	class duration_group;
  	var timef;
run;
title;
ods rtf close;
