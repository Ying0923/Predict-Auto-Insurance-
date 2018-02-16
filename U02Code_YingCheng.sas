
%let ME =  Ying.Cheng;
%let path=/sscc/home/y/...;
libname IHW  "&path";

data InsuranceData;
	SET IHW.logit_insurance;

proc contents ;
run;

data TEMPFILE;
set InsuranceData;
**drop INDEX;
**drop TARGET_AMT;
run;

proc print data= TEMPFILE (obs=7);
run;

* Overall View of Numeric variables;

PROC MEANS data=TEMPFILE MEAN MEDIAN MODE N NMISS MIN MAX STDDEV VARDEF=DF CLM maxdec=0;
VAR KIDSDRIV AGE HOMEKIDS YOJ INCOME  HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ  MVR_PTS CAR_AGE ;
run;

* Correlation for numeric variables;

proc corr data=TEMPFILE ;
	var KIDSDRIV AGE HOMEKIDS YOJ INCOME  HOME_VAL TRAVTIME BLUEBOOK TIF OLDCLAIM CLM_FREQ  MVR_PTS CAR_AGE ;
run;

* Overall View of categorical variables;

PROC FREQ data= TEMPFILE ORDER=freq;
TABLE TARGET_FLAG 
	CAR_TYPE
	CAR_USE
	EDUCATION
	JOB
	MSTATUS
	PARENT1
	RED_CAR
	REVOKED
	SEX
	URBANICITY/ MISSING;
RUN;

data Income_homeV;
set TEMPFILE;
keep INCOME  HOME_VAL ;
	if (INCOME <=0) then delete;
	if (INCOME="") then delete;
	if(HOME_VAL <=0)then delete;
	if (HOME_VAL ="") then delete;
	
PROC CORR DATA=Income_homeV PLOT=(matrix) plots(maxpoints=NONE);
	VAR INCOME  ;
	WITH HOME_VAL ;
	title 'Correlation-When Both Income and HOME_VAL >=0 ';
run;		
		
* The correlation we got above is 0.96
indicating we later will only keep one of the variable.
At the same time we can use Home_Val to fill in the missing INCOME values;
		
proc reg data = Income_homeV;
model INCOME  = HOME_VAL / vif ;
title 'SLR - INCOME  vs HOME_VAL ';
output out = fitted_model1 pred=yhat residual=residual ucl=ucl lcl=lcl
cookd=cook covratio=cov dffits=dfits press=prss;
run;

* Get an idea how different the INCOME distribution in different Job Title;

proc sort data = TEMPFILE ; by JOB;
proc means; var income; by JOB; run;


data Income_CarA;
set TEMPFILE;
keep INCOME  CAR_AGE;
	if(INCOME <=0) then delete;
	if(INCOME="") then delete;
	if(CAR_AGE<0)then delete;
	if(CAR_AGE="") then delete;
	
PROC CORR DATA=Income_CarA PLOT=(matrix) plots(maxpoints=NONE);
	VAR INCOME  ;
	WITH CAR_AGE ;
	title 'Correlation-When Both Income and CAR_AGE >=0 ';
run;	

data CleanedDataNUM;
set TEMPFILE;
if (CAR_AGE=-3) then delete;

IMP_AGE = AGE;
if missing( IMP_AGE ) then IMP_AGE = 45;

IMP_YOJ = YOJ;
if missing( IMP_YOJ ) then do;
	if INCOME = 0 then IMP_YOJ =0;
	else IMP_YOJ =11; end;

IMP_INCOME 	= INCOME;
if missing(IMP_INCOME) then do;
	if HOME_VAL >76648  THEN IMP_INCOME = -34863 + 0.45485*HOME_VAL ;
	else if JOB = "Clerical" THEN IMP_INCOME =33861;
	else if JOB ="Doctor" THEN IMP_INCOME = 128680;
	else if JOB = "Home Maker" THEN IMP_INCOME =12073;
	else if JOB ="Lawyer" THEN IMP_INCOME = 88305;
	else if JOB = "Manager" THEN IMP_INCOME =87462;
	else if JOB ="Professional" THEN IMP_INCOME = 76593;
	else if JOB ="Student" THEN IMP_INCOME = 6310;
	else if JOB = "z_Blue Collar" THEN IMP_INCOME =58957;
	else if JOB ="" THEN IMP_INCOME = 118853;
	else IMP_INCOME = 54028; end;

IMP_CAR_AGE = CAR_AGE;
if missing( IMP_CAR_AGE) then IMP_CAR_AGE= 8;
	
* Overall View of Numeric variables  in CleanedDataNUM ;

PROC MEANS data= CleanedDataNUM MEAN MEDIAN MODE N NMISS MIN MAX STDDEV VARDEF=DF CLM maxdec=0;
var AGE IMP_AGE   YOJ IMP_YOJ  INCOME IMP_INCOME CAR_AGE IMP_CAR_AGE ;
run;

** Analysis for Categorical Data;
** Overall View of categorical variables;

PROC FREQ data = CleanedDataNUM ORDER=freq;
TABLE TARGET_FLAG 
	CAR_TYPE
	CAR_USE
	EDUCATION
	JOB
	MSTATUS
	PARENT1
	RED_CAR
	REVOKED
	SEX
	URBANICITY/ MISSING;
RUN;

proc freq data = CleanedDataNUM ;
table KIDSDRIV * TARGET_FLAG /missing;
run;

proc freq data = CleanedDataNUM ;
table HOMEKIDS * TARGET_FLAG /missing;
run;

proc freq data = TEMPFILE;
table CLM_FREQ* TARGET_FLAG /missing;
run;

proc freq data = CleanedDataNUM;
table CAR_TYPE* TARGET_FLAG /missing;
run;

proc freq data = CleanedDataNUM;
table EDUCATION* TARGET_FLAG /missing;
run;

proc freq data = CleanedDataNUM;
table JOB* TARGET_FLAG /missing;
run;

proc freq data = CleanedDataNUM ;
table ( _character_ ) * TARGET_FLAG /missing;
run;

proc means data = CleanedDataNUM mean median;
class TARGET_FLAG;
var _numeric_;
run;

proc univariate data=CleanedDataNUM;
class TARGET_FLAG;
var IMP_INCOME;
histogram;
run;

proc univariate data=CleanedDataNUM;
class TARGET_FLAG;
var _numeric_;
histogram;
run;

** Final Cleaned Data;
data CleanedData;
	set CleanedDataNUM;
	log_TRAVTIME = log(TRAVTIME +1 );
	log_INCOME = log(IMP_INCOME +1 );
	log_BLUEBOOK = log(BLUEBOOK +1 );
	log_OLDCLAIM = log(OLDCLAIM +1 );
	log_CAR_AGE = log(IMP_CAR_AGE +1 );
	No_KIDSDRIV = 0;
	if KIDSDRIV = 0 then No_KIDSDRIV= 1;	
	No_HOMEKIDS = 0;
	if HOMEKIDS = 0 then No_HOMEKIDS = 1;	
	No_CLM_FREQ = 0;
	if CLM_FREQ = 0 then No_CLM_FREQ = 1;			
	No_Income = 0;
	if Income = 0 then No_Income = 1;	
	No_HOME = 0;
	if HOME_VAL = 0 then No_HOME = 1;		
	No_OLDCLAIM = 0;
	if OLDCLAIM = 0 then No_OLDCLAIM = 1;
	NewCar = 0;
	if IMP_CAR_AGE<4 then NewCar = 1;	
	if PARENT1  = 'Yes' then PARENT_Y = 1; else PARENT_Y = 0;
	if MSTATUS= 'Yes' then MSTATUS_Y = 1; else MSTATUS_Y = 0;
	if SEX= 'M' then SEX_M = 1; else SEX_M = 0;	
	if CAR_USE = 'Commercial' then CAR_USE_C = 1; else CAR_USE_C = 0;	
	if RED_CAR = 'yes' then RED_CAR_Y = 1; else RED_CAR_Y= 0;
	if REVOKED = 'Yes' then REVOKED_Y = 1; else REVOKED_Y = 0;	
	if URBANICITY= 'Highly Urban/ Urban' then URBANICITY_HU = 1; else URBANICITY_HU = 0;
	if CAR_TYPE='Minivan' then CAR_TYPE_MV=1; else CAR_TYPE_MV=0;
	if CAR_TYPE in ('Pickup','Sports Car') then CAR_TYPE_PS=1; else CAR_TYPE_PS=0;
	if EDUCATION in ('Masters', 'PhD') then HighEducation=1; else HighEducation=0;
	if EDUCATION in ('z_High School','<High School') then LowEducation=1; else LowEducation=0;	
	if JOB in ('Doctor', 'Lawyer','Manager') then JOB_WHITE_COLLAR=1; else JOB_WHITE_COLLAR=0;
	if JOB in ('Student','z_Blue Collar') then JOB_BLUE_STUDENT =1; else JOB_BLUE_STUDENT=0;	

* Overall View of the variables  in CleanedData ;
* numeric;
PROC MEANS data= CleanedData MEAN MEDIAN MODE N NMISS MIN MAX STDDEV VARDEF=DF CLM maxdec=0;
var AGE IMP_AGE   YOJ IMP_YOJ  INCOME IMP_INCOME CAR_AGE IMP_CAR_AGE log_TRAVTIME  log_INCOME 
	 log_BLUEBOOK log_OLDCLAIM  log_CAR_AGE  ;
run;

* catergorical;
PROC FREQ data = CleanedData ORDER=freq;
TABLE( 
	CAR_TYPE
	CAR_USE
	EDUCATION
	JOB
	MSTATUS
	PARENT1
	RED_CAR
	REVOKED
	SEX
	URBANICITY
	No_KIDSDRIV 
	No_HOMEKIDS 
	No_CLM_FREQ 
	No_Income 
	No_HOME 
	No_OLDCLAIM 
	NewCar 
	CAR_TYPE_MV
	CAR_TYPE_PS
	HighEducation
	LowEducation JOB_WHITE_COLLAR JOB_BLUE_STUDENT )* TARGET_FLAG /missing;

RUN;

** Build the Logistic Regression Model with the Clean Data;

** bench mark model;
proc logistic data = CleanedData ;
model TARGET_FLAG( ref="0" ) = 
				MVR_PTS
				CLM_FREQ  				 
				IMP_INCOME 
				OLDCLAIM  
				HOMEKIDS 
				KIDSDRIV
				BLUEBOOK 
				IMP_AGE
				IMP_CAR_AGE 
				TIF 
                                IMP_YOJ 
				TRAVTIME 					
				CAR_USE_C  
				MSTATUS_Y 
				PARENT_Y 
				RED_CAR_Y 
				REVOKED_Y 
				SEX_M 
				URBANICITY_HU 
				;
run;



proc logistic data = CleanedData ;
model TARGET_FLAG( ref="0" ) = 
				MVR_PTS
				No_CLM_FREQ  
				No_HOME 
				log_INCOME 
				No_Income 
				log_OLDCLAIM  
				No_OLDCLAIM 
				No_HOMEKIDS 
				No_KIDSDRIV
				log_BLUEBOOK 
				IMP_AGE
				log_CAR_AGE  
				NewCar
				TIF 
				IMP_YOJ 
				log_TRAVTIME 
				CAR_TYPE_MV        
				CAR_TYPE_PS
				CAR_USE_C 
				HighEducation     
                                LowEducation 
				JOB_WHITE_COLLAR         
				JOB_BLUE_STUDENT 
				MSTATUS_Y 
				PARENT_Y 
				RED_CAR_Y 
				REVOKED_Y 
				SEX_M 
				URBANICITY_HU /selection=forward;
run;

data cut_insurance;
set logit_insurance;

IMP_YOJ = YOJ;
if missing( IMP_YOJ ) then IMP_YOJ = 11;

YHAT = 	-1.2310 + 
	0.8886*( REVOKED in ("Yes") ) 	+
	0.3775*KIDSDRIV +
	0.2078*MVR_PTS	+
	-0.0374*IMP_YOJ
	;

YHAT = exp( YHAT );
PROB = YHAT / (1+YHAT);
run;

proc print data=cut_insurance(obs=10);
var PROB;
run;

proc means data=cut_insurance nmiss;
var PROB;
run;

** build a model to predict claim amount;
DATA PCAData ;
	SET CleanedData ;
	if (TARGET_AMT<0) then delete;
	
Proc Reg data=CleanedData outest= ADJRSQ_summary;
	Title 'adjusted R-Squared - Training data';
	model TARGET_AMT= 
			MVR_PTS 
			No_CLM_FREQ  
			No_HOME 
			log_INCOME
			No_Income 
			log_OLDCLAIM  
			No_HOMEKIDS 
			No_KIDSDRIV
			log_BLUEBOOK 
			IMP_AGE
			log_CAR_AGE 
			NewCar
			TIF 
			IMP_YOJ 
			log_TRAVTIME
			CAR_TYPE_MV        
			CAR_TYPE_PS
			CAR_USE_C 
			HighEducation  
			LowEducation 
			JOB_WHITE_COLLAR        
			JOB_BLUE_STUDENT
			MSTATUS_Y 
			PARENT_Y 
			RED_CAR_Y 
			REVOKED_Y 
			SEX_M 
			URBANICITY_HU /selection = FORWARD AIC VIF BIC MSE groupnames = 'EDUCATION' 'JOB' 'CAR_TYPE';
			
output out= ADJRSQ_out pred=yhat residual = resid ucl=ucl lcl=lcl cookd=cook
covratio=cov dffits=dfits press=prss;
run;
proc print data = ADJRSQ_summary; run;
proc print data=ADJRSQ_summary(obs=5); run;

*Scoring Code;
data SCOREFILE;
set IHW.logit_insurance_test;

IMP_AGE = AGE;
if missing( IMP_AGE ) then IMP_AGE = 45;
IMP_YOJ = YOJ;
if missing( IMP_YOJ ) then do;
	if INCOME = 0 then IMP_YOJ =0;
	else IMP_YOJ =11; end;

IMP_INCOME = INCOME;
if missing(IMP_INCOME) then do;
	if HOME_VAL >76648  THEN IMP_INCOME = -34863 + 0.45485*HOME_VAL ;
	else if JOB = "Clerical" THEN IMP_INCOME =33861;
	else if JOB ="Doctor" THEN IMP_INCOME = 128680;
	else if JOB = "Home Maker" THEN IMP_INCOME =12073;
	else if JOB ="Lawyer" THEN IMP_INCOME = 88305;
	else if JOB = "Manager" THEN IMP_INCOME =87462;
	else if JOB ="Professional" THEN IMP_INCOME = 76593;
	else if JOB ="Student" THEN IMP_INCOME = 6310;
	else if JOB = "z_Blue Collar" THEN IMP_INCOME =58957;
	else if JOB ="" THEN IMP_INCOME = 118853;
	else IMP_INCOME = 54028; end;
IMP_CAR_AGE = CAR_AGE;
if missing( IMP_CAR_AGE) then IMP_CAR_AGE= 8;
	
	log_TRAVTIME = log(TRAVTIME +1 );
	log_INCOME = log(IMP_INCOME +1 );
	log_BLUEBOOK = log(BLUEBOOK +1 );
	log_OLDCLAIM = log(OLDCLAIM +1 );
	log_CAR_AGE = log(IMP_CAR_AGE +1 );
		
	No_KIDSDRIV = 0;
	if KIDSDRIV = 0 then No_KIDSDRIV= 1;
	No_HOMEKIDS = 0;
	if HOMEKIDS = 0 then No_HOMEKIDS = 1;	
	No_CLM_FREQ = 0;
	if CLM_FREQ = 0 then No_CLM_FREQ = 1;			
	No_Income = 0;
	if Income = 0 then No_Income = 1;		
	No_HOME = 0;
	if HOME_VAL = 0 then No_HOME = 1;			
	No_OLDCLAIM = 0;
	if OLDCLAIM = 0 then No_OLDCLAIM = 1;
	NewCar = 0;
	if IMP_CAR_AGE<4 then NewCar = 1;		
	if PARENT1  = 'Yes' then PARENT_Y = 1; else PARENT_Y = 0;	
	if MSTATUS= 'Yes' then MSTATUS_Y = 1; else MSTATUS_Y = 0;	
	if SEX= 'M' then SEX_M = 1; else SEX_M = 0;	
	if CAR_USE = 'Commercial' then CAR_USE_C = 1; else CAR_USE_C = 0;	
	if RED_CAR = 'yes' then RED_CAR_Y = 1; else RED_CAR_Y= 0;	
	if REVOKED = 'Yes' then REVOKED_Y = 1; else REVOKED_Y = 0;	
	if URBANICITY= 'Highly Urban/ Urban' then URBANICITY_HU = 1; else URBANICITY_HU = 0;
	if CAR_TYPE='Minivan' then CAR_TYPE_MV=1; else CAR_TYPE_MV=0;
	if CAR_TYPE in ('Pickup','Sports Car') then CAR_TYPE_PS=1; else CAR_TYPE_PS=0;	
	if EDUCATION in ('Masters', 'PhD') then HighEducation=1; else HighEducation=0;
	if EDUCATION in ('z_High School','<High School') then LowEducation=1; else LowEducation=0;	
	if JOB in ('Doctor', 'Lawyer','Manager') then JOB_WHITE_COLLAR=1; else JOB_WHITE_COLLAR=0;
	if JOB in ('Student','z_Blue Collar') then JOB_BLUE_STUDENT =1; else JOB_BLUE_STUDENT=0;	
	
TEMP = 1.821353 + 		
		0.104026* 	MVR_PTS	+		
		-1.820780*	No_CLM_FREQ +		
		0.248516*	NO_HOME + 				 
		-0.054345*	log_INCOME +			
		-0.159289*	log_OLDCLAIM  +		
		-0.234877*	No_HOMEKIDS +		
		-0.572909*	No_KIDSDRIV +			
		-0.381609*	log_BLUEBOOK  +		
		-0.053123*	TIF  +				
		0.435091*	log_TRAVTIME 	+			
		-0.662794*	CAR_TYPE_MV +		
		0.589742*	CAR_USE_C +			
		0.547460*	LowEducation +		
		-0.480619*	JOB_WHITE_COLLAR +		
		-0.560750*	MSTATUS_Y  +			
		0.240783*	PARENT_Y +			
		0.870447*	REVOKED_Y +						
		2.299562*	URBANICITY_HU	;
			
TEMP = exp( TEMP );
TEMP = TEMP / (1.0+TEMP);
P_TARGET_FLAG = TEMP;
drop TEMP;

P_TARGET_AMT = -8013.70	+
		129.08*MVR_PTS+
		1413.24*log_BLUEBOOK+ 
		-682.88*REVOKED_Y+
		642.61*SEX_M;

keep index P_TARGET_FLAG P_TARGET_AMT;

proc print data=SCOREFILE;
run;

*Ensuring it saves in my folder.;
libname HW "/sscc/home/y/...";

data IHW.SCOREFILE;
	set SCOREFILE;	
run;
























