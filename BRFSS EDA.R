library(foreign)
BRFSS_a <-read.xport("LLCP2016.xpt")
dim(BRFSS_a)

#create a variable of the BRFSS variable list of interest using the BRFSS CODEBOOK2016
BRFSSVarList <- c("VETERAN3","DRNK3GE5","AVEDRNK2", "ALCDAY5","SLEPTIM1","ASTHMA3",
                  "X_AGE_G","SMOKE100","SMOKDAY2",	"SEX","X_HISPANC",	"X_MRACE1",
                  "MARITAL",	"GENHLTH",	"HLTHPLN1",	"EDUCA",	"INCOME2","X_BMI5CAT",
                  "EXERANY2")

#this subsets the BRFSS data that we need from the BRFSS_a into BRFSS_b 
#using the BRFSSVarlist 
BRFSS_b <- BRFSS_a[BRFSSVarList]
dim(BRFSS_b)

#subsets into BRFSS_c, variables where veteran responds as yes using 
#the data dictionary
BRFSS_c <- subset(BRFSS_b, VETERAN3==1) 
dim(BRFSS_c)

#using the data dictionary we check for veterans that are dont have a 
#valid answer to the alcohol question
BRFSS_d <- subset(BRFSS_c, ALCDAY5 < 777 | ALCDAY5==888)
dim(BRFSS_d)

# subsets the data that has valid sleep time using data dictionary
#Label:How Much Time Do You Sleep(SLEPTIM1)

BRFSS_e <- subset(BRFSS_d, SLEPTIM1 < 77)

# subsets the data that has valid asthma using data dictionary
#Label:(Ever told) you had asthma?(ASTHMA3)

BRFSS_f <- subset(BRFSS_e, ASTHMA3 < 7)

#copy of the dataset
BRFSS_g <- BRFSS_f

#using data dictionary,add the categorical variable set 
#to 9 to the dataset
BRFSS_g$ALCOHOL_GROUP <- 9

#creating new alcohol group using ALCDAYS5 variable

BRFSS_g$ALCOHOL_GROUP[BRFSS_g$ALCDAY5 <200 ] <- 3
BRFSS_g$ALCOHOL_GROUP[BRFSS_g$ALCDAY5 >=200 & BRFSS_g$ALCDAY5 <777] <- 2
BRFSS_g$ALCOHOL_GROUP[BRFSS_g$ALCDAY5 == 888] <- 1

#Adding flags to monthly and weekly alcohol intake

BRFSS_g$DRANK_MONTHLY <- 0
BRFSS_g$DRANK_MONTHLY[BRFSS_g$ALCOHOL_GROUP == 2] <- 1
table(BRFSS_g$ALCOHOL_GROUP, BRFSS_g$DRANK_MONTHLY)

BRFSS_g$DRANK_WEEKLY <- 0
BRFSS_g$DRANK_WEEKLY [BRFSS_g$ALCOHOL_GROUP == 1] <- 1
table(BRFSS_g$ALCOHOL_GROUP, BRFSS_g$DRANK_WEEKLY)

#creating new binge drinking group using DRINK3GE5 variable using the
#data dictionary
# Label:Binge Drinking(DRINK3GE5)
#1=binge drinking(yes), 0= binge drinking(no/no valid response)
BRFSS_g$BINGE_DRINKING <-0
BRFSS_g$BINGE_DRINKING[BRFSS_g$DRNK3GE5 <77] <- 1
table(BRFSS_g$VETERAN3,BRFSS_g$BINGE_DRINKING)


#Make a copy of the dataset
BRFSS_h <- BRFSS_g

#using data dictionary, make sleep variable

BRFSS_h$SLEPTIM2 <- NA
BRFSS_h$SLEPTIM2[!is.na(BRFSS_h$SLEPTIM1) & BRFSS_h$SLEPTIM1 !=77
                 & BRFSS_h$SLEPTIM1 !=99] <- BRFSS_h$SLEPTIM1
table(BRFSS_h$SLEPTIM1, BRFSS_h$SLEPTIM2)

#using data dictionary, make asthma variable
#1=Has Asthma, 0= No asthma
BRFSS_h$ASTHMA4 <- 9
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 1] <- 1
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 2] <- 0

table(BRFSS_h$ASTHMA3, BRFSS_h$ASTHMA4)

#Make a copy of the dataset
BRFSS_i <- BRFSS_h

#created new age variables based on age group.
#AGE2= 25-34yrs, AGE3=35-44yrs, AGE4=45-54yrs, AGE5=55-64yrs,AGE6= >65yrs
BRFSS_i$AGE2 <- 0
BRFSS_i$AGE3 <- 0
BRFSS_i$AGE4 <- 0
BRFSS_i$AGE5 <- 0
BRFSS_i$AGE6 <- 0

BRFSS_i$AGE2[BRFSS_i$X_AGE_G == 2] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE2)

BRFSS_i$AGE3[BRFSS_i$X_AGE_G == 3] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE3)

BRFSS_i$AGE4[BRFSS_i$X_AGE_G == 4] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE4)

BRFSS_i$AGE5[BRFSS_i$X_AGE_G == 5] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE5)

BRFSS_i$AGE6[BRFSS_i$X_AGE_G == 6] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE6)

#make smoking variables
#Label:Have you smoked at least 100 cigarettes in your entire life?(SMOKE100)
#Label:Do you now smoke cigarettes every day, some days, or not at all?(SMOKEDAY2)

#1=smoker 2=non-smoker, 9= Not reported
BRFSS_i$NEVER_SMOKED <- 0
BRFSS_i$NEVER_SMOKED [BRFSS_i$SMOKE100 == 2] <- 1

BRFSS_i$SMOKE_GROUP <- 9
BRFSS_i$SMOKE_GROUP[BRFSS_i$SMOKDAY2 == 1 | BRFSS_i$SMOKDAY2 == 2] <- 1
BRFSS_i$SMOKE_GROUP[BRFSS_i$SMOKDAY2 == 3 | BRFSS_i$NEVER_SMOKED == 1] <- 2

BRFSS_i$SMOKER <- 0
BRFSS_i$SMOKER[BRFSS_i$SMOKE_GROUP == 1] <- 1
table(BRFSS_i$SMOKER, BRFSS_i$SMOKE_GROUP)

#sex variable
#1=male 2=female 9=Not reported

BRFSS_i$MALE <- 0
BRFSS_i$MALE[BRFSS_i$SEX == 1] <- 1
table(BRFSS_i$SEX,BRFSS_i$MALE)

#Hispanic variable
#Label:1=Hispanic, 0=Not Hispanic 9= Not reported

BRFSS_i$HISPANIC <- 0
BRFSS_i$HISPANIC[BRFSS_i$X_HISPANC == 1] <- 1
table(BRFSS_i$HISPANIC, BRFSS_i$X_HISPANC)

#race variables
BRFSS_i$RACE_GROUP <- 9
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 1] <- 1
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 2] <- 2
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 3] <- 3
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 4] <- 4
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 5] <- 5
BRFSS_i$RACE_GROUP[BRFSS_i$X_MRACE1 == 6 | BRFSS_i$X_MRACE1 == 7] <- 6

table(BRFSS_i$RACE_GROUP , BRFSS_i$X_MRACE1)

BRFSS_i$BLACK <- 0
BRFSS_i$ASIAN <- 0
BRFSS_i$OTHRACE <- 0

BRFSS_i$BLACK[BRFSS_i$RACE_GROUP == 2] <- 1
table(BRFSS_i$RACE_GROUP, BRFSS_i$BLACK)

BRFSS_i$ASIAN[BRFSS_i$RACE_GROUP == 4] <- 1
table(BRFSS_i$RACE_GROUP, BRFSS_i$ASIAN)

BRFSS_i$OTHRACE[BRFSS_i$RACE_GROUP == 3 | BRFSS_i$RACE_GROUP == 5 | BRFSS_i$RACE_GROUP == 6 | BRFSS_i$RACE_GROUP == 7] <- 1
table(BRFSS_i$RACE_GROUP, BRFSS_i$OTHRACE)

#marital variables

BRFSS_i$MARITAL_STATUS <- 9
BRFSS_i$MARITAL_STATUS[BRFSS_i$MARITAL == 1 | BRFSS_i$MARITAL == 5] <- 1
BRFSS_i$MARITAL_STATUS[BRFSS_i$MARITAL == 2 | BRFSS_i$MARITAL == 3 ] <- 2
BRFSS_i$MARITAL_STATUS[BRFSS_i$MARITAL == 4] <- 3

table(BRFSS_i$MARITAL_STATUS, BRFSS_i$MARITAL)

BRFSS_i$NEVER_MARRIED <- 0
BRFSS_i$FORMERLY_MARRIED <- 0

BRFSS_i$NEVER_MARRIED[BRFSS_i$MARITAL_STATUS == 3] <- 1
table(BRFSS_i$MARITAL_STATUS, BRFSS_i$NEVER_MARRIED)

BRFSS_i$FORMERLY_MARRIED[BRFSS_i$MARITAL_STATUS == 2] <- 1
table(BRFSS_i$MARITAL_STATUS, BRFSS_i$FORMERLY_MARRIED)

#Genhealth variables
#Label:Would you say that in general your health is--- 

BRFSS_i$GENHLTH2 <- 9
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 1] <- 1
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 2] <- 2
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 3] <- 3
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 4] <- 4
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 5] <- 5

table(BRFSS_i$GENHLTH2, BRFSS_i$GENHLTH)

BRFSS_i$FAIRHLTH <- 0
BRFSS_i$POORHLTH <- 0

BRFSS_i$FAIRHLTH [BRFSS_i$GENHLTH2 == 4] <- 1
table(BRFSS_i$FAIRHLTH, BRFSS_i$GENHLTH2)

BRFSS_i$POORHLTH [BRFSS_i$GENHLTH2 == 5] <- 1
table(BRFSS_i$POORHLTH, BRFSS_i$GENHLTH2)

#Health plan variables
#Label:Do you have any kind of health care coverage, including health insurance,
#prepaid plans such as HMOs, or government plans such as Medicare, or Indian 
#Health Service?
BRFSS_i$HLTHPLN2 <- 9
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 1] <- 1
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 2] <- 2

table(BRFSS_i$HLTHPLN1, BRFSS_i$HLTHPLN2)

BRFSS_i$NOPLAN <- 0
BRFSS_i$NOPLAN [BRFSS_i$HLTHPLN2== 2] <- 1
table(BRFSS_i$NOPLAN, BRFSS_i$HLTHPLN2)

#Education variables
#Label:What is the highest grade or year of school you completed?

BRFSS_i$EDUCATION_GROUP <- 9
BRFSS_i$EDUCATION_GROUP[BRFSS_i$EDUCA == 1 | BRFSS_i$EDUCA == 2 | BRFSS_i$EDUCA == 3] <- 1
BRFSS_i$EDUCATION_GROUP[BRFSS_i$EDUCA == 4] <- 2
BRFSS_i$EDUCATION_GROUP[BRFSS_i$EDUCA == 5] <- 3
BRFSS_i$EDUCATION_GROUP[BRFSS_i$EDUCA == 6] <- 4

table(BRFSS_i$EDUCATION_GROUP, BRFSS_i$EDUCA)

BRFSS_i$LOW_EDUCATION <- 0
BRFSS_i$COLLEGE <- 0

BRFSS_i$LOW_EDUCATION[BRFSS_i$EDUCATION_GROUP == 1 | BRFSS_i$EDUCATION_GROUP == 2 ] <- 1
table(BRFSS_i$LOW_EDUCATION, BRFSS_i$EDUCATION_GROUP)

BRFSS_i$COLLEGE [BRFSS_i$EDUCATION_GROUP == 3] <- 1
table(BRFSS_i$COLLEGE, BRFSS_i$EDUCATION_GROUP)

#Income variables
#Label:Is your annual household income from all sources: 

BRFSS_i$INCOME3 <- BRFSS_i$INCOME2
BRFSS_i$INCOME3[BRFSS_i$INCOME2 >=77] <- 9
table(BRFSS_i$INCOME2, BRFSS_i$INCOME3)

BRFSS_i$INC1 <- 0
BRFSS_i$INC2 <- 0
BRFSS_i$INC3 <- 0
BRFSS_i$INC4 <- 0
BRFSS_i$INC5 <- 0
BRFSS_i$INC6 <- 0
BRFSS_i$INC7 <- 0

BRFSS_i$INC1[BRFSS_i$INCOME3 == 1] <- 1
table(BRFSS_i$INC1, BRFSS_i$INCOME3)

BRFSS_i$INC2[BRFSS_i$INCOME3 == 2] <- 1
table(BRFSS_i$INC2, BRFSS_i$INCOME3)

BRFSS_i$INC3[BRFSS_i$INCOME3 == 3] <- 1
table(BRFSS_i$INC3, BRFSS_i$INCOME3)

BRFSS_i$INC4[BRFSS_i$INCOME3 == 4] <- 1
table(BRFSS_i$INC4, BRFSS_i$INCOME3)

BRFSS_i$INC5[BRFSS_i$INCOME3 == 5] <- 1
table(BRFSS_i$INC5, BRFSS_i$INCOME3)

BRFSS_i$INC6[BRFSS_i$INCOME3 == 6] <- 1
table(BRFSS_i$INC6, BRFSS_i$INCOME3)

BRFSS_i$INC7[BRFSS_i$INCOME3 == 7] <- 1
table(BRFSS_i$INC7, BRFSS_i$INCOME3)

#BMI variables

BRFSS_i$BMICAT<- 9
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==1] <- 1
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==2] <- 2
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==3] <- 3

BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==4] <- 4

table(BRFSS_i$BMICAT, BRFSS_i$X_BMI5CAT)

BRFSS_i$UNDERWEIGHT <- 0  
BRFSS_i$OVERWEIGHT <- 0   
BRFSS_i$OBESE <- 0  

BRFSS_i$UNDERWEIGHT[BRFSS_i$BMICAT== 1] <- 1
table(BRFSS_i$UNDERWEIGHT, BRFSS_i$BMICAT)

BRFSS_i$OVERWEIGHT[BRFSS_i$BMICAT== 3] <- 1
table(BRFSS_i$OVERWEIGHT, BRFSS_i$BMICAT)

BRFSS_i$OBESE[BRFSS_i$BMICAT== 4] <- 1
table(BRFSS_i$OBESE, BRFSS_i$BMICAT)

#Exercise variables
#Label:During the past month, other than your regular job,
#did you participate in any physical activities or exercises such 
#as running, calisthenics, golf, gardening, or walking for exercise?(EXERANY2)

BRFSS_i$EXERANY3<- 9
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==1] <- 1
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==2] <- 2

table(BRFSS_i$EXERANY3, BRFSS_i$EXERANY2)

BRFSS_i$NOEXER <- 0    #no exercise
BRFSS_i$NOEXER[BRFSS_i$EXERANY3 ==2] <- 1
table(BRFSS_i$NOEXER, BRFSS_i$EXERANY3)

dim(BRFSS_i)
write.csv(BRFSS_i, file = "analytic.csv")

#This script makes helps to process the raw BRFSS data and creates new 
#variables using the
#data dictionary and the output is saved as analytic.csv