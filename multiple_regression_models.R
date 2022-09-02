#read in analytic table

analytic <- read.csv("analytic.csv", header=TRUE, sep=",")
#make Model 1

Model1 = lm(SLEPTIM2 ~ DRANK_WEEKLY+DRANK_MONTHLY+ BINGE_DRINKING, data=analytic) 
summary(Model1)

library (broom)

InitialModel <- tidy(Model1)
write.csv(FinalModel, file = "Initial Linear Regression Model.csv")

ConfidenceLevel<-confint(Model1, level = 0.95)
write.csv(ConfidenceLevel,file = "Initial Model Confidence Level.csv")



#model2

Model2 = lm(SLEPTIM2 ~ DRANK_MONTHLY + DRANK_WEEKLY + MALE + AGE2 + AGE3 + AGE4 + AGE5 + AGE6, data=analytic)

summary(Model2)

Model3 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6, data=analytic) 
summary(Model3) 

#plus smoker

Model4 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER, data=analytic) 
summary(Model4) 

#plus Hispanic

Model5 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC, data=analytic) 
summary(Model5) 

#plus race

Model6 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE, data=analytic) 
summary(Model6) 

#plus marital status

Model7 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED, data=analytic) 
summary(Model7)


#plus Gen health

Model8 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH, data=analytic) 
summary(Model8)

#plus health plan

Model9 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + NOPLAN, data=analytic) 
summary(Model9)



Model10 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + LOW_EDUCATION + COLLEGE, data=analytic) 
summary(Model10)


Model11 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE, data=analytic) 
summary(Model11)

#add income

Model12 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC1 + INC2 + INC3 + INC4 + INC5 + INC6 + INC7, data=analytic) 
summary(Model12)

#remove insignificant income variables

Model13 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7, data=analytic) 
summary(Model13)

#plus BMI

Model14 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + UNDERWEIGHT + OVERWEIGHT + OBESE, data=analytic) 
summary(Model14)


Model15 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE, data=analytic) 
summary(Model15)

#plus exercise

Model16 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + NOEXER, data=analytic) 
summary(Model16)




Model17 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + MALE, data=analytic) 
summary(Model17)


Model18 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE2 + AGE3, data=analytic) 
summary(Model18)


Model19 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3, data=analytic) 
summary(Model19)


Model20 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + UNDERWEIGHT, data=analytic) 
summary(Model20)


Model21 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION, data=analytic) 
summary(Model21)



Model22 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + NOEXER, data=analytic) 
summary(Model22)


Model23 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + NOPLAN, data=analytic) 
summary(Model23)


Model24 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + AGE2, data=analytic) 
summary(Model24)


Model25 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + INC1, data=analytic) 
summary(Model25)


Model26 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + INC3, data=analytic) 
summary(Model26)


Model27 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + INC4, data=analytic) 
summary(Model27)


Model28 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + INC5, data=analytic) 
summary(Model28)


Model29 = lm(SLEPTIM2 ~ DRANK_WEEKLY + AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + INC6, data=analytic) 
summary(Model29)


Model30 = lm(SLEPTIM2 ~ DRANK_WEEKLY +BINGE_DRINKING +  AGE4 + AGE5 + AGE6 
	+ SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVER_MARRIED + FORMERLY_MARRIED
	+ FAIRHLTH + POORHLTH + COLLEGE
	+ INC2 + INC7 + OVERWEIGHT + OBESE + AGE3 + LOW_EDUCATION + MALE, data=analytic) 
summary(Model30)

#Remove the non significant variables
###FINAL MODEL

FinalLinearRegressionModel = lm(SLEPTIM2 ~ DRANK_MONTHLY + BINGE_DRINKING + DRANK_WEEKLY +  AGE5 + AGE6 
	+ HISPANIC + BLACK + ASIAN + OTHRACE + FORMERLY_MARRIED + NEVER_MARRIED
  + COLLEGE+ OVERWEIGHT + OBESE + SMOKER + FAIRHLTH + POORHLTH, data=analytic) 

summary(FinalLinearRegressionModel)




#output as CSV

library (broom)

FinalModel <- tidy(FinalLinearRegressionModel)
write.csv(FinalModel, file = "Final Linear Regression Model.csv")

ConfidenceLevel<-confint(FinalLinearRegressionModel, level = 0.95)
write.csv(ConfidenceLevel,file = "Final Model Confidence Level.csv")
















