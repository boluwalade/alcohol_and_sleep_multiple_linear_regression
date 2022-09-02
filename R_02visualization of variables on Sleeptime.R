analytic <- read.csv("analytic.csv", header = TRUE)

#See box plots of groups next to each other (comparing alcohol consumption with sleep time in veteran and non veteran)
#its called bivariate analysis
Non_veteran <- subset(BRFSS_b, VETERAN3==2 & SLEPTIM1 < 77) 
Non_veteran$ALCOHOL_GROUP[Non_veteran$ALCDAY5 <200 ] <- 3
Non_veteran$ALCOHOL_GROUP[Non_veteran$ALCDAY5 >=200 & Non_veteran$ALCDAY5 <777] <- 2
Non_veteran$ALCOHOL_GROUP[Non_veteran$ALCDAY5 == 888] <- 1
dim(Non_veteran)


#boxplot showing association between alcohol intake and sleep duration in veterans
boxplot(SLEPTIM1~ALCOHOL_GROUP, data=analytic, main="Box Plot of SLEEP DURATION by ALCOHOL INTAKE LAST MONTH(VETERAN)", 
        xlab="ALCOHOL GROUP", ylab="SLEEP DURATION(HRS)", names= c("None","One or more", "One or more/week"))


#boxplot showing relationship between alcohol intake and sleep duration in non_veterans
boxplot(SLEPTIM1~ALCOHOL_GROUP, data=Non_veteran, main="Box Plot of SLEEP TIME by ALCOHOL INTAKE LAST MONTH (NON-VETERAN)", 
        xlab="ALCOHOL GROUP", ylab="SLEEP DURATION(HRS)", names= c("None","One or more", "One or more/week"))

#COMPARISON BETWEEN SLEEP TIME FOR VETERAN AND NON VETERAN BY AGE

boxplot(SLEPTIM1~X_AGE_G, data=analytic, main="Box Plot of SLEEP DURATION by AGE FOR VETERAN", 
        xlab="AGE GROUP", ylab="SLEEP DURATION (HRS)",names=c("18-24","25-34","35-44","45-54","55-64",">65"))



boxplot(SLEPTIM1~X_AGE_G, data=Non_veteran, main="Box Plot of SLEEP DURATION by AGE FOR NON VETERAN", 
        xlab="AGE GROUP", ylab="SLEEP DURATION (HRS)",names=c("18-24","25-34","35-44","45-54","55-64",">65"))

#using the analytic.csv file bivariate analysis was performed and displayed
# boxplots

