# Survival-data-analysis

## Introduction
This project aimed to analyze the Randomised International Stroke Trial Dataset to investigate the survival time of patients treated with aspirin. The dataset consisted of 19,435 patients with acute ischemic stroke who were randomized to different combinations of aspirin and heparin treatments. The analysis focused on examining the effect of aspirin on time to death, time to death due to specific causes, and the association between covariates such as sex, age, systolic blood pressure, and stroke type with time to death.

## Methodology
The methodology involved extracting seven variables from the dataset and removing missing values. The effect of aspirin on the outcome variable was assessed using a log-rank test and Cox proportional hazards models. Model building was conducted using Cox proportional hazards models, considering variables such as age, sex, systolic blood pressure, and stroke type. The final model revealed that age and stroke type had a significant impact on the risk of death, while the effect of aspirin treatment was not statistically significant.

## Results and Discussion
The results showed that aspirin therapy had little effect on reducing mortality in patients with acute ischemic stroke. The survival curves varied between genders, with males having a longer survival period than females. However, the difference was not statistically significant. The analysis also examined the effect of aspirin on specific causes of death, such as pulmonary embolism. The results showed no significant difference in death rates due to pulmonary embolism or other causes between the treatment groups.

## Conclusion 
Overall, the analysis of the Randomised International Stroke Trial Dataset found that aspirin treatment had minimal impact on the survival time of patients with acute ischemic stroke. The study highlighted the importance of age and stroke type in predicting the risk of death.

```
# Survival group project

#PACKAGES
library(car)
library(survival)
library(tidyverse)
library(survminer)
library(caTools)
library(ggplot2)
library(gtsummary)
library(cmprsk) 

#Reading and viewing the data
data <- read.table("ITS_Clean.csv", sep = ',', header = TRUE) 

head(data)  # six observations
tail(data)  # last six observations
names(data) # names of the variables 
dim(data)   # dimensions
str(data) 

# selecting variables of interst

select <- c("HOSPNUM","SEX","AGE","RSBP","STYPE","RXASP","Death_Time_6",
          "Death_Ind_6","Death_C_Ind_6", "Death_C", "STYPE2")
data_red <- data[ ,which(names(data) %in% select)]
data_red<- na.omit(data_red)

head(data_red)  # first six observations
tail(data_red)  # last six observations
names(data_red) # names of the variables of the dataset
dim(data_red)   # dimensions
str(data_red) 

#renaming variables
data_red<-data_red %>%rename(Hospital=HOSPNUM ,
                   SBP=RSBP ,Time=Death_Time_6,
                    Status=Death_Ind_6,
                     Death=Death_C_Ind_6,
                      Aspirin =RXASP,
                   Stroke =STYPE,
                   Stroke_new=STYPE2,
                   Event=Death_C)

#changing the levels of some variables

#levels(data_red$SEX)[levels(data_red$SEX)=="M"] <- "1"
#levels(data_red$SEX)[levels(data_red$SEX)=="F"] <- "0"
levels(data_red$Aspirin)[levels(data_red$Aspirin)=="Y"] <- "Yes"
levels(data_red$Aspirin)[levels(data_red$Aspirin)=="N"] <- "No"
levels(data_red$Status)[levels(data_red$Status)=="TRUE"] <- "1"
levels(data_red$Status)[levels(data_red$Status)=="FAlse"] <- "0"

# recode status indicator
data_red$Status<- as.factor(data_red$Status)

#Descriptive analysis for training data
summary(data_red)
table(data_red$Death)

#Histogram of continous variables
hist(data_red$SBP, prob="true", main="Histogram for Systolic blood pressure(mmHg) of patients ", 
     xlab="SBP",  border="green",  col="blue",
     las=1, breaks=5)
hist(data_red$AGE, main="Histogram for Age of patients ", 
     xlab="AGE",  border="green",  col="blue",
     las=1, breaks=5)

hist(data_red$Time, main="Histogram for time to death at 6 month of patients ", 
     xlab="Time",  border="green",  col="blue",
     las=1, breaks=5)

#status
data_red$Status<- car::recode(data_red$Status, "'TRUE'=1; 'FALSE'=0;", as.factor=FALSE)

# overall KM Curve
fit1=survfit(Surv(Time,Status)~1,type="kaplan-meier",conf.type="log-log", data=data_red)
plot(fit1,  conf.int=TRUE, xlab = "Time (Days) ",ylab = "S",
       main = "Estimated Marginal Survival Curve",ylim = c(0.5,1.0))

#KM stratied by treatment
fit2=survfit(Surv(Time,Status)~Aspirin,type="kaplan-meier",conf.type="log-log", data=data_red)
ggsurvplot(fit2, data = data_red, conf.int = TRUE, pval = TRUE,
           risk.table = TRUE)

#KM startified by Gender
fit3=survfit(Surv(Time,Status)~SEX,type="kaplan-meier",conf.type="log-log",data=data_red)
ggsurvplot(fit3, data = data_red, conf.int = TRUE, pval = TRUE,
           risk.table = TRUE)

#Log-rank test by SEX 

LR_S=survdiff(formula=Surv(Time,Status)~SEX,
             data=data_red)

#Log-rank test by treatment  

LR_T=survdiff(formula=Surv(Time,Status)~Aspirin,
              data=data_red)

#checking the validity of log rank startified by treatment and gender

plot(fit2, xlab="Log(time)", ylab="Log(-log(S(t)))", fun = "cloglog", col=1:2); 
legend("bottomright", names(fit2$strata), bty = "n", col = 1:2,lty = 1)

plot(fit3, xlab="Log(time)", ylab="Log(-log(S(t)))", fun = "cloglog", col=1:2); 
legend("bottomright", names(fit3$strata), bty = "n", col = 1:2,lty = 1)

#Q1
#cumulative hazard

plot(fit2, conf.int=T, fun = function(x) 1-x, 
     xlab = "Time in days", ylab = "Cumulative incidence (%)",
     ylim = c(0.0,0.5),col = 2:1,main = "Cumulative Incidence");
legend("topleft", names(fit2$strata), bty = "n", lty = 1, col = 1:2)

#Cox Model to see the effect of treatment 
fitcox1<-coxph(Surv(Time, Status) ~ Aspirin, data = data_red)
summary(fitcox1)
coxph(Surv(Time, Status) ~ Aspirin, data = data_red) %>% 
        gtsummary::tbl_regression(exp = TRUE) 

#Q2
fitcox2<-coxph(Surv(Time, Status) ~ Aspirin+strata(Hospital), data = data_red)
summary(fitcox2)

#it will be difficult however to plot a KM curve 

str(data_red)
#Q3
fit.ci <- cuminc(data_red$Time, data_red$Event, data_red$Aspirin, cencode=0)
fit.ci$Tests
ggcompetingrisks(fit.ci)

event_c<- data.frame( Event = 0:2,
                         event_char = c("No Event", "Death_C", "Death_Other"))
data_red <- data_red%>%left_join(event_c)

with(data_red, table(Event, event_char))

fit.ci_c <- cuminc(data_red$Time, data_red$event_char, data_red$Aspirin, cencode="No Event") 
ggcompetingrisks(fit.ci_c)

#merging the plot

fit.ci_c2 <- cuminc(data_red$Time, data_red$event_char, cencode="No Event")  
ggcompetingrisks(fit.ci_c2)
str(data_red)

# cause specific hazard for death due to plumunary disease
death_c <- coxph(formula = Surv(Time, Event==1) ~ Aspirin, data = data_red)

summary(death_c)

## cause specific hazard for death due to other cause
death_o <- coxph(formula = Surv(Time, Event==2) ~ Aspirin, data = data_red)

summary(death_o)
#Q4
#advantage
#power increase

#disadvanteg
#type I error (false postive )

#Q5

fitcox3<-coxph(Surv(Time, Status) ~ AGE, data = data_red) # (significant)
summary(fitcox3)

#5A
#estimated survival curve
survest <-  predict(coxph(Surv(Time, Status) ~ Aspirin + AGE, data=data_red), newdata = data_red,type="lp")
median  <-  median(survest)
q1  <-  quantile(survest, 0.25)
q3  <-  quantile(survest, 0.75)
basehaz  <-  basehaz(coxph(Surv(Time, Status) ~ Aspirin + AGE, data=data_red), centered=TRUE)
plot(basehaz$time, exp(-basehaz$hazard*exp(q3)), type='l', xlab = "Prognostic score", ylab = "Estimated survival")
lines(basehaz$time, exp(-basehaz$hazard*exp(median)), col='red')
lines(basehaz$time, exp(-basehaz$hazard*exp(q1)), col='blue')
legend("bottomleft", c("Q1", "Median", "Q3"), lty = 1, col = c("blue", "red", "black"), bty = "n", cex=1.1)

#probability of surviving for more than 1 year function of age
basehaz12  <-  max(which(basehaz$time<=365))
plot(survest, exp(-basehaz$hazard[basehaz12]*exp(survest)), type='l', xlab = "Prognostic score",
     ylab = "Probability of surviving for more than 365 days")

#linearity check
plot(data_red$AGE ,residuals(fitcox3, type=c("martingale")), xlab = "Age of Subject")
lines(lowess(data_red$AGE, resid(fitcox3)),col='red')

#Q6 we continue with linear, we added SBP to the linear age 

fitcox4<-coxph(Surv(Time, Status) ~ AGE+SBP, data = data_red) 
summary(fitcox4)

#Q7

#SBP without age 
#Age without SBP

fitcox5<-coxph(Surv(Time, Status) ~ SBP, data = data_red) 
fitcox6<-coxph(Surv(Time, Status) ~ AGE, data = data_red) 

summary(fitcox5)
summary(fitcox6)
#comparing the two model using AIC 
extractAIC(fitcox5)
extractAIC(fitcox6)

#Q8A
fitcox7<-coxph(Surv(Time, Status) ~ Aspirin+AGE+SEX+SBP+Stroke+ Stroke*Aspirin, data = data_red) 
summary(fitcox7)
Anova(fitcox7)

#replacing Stroke with Stroke New
fitcox8<-coxph(Surv(Time, Status) ~ Aspirin+AGE+SEX+SBP+Stroke_new+Stroke_new*Aspirin, data = data_red) 
summary(fitcox8)
#interaction effect (not significant)
Anova(fitcox8)

#Final Model
fitcox9<-coxph(Surv(Time, Status) ~ Aspirin+AGE+SEX+SBP+Stroke_new, data = data_red) 
summary(fitcox9)

#table cox model
HR <- round(exp(coef(fitcox9)), 2)
CI <- round(exp(confint(fitcox9)), 2)
SE<-round(coef(summary(fitcox9))[,3], 3)
P <- round(coef(summary(fitcox9))[,5], 3)
# Names the columns of CI
colnames(CI) <- c("Lower", "Higher")
# Bind columns together as dataset
table1 <- as.data.frame(cbind(HR, CI, SE,P))
table1

#assumption PH
final_zph <- cox.zph(fitcox9)
ggcoxzph(cox.zph(fitcox9))

#9
fit10 <- survfit(coxph(Surv(Time, Status)
                         ~ AGE+SEX+SBP+Stroke_new
                         + strata(Aspirin), data=data_red))

plot(fit10, conf.int=T, xlab = "Time (Days)",ylab = "S(t)", main = "Survival Curve stratified by Treatment",
     ylim = c(0.5,1.0),col = c(1:2));
legend("topright", names(fit10$strata), bty = "n", lty = 1, col = c(1:2))




```
