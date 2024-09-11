#used this tutorial for inspo: https://bioconnector.github.io/workshops/r-survival.html#cox_ph_model

#load packages

#remember to install them first, if you haven't already
#eg install.packages("survminer")

library(dplyr)
library(survival)
library(survminer)
library(ggsurvfit)
library(codaredistlm)
library(compositions)

select=dplyr::select
filter=dplyr::filter


#lung is an open access dataset that comes with R
head(lung)

set.seed(123)
lung$sleep=sample(fairclough$sleep, 228, replace=TRUE)
lung$sb=sample(fairclough$sed, 228, replace=TRUE)
lung$lpa=sample(fairclough$lpa, 228, replace=TRUE)
lung$mvpa=sample(fairclough$mvpa, 228, replace=TRUE)
lung$id=seq(1:228)
d=lung %>% select(id,time, status, sex,age, sleep, sb, lpa,mvpa)
write.csv(d, "survivaldata.csv", row.names = FALSE)

###
setwd("/where/data/are")
d=read.csv("survivaldata.csv")

head(d)
str(d)
#time is time in days between start of study and followup (*censoring)
#status is whether they have diabetes or not at the follow up date (1=censored, 2=had event)
#age in years
#sex Male=1, Female=2

#######
#we have right censored data -> everyone entered the study at the same time but follow-up can vary
#######
d$event=factor(d$status)
ggplot(data=d, aes(x=time, y=id))+
  geom_segment(x=0, aes(xend=time, colour=event))+
  theme_bw()
#red means they are censored (no event as far as we know)
#blue : they had the event

#create survival object using the Surv() function
#It tells you both how long the subject was tracked for, 
#and whether or not the event occured or the sample was censored (shown by the +).
d$s = Surv(d$time, d$status)
head(d %>% select(time, status, s) )

#Survival curves
# fit a survival curve with the survfit() function.
#simplest - specify just an intercept (e.g., ~1)

survfit(s~1, data=d)
#same as 
(sfit = survfit(Surv(time, status)~1, data=d))

#summary of the above will show a life table.
summary(sfit)
#These tables show a row for each time point where either the event occured
#or a sample was censored. It shows the number at risk (number still remaining),
#and the cumulative survival at that instant.

#can separate the survival curve by sex
sfit <- survfit(Surv(time, status)~sex, data=d)
summary(sfit)

#might want the same time diffs for both sexes for comparability
# From these tables we can start to see that males tend to have 
#more events than females.
range(d$time)
summary(sfit, times=seq(0, 1000, 100))

#Kaplan-Meier Plots
#see the tables in a plot
plot(sfit)
#nicer plot with survminer package
ggsurvplot(sfit)

#Let’s add confidence intervals, show the p-value for the log-rank test (diff bw sexes), 
#show a risk table below the plot, and change the colors and the group labels.
ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Male", "Female"), legend.title="Sex",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curv", 
           risk.table.height=.25)

#but can't do this for continuous variable like age...
sfit <- survfit(Surv(time, status)~age, data=d)
plot(sfit)

#where Cox proporational hazards regression comes in
## IT doesn’t assume that the hazard is constant, 
#but does assume that the ratio of hazards between groups is constant over time.

####do it for binary predictor: sex first

fit <- coxph(Surv(time, status)~sex, data=d)
fit
#note exp(coef) in the output
#this is 0.588, which is the hazard ratio

#so going from male to female results in approximately ~40% reduction in hazard
#or females experience the event at 0.588x (less) the rate per unit time as males

#Also The 0.00111 p-value is really close to the p=0.00131 p-value we saw
#on the Kaplan-Meier plot.
summary(fit)
#shows exp(-coef): males experience the event approximately 1.7x 
#the rate per unit time as females


##add age
fit <- coxph(Surv(time, status)~sex + age, data=d)
fit
#hazard increases with age (but not significant).
#make some predictions for how risk changes with increasing age...
#the predict.coxph() function can already incorporate the difference from the mean
#prediction via the argument reference = "sample".
(mean.age = mean(d$age))
#the log hazard change due to increase in age 

(newdata1 = data.frame(age = c(mean.age, mean.age+10),
                      sex = 1))

(pnewM = predict(fit, newdata1))# for men: predicted natural log of the hazard at mean age vs hazard at 10 y later
0.3730396-0.2025863#differences in hazard for +10 y for men
pnewM[2]-pnewM[1]
exp(pnewM[2]-pnewM[1])#to get hazard ratio between the two ages

#repeat for women
(newdata2 = data.frame(age = c(mean.age, mean.age+10),
                       sex = 2))

(pnewF=predict(fit, newdata2))# for women: predicted hazard at mean age vs hazard at 10 y later
pnewF[2]-pnewF[1]#differences in hazard for +10 y for women (differences same for both sexes)
exp(pnewF[2]-pnewF[1])#to get hazard ratio between the two ages

############
#can get the predict function to give us the hazard ratio without doing it manually
(pnew1=predict(fit, newdata1,
              reference = "sample",
              type="terms",
              terms="age"))
#0.1704533
#same as  
pnewM[2]-pnewM[1]

#the difference in hazard ratio can be calculated by exponentiation of the above
exp(pnew1)
#1.185842
#same as 
exp(pnewF[2]-pnewF[1])

#########diff in sex?
(newdata3 = data.frame(age = mean.age,
                       sex = c(1,2)))
(pnewS=predict(fit, newdata3,
               reference = "sample",
               type="terms",
               terms="sex"))
exp(pnewS[2]-pnewS[1])
#compare with regression output
fit #look at exp(coef) - same!

###########################################################
##Advanced: compositional predictor of survival

#quick check for zeros
missingSummary(d%>%dplyr::select(sleep, sb, lpa, mvpa))
#make ilrs
d$ilrs <- ilr(d%>%dplyr::select(sleep, sb, lpa, mvpa))
str(d$ilrs)
d$ilr1= d$ilrs[,1]
d$ilr2= d$ilrs[,2]
d$ilr3= d$ilrs[,3]
str(d$ilr1)

#make model
fit <- coxph(Surv(time, status)~ cbind(ilr1, ilr2, ilr3) + sex + age, data=d)
summary(fit)
car::Anova(fit)#setting up with cbind(ilrs) lets us look at the significance of the overall composition
#overall composition is not significant.

#same model below, not using cbind(ilrs) because it makes things easier when doing predictions
fit <- coxph(Surv(time, status)~ ilr1 + ilr2 + ilr3 + sex + age, data=d)
summary(fit)

#predict risk for mean composition
#create mean and ilrs of the mean
(m <- clo(mean(acomp(d%>%dplyr::select(sleep, sb, lpa, mvpa))),total=1440))
ilr(m)
(ilr1m <- ilr(m)[1])
(ilr2m <- ilr(m)[2])
(ilr3m <- ilr(m)[3])

fit <- coxph(Surv(time, status)~ ilr1 + ilr2 + ilr3 + sex + age, data=d)
#predict for mean
newdata0 = data.frame(ilr1 = ilr1m,
                      ilr2 = ilr2m,
                      ilr3 = ilr3m,
                      age = mean.age,
                      sex = 1)
(pmean.0=predict(fit, newdata0))

#predict risk for reallocated composition (+60 to MVPA, take away from SB)
m1 <- c(m[1], m[2]-60, m[3], m[4]+60)
m1
#compare to m
m
m1-m

#express new composition as ilr
ilr1m1 <- ilr(m1)[1]
ilr2m1 <- ilr(m1)[2]
ilr3m1 <- ilr(m1)[3]

newdata1 = data.frame(ilr1 = ilr1m1,
                      ilr2 = ilr2m1,
                      ilr3 = ilr3m1,
                      age = mean.age,
                      sex = 1)
(pmean.1=predict(fit, newdata1))
exp(pmean.1-pmean.0) #

#the predict.coxph() function can already incorporate the difference from the mean
#prediction via the argument reference = "sample".
#the log hazard change due to reallocation can be calculated by summing the predictions for the 3 ilrs
(pnew=predict(fit, newdata1,
              reference = "sample",
              type="terms",
              terms=c("ilr1", "ilr2", "ilr3")))
sum(pnew)#sum to get log hazard change.
#the difference in hazard ratio can be calculated by exponentiation of the above
exp(sum(pnew))
#same as 
exp(pmean.1-pmean.0) #from above

#look at what happens when predict for the mean ilrs as the "new" situation
(pmean=predict(fit, newdata0,
               reference = "sample",
               type="terms", 
               terms=c("ilr1", "ilr2", "ilr3")))#only want to have predictions for ilr terms
sum(pmean)#this makes sense because I'm comparing the mean prediction to the sample mean (it's the same)
exp(sum(pmean))

####################################
###super advanced
#################################
#add CIs

#codes from epicoda package 
###get 95% CI for the difference in hazard ratio
# Calculate SE using variance-covariance matrix
modelmatrix <- vcov(fit)[c("ilr1", "ilr2", "ilr3"), c("ilr1","ilr2", "ilr3")]
#manually subtract predictions for new composition away from predictions for mean composition
x <- data.matrix(cbind(newdata1$ilr1, newdata1$ilr2, newdata1$ilr3) - cbind(newdata0$ilr1, newdata0$ilr2, newdata0$ilr3))
t_x <- t(x)#transpose matrix
in_sqrt_true <- diag((x %*% modelmatrix) %*% t_x)
value <- sqrt(data.matrix(in_sqrt_true))#get SE of difference
# Calculate Wald CI assuming critical value approximately 1.96 for 95% CI
z_value <- stats::qnorm(0.975)  
alpha_lower <- sum(pnew) - z_value * value
alpha_upper <- sum(pnew) + z_value * value
(lowerCI <- exp(alpha_lower))
(upperCI <- exp(alpha_upper))

#estimate for Hazard Ratio with 95% CI:
c( exp(sum(pnew)), lowerCI, upperCI) #and lower and upper CIs from above
#the 95% CI cross 1, so not statistically significant.
