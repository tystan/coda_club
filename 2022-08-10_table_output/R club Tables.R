#set the working directory to the same folder that has this script and the dataset saved in it.
setwd("C:/Users/xxx/yyy/R club")

#get example dataset
data = read.csv("Fairclough.csv")

#look at the dataset
#top few rows
head(data)
#there are a few weird names which might be worth changing for ease of calling them later on

#look at the end of the dataset
tail(data)

#select the variables that I want (we can rename them during the selection process)
#use the dplyr package
install.packages("dplyr")
require(dplyr)

d1 = data %>% dplyr::select(school=School, sex=Sex, age=Decimal.Age..y., ses=IMD.Decile,
                     height=Height..cm., mass=Mass..cm., bmi=BMI, zbmi=zBMI,
                     owob=IOTF.grade, waist=Waist.Circumference..cm., wht=WHtR,
                     shuttles=Total.20m.Shuttles, sb=Sedentary, lpa=Light,
                     mvpa=MVPA, sleep)

head(d1)
#see what sort of variables R thinks these are
str(d1)
#will need to change some of these variables to factors.
d1$school = factor(d1$school)
d1$sex = factor(d1$sex)
d1$owob = factor(d1$owob)
#check
str(d1)

#are there any missing values?
table(is.na(d1))
#yes, there is one missing value.

#where is is?
is_na = is.na(d1) 
head(is_na)#FALSE are attributed a 0 value, TRUE a 1 value.
colSums(is_na) #shows us that the NA is in OWOB.

subset(d1, is.na(d1$owob))
#could actually fill in the value, but for now we will exclude this observation

#can use na.omit(dataset name) to get rid of all rows/IDs that have missing data
nrow(d1)
d2 = na.omit(d1)
nrow(d2)
table(is.na(d2))

#or can choose to remove rows that have NAs in a specific variable/column
d3 = subset(d1, !is.na(d1$owob))
nrow(d3)
table(is.na(d3))

#if you choose a different column nothing happens
d4 = subset(d1, !is.na(d1$mvpa))
nrow(d4)
table(is.na(d4)) #the owob NA is still there.

####MAKE A SAMPLE DESCRIPTIVES TABLE 
#my complete analytical dataset is "d2"

#descriptive tables are made very easy using the tableone package

install.packages("tableone")
require(tableone)


CreateTableOne(vars=c("sex","age","ses"), factorVars = c("sex"), data=d2)

#for all variables in d2?
names(d2)
CreateTableOne(vars=names(d2), factorVars = c("sex", "school","owob"), data=d2)
CreateTableOne(vars=names(d2[-1]), factorVars = c("sex", "school","owob"), data=d2)

#by sex
CreateTableOne(vars=names(d2[-1]), factorVars = c("sex", "school","owob"),
               strata="sex", data=d2)



####RESULTS TABLES - eg results from linear regression models

#research Q : what personal and sociodemographic factors are correlates of 
#physical activity, sedentary behaviour and sleep?

#will have 4 models, one for each of MVPA, LPA, SB and sleep.
names(d2)
mod.mvpa = lm(mvpa ~ sex + age + ses + school + zbmi + waist + shuttles, data = d2)
(sum.mvpa=summary(mod.mvpa))
car::Anova(mod.mvpa)#check multivariate tests for factor variables
(coef.mvpa=sum.mvpa$coefficients)
(ci.mvpa=confint(mod.mvpa))
(beta= round(coef.mvpa[,1],1))
(cis=round(ci.mvpa,1))
(pval=round(coef.mvpa[,4],3))
(mvpa.sum = cbind.data.frame(beta,cis,pval))

#LPA
mod.lpa = lm(lpa ~ sex + age + ses + school + zbmi + waist + shuttles, data = d2)
(sum.lpa=summary(mod.lpa))
car::Anova(mod.lpa)#check multivariate tests for factor variables
(coef.lpa=sum.lpa$coefficients)
(ci.lpa=confint(mod.lpa))
(beta= round(coef.lpa[,1],1))
(cis=round(ci.lpa,1))
(pval=round(coef.lpa[,4],3))
(lpa.sum = cbind.data.frame(beta,cis,pval))

#SB
mod.sb = lm(sb ~ sex + age + ses + school + zbmi + waist + shuttles, data = d2)
(sum.sb=summary(mod.sb))
car::Anova(mod.sb)#check multivariate tests for factor variables
(coef.sb=sum.sb$coefficients)
(ci.sb=confint(mod.sb))
(beta= round(coef.sb[,1],1))
(cis=round(ci.sb,1))
(pval=round(coef.sb[,4],3))
(sb.sum = cbind.data.frame(beta,cis,pval))

#sleep
mod.sleep = lm(sleep ~ sex + age + ses + school + zbmi + waist + shuttles, data = d2)
(sum.sleep=summary(mod.sleep))
car::Anova(mod.sleep)#check multivariate tests for factor variables
(coef.sleep=sum.sleep$coefficients)
(ci.sleep=confint(mod.sleep))
(beta= round(coef.sleep[,1],1))
(cis=round(ci.sleep,1))
(pval=round(coef.sleep[,4],3))
(sleep.sum = cbind.data.frame(beta,cis,pval))

#combine into one summary results table
#can choose to put them side by side or down the page

#side by side
cbind(mvpa.sum, lpa.sum, sb.sum, sleep.sum)

#down the page
rbind(mvpa.sum, lpa.sum, sb.sum, sleep.sum)


#of course, R has packages that does this for you
#here is an example
install.packages("sjPlot")
require(sjPlot)

tab_model(mod.mvpa, mod.lpa, mod.sb, mod.sleep)
#put cis in same column
tab_model(mod.mvpa, mod.lpa, mod.sb, mod.sleep, collapse.ci = TRUE)
tab_model(mod.mvpa, mod.lpa, mod.sb, mod.sleep, collapse.ci = TRUE, p.style="stars" )
#lots of options
# plhttps://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html

#save as word document
tab_model(mod.mvpa, mod.lpa, mod.sb, mod.sleep, file="results.doc")
tab_model(mod.mvpa, mod.lpa, mod.sb, mod.sleep, file="results.html")

#plot
plot_model(mod.mvpa) #plots coefficients

#plots predicted values of mvpa across a predictor
plot_model(mod.mvpa, type="eff", terms="shuttles")
range(d2$shuttles)
#lots of options
# http://www.strengejacke.de/sjPlot/reference/plot_model.html