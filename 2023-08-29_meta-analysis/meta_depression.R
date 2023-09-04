
### Many, many thanks to Jacinta Brinsley for
### sharing her knowledge, this code and leading the session


#---- install packages----

install.packages(c("meta", "metafor", "metaforest", "esc", "forestplot"))


#---- open libraries ----

library(tidyverse) # likely already installed
library(meta)
library(metafor)
library(metaforest)
library(esc)
library(forestplot)



# ---- load and check data ----

#load data to workspace
depression <- read_csv("2023-08-29_meta-analysis/depression_data.csv")



#check the data have imported correctly
colnames(depression)



#check classification of variables - change to as.factor, as.numeric, as.character
str(depression)
depression$year_cat <- as.factor(depression$year_cat)
depression$pedro_cat <- as.factor(depression$pedro)
depression$population <- as.factor(depression$population)
depression$control_group <- as.factor(depression$control_group)
depression$behaviour <- as.factor(depression$behaviour)
depression$wearable <- as.factor(depression$wearable)
depression$app <- as.factor(depression$app)
depression$web <- as.factor(depression$web)



#sms 
#multi_delivery



#######################################################################################



#---- calculate SMDs----
depression_SMDs <- escalc(measure="SMD",
                          m1 = int_m,
                          m2 = con_m,
                          sd1 = int_sd, 
                          sd2 = con_sd,
                          n1 = int_n,
                          n2 = con_n,
                          data=depression)



######################################################################################



#---- Pool effect sizes / meta-analyze!!! ----



#metafor
dep_rma <- rma(yi = depression_SMDs$yi,     # The d-column of the df, which contains Cohen's d
               vi = depression_SMDs$vi,  # The vi-column of the df, which contains the variances
               measure = "SMD",
               weighted=TRUE,
               method = "REML",
               test = "knha")   
dep_rma



#meta package meta-analysis of continuous, raw data
meta.dep <- metacont(n.e = int_n,
                     mean.e = int_m,
                     sd.e = int_sd,
                     n.c = con_n,
                     mean.c = con_m,
                     sd.c = con_sd,
                     studlab = study,
                     data = depression,
                     sm = "SMD",
                     method.smd = "Hedges",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Effect of Lifestyle Interventions on Depressive Symptoms")
summary(meta.dep)



#n per group
depression_SMDs %>%
  summarise(
    I = sum(int_n),
    C = sum(con_n))



#########################################################################################



#----Sensitivity analyses / exploring heterogeneity----
#----removing outliers



# #find the upper and lower bound of confidence intervals
dep_rma$ci.ub
dep_rma$ci.lb



# use these boundaries to determine if there are any outliers in the data



# Calculate CI for all observed effect sizes
depression_SMDs$upperci <- depression_SMDs$yi + 1.96 * sqrt(depression_SMDs$vi)
depression_SMDs$lowerci <- depression_SMDs$yi - 1.96 * sqrt(depression_SMDs$vi)



# Create filter variable
depression_SMDs$outlier <- depression_SMDs$upperci < dep_rma$ci.lb | depression_SMDs$lowerci > dep_rma$ci.ub



# Count number of outliers:
sum(depression_SMDs$outlier)



# work out which studies are outliers/listed in dataframe
depression_SMDs[depression_SMDs$outlier, c("yi", "upperci", "lowerci")]



#does the pooled effect size change if we remove outliers from the analysis? 
dep_rma_no_out <- rma(yi = depression_SMDs$yi[!depression_SMDs$outlier],
                      vi = depression_SMDs$vi[!depression_SMDs$outlier],
                      measure = "SMD",
                      weighted = TRUE,
                      method = "REML",
                      test = "knha")
dep_rma_no_out



#OR 



cat("SMD = ", round(dep_rma_no_out$b[1], 2),
    ", 95% CI [", round(dep_rma_no_out$ci.lb, 2),
    ", ", round(dep_rma_no_out$ci.ub, 2), "] (no outliers)", sep = "")



#---- removing low quality studies
depression_SMDs$lowqual <- depression_SMDs$pedro < 6
sum(depression_SMDs$lowqual)



dep_rma_no_lowqual <- rma(yi = depression_SMDs$yi[!depression_SMDs$lowqual],
                          vi = depression_SMDs$vi[!depression_SMDs$lowqual],
                          measure = "SMD",
                          weighted = TRUE,
                          method = "REML",
                          test = "knha")



cat("SMD = ", round(dep_rma_no_lowqual$b[1], 2),
    ", 95% CI [", round(dep_rma_no_lowqual$ci.lb, 2),
    ", ", round(dep_rma_no_lowqual$ci.ub, 2), "] (no poor/fair quality studies)", sep = "")




#----removing small studies
depression_SMDs$totalsample <- depression_SMDs$int_n + depression_SMDs$con_n



depression_SMDs$smalln <- depression_SMDs$totalsample < 101



sum(depression_SMDs$smalln)



dep_rma_no_smalln <- rma(yi = depression_SMDs$yi[!depression_SMDs$smalln],
                         vi = depression_SMDs$vi[!depression_SMDs$smalln],
                         measure = "SMD",
                         weighted = TRUE,
                         method = "REML",
                         test = "knha")



cat("SMD = ", round(dep_rma_no_smalln$b[1], 2),
    ", 95% CI [", round(dep_rma_no_smalln$ci.lb, 2),
    ", ", round(dep_rma_no_smalln$ci.ub, 2), "] (no studies where n = <100)", sep = "")



######################################################################################



# ---- funnel plot / publication bias----
funnel(meta.dep,
       studlab = TRUE)



#fancy funnel
col.contour = c("gray75", "gray85", "gray95")



funnel.meta(meta.dep,
            xlim = c(-1.6,1.2),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour,
            studlab = TRUE)



legend(x=-1.5, y=0.00,
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)



title("Funnel Plot: Lifestyle interventions for depressive symptoms")




#---- Egger's test
metabias(meta.dep, method.bias = "linreg")



#---- Begg and Mazumdar
ranktest(dep_rma)



#Duval & Tweedie's trim-and-fill procedure (only if Egger's is significant)
meta.dep$I2
tf <- trimfill(meta.dep)
summary(tf)



#######################################################################################



# ---- forest plots ----
forest.meta(meta.dep)




# open pdf "plotting device"
pdf("2023-08-29_meta-analysis/depression_plot.pdf", width = 12, height = 13) # w/h in inches



#add plot to "plotting device"
forest.meta(meta.dep,
            print.tau = FALSE,
            sortvar = TE,
            leftcols = c("studlab", "int_n", "int_m", "int_sd", "con_n", "con_m", "con_sd"),
            leftlabs = c("Author", "N", "M", "SD", "N", "M", "SD"),
            rightcols = c("effect", "ci", "w.random"),
            rightlabs = c("SMD", "95% CI", "Weight"),
            label.left = "Favours intervention",
            label.right = "Favours control",
            addrows.below.overall = 3,
            col.square = "pink",
            col.diamond = "blue")



# close off plotting device
dev.off()




######################################################################################



#----subgroup analyses----
popsubgroup <- update.meta(meta.dep, 
                           subgroup = population,
                           tau.common = FALSE)
popsubgroup
popsubgroup$pval.random.w



#----report the number of participants in each subgroup



depression_SMDs %>%
  filter(population ==1) %>%
  summarise(
    H.I = sum(int_n),
    H.C = sum(con_n))




#----meta-regressions---- 
depreg.dur <- metareg(meta.dep, ~duration)
depreg.dur



bubble(depreg.dur, studlab = TRUE, col.line = "blue")

