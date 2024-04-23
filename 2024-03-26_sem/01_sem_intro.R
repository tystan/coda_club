
# ---- preamble ----


# ::::::::::::::::: #
# ::: SEM ::: #
# ::::::::::::::::: #

# Structural equation modelling

# ::::::::::::::::: #
# ::: what is? ::: #
# ::::::::::::::::: #

# data analysis technique that handles data that can be conceptualised
# in paths diagrams (fancy boi version: directed acyclic graph (DAG))
# (is this related to (simple or multiple) linear regression? YAAS)


# ::::::::::::::::: #
# ::: why use? ::: #
# ::::::::::::::::: #

# multiple regression equations (intermediate outcomes that "predict" other outcomes)
# latent variables
# mediation (related to multiple regression equations: direct and indirect effects)
# can use summary statistics instead of person level data
# [almost certainly heaps more]

# flow chart: 
#    https://kevintshoemaker.github.io/NRES-746/SEM%20Models.png
# also worth noting, not everyone loves SEMs: 
#     https://en.wikipedia.org/wiki/Structural_equation_modeling#Controversies_and_Movements


# ::::::::::::::::: #
# ::: who use? ::: #
# ::::::::::::::::: #

# social sciences mainly (examples: psychology, education, economics, politics, business sciences, etc)
#     https://link.springer.com/chapter/10.1007/978-3-030-80519-7_1/tables/1
# (why? my guess: SEM allows analysis of complex relationships - people are complex)


# ::::::::::::::::: #
# ::: examples in research ::: #
# ::::::::::::::::: #


### the best and only one :-)
# https://www.tandfonline.com/doi/full/10.1111/ajpy.12083

### thanks to jaslie for this example:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3395924/

# so many (screenshots shown, but references on request)

# ::::::::::::::::: #
# ::: references used ::: #
# ::::::::::::::::: #

# fantastic overview: 
#    https://kevintshoemaker.github.io/NRES-746/SEM.RMarkdown.html
# software in R: [lavaan]
#    https://lavaan.ugent.be/tutorial.pdf
#    https://lavaan.ugent.be/about/gettingstarted.html
#    https://users.ugent.be/~yrosseel/lavaan/lavaan2.pdf
#    https://www.jstatsoft.org/article/view/v048i02
# UCLA as always doing their thing: 
#    https://stats.oarc.ucla.edu/r/seminars/rsem/
# deeper dive but fantastic lecture slides from a decade ago: 
#    http://cda.psych.uiuc.edu/CovarianceStructureAnalysis/Lectures/
# academic journal intros if you fancy yo: 
#    [An Introduction to Structural Equation Modeling (2021)] 
#        https://doi.org/10.1007/978-3-030-80519-7_1 
#    [Structural equation modeling in medical research: a primer (2010)]
#        https://doi.org/10.1186/1756-0500-3-267




# ::::::::::::::::: #
# ::: sem specific definitions ::: #
# ::::::::::::::::: #




# ::::::::::::::::: #
# ::: when no use? ::: #
# ::::::::::::::::: #

# not assured of causality?


# ::::::::::::::::: #
# ::: assumptions? ::: #
# ::::::::::::::::: #

# multivariate normality? (sorta)
# continuous variables? (sorta)



# ---- libs ----

# install.packages(c("lavaan", "semPlot")

library("lavaan") # SEM + meditation analysis 
library("semPlot") # SEM diagrams

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("ggthemes")
library("GGally")




# ---- some_data ----

### THANK YOU TO THESE AUTHORS FOR MAKING DATA PUBLIC!
# Data are from:
#   Mongin, D., García Romero, J., & Alvero Cruz, J. R. (2021). 
#   Treadmill Maximal Exercise Tests from the Exercise Physiology 
#   and Human Performance Lab of the University of Malaga (version 1.0.1). 
#   PhysioNet. https://doi.org/10.13026/7ezk-j442.

### licensing of the data:
# https://physionet.org/content/treadmill-exercise-cardioresp/1.0.1/LICENSE.txt

### (https://physionet.org/content/treadmill-exercise-cardioresp/1.0.1/#methods)
# Methods
#
# The measurements were taken between 2008 and 2018. The athletes performed a
# maximal Graded Exercise Testing (GET) on a PowerJog J series treadmill
# connected to a CPX MedGraphics gas analyzer system (Medical Graphics, MN, USA)
# with breath-by-breath measurements of respiratory parameters -including oxygen
# consumption and pulmonary ventilation- and heart rate collected by a Mortara
# 12-lead ECG device.
#
# The stress tests consisted of a continuous (ramping) or step-by-step
# incremental effort. Most of the exercise phases are preceded by a warmup
# period of walking at 5 km/h. When incremental, the step amplitudes range from
# 0.5 to 1 km/h. The participants were asked to go beyond exhaustion, and the
# test was considered maximal if the oxygen consumption was saturated. The
# effort was then ceased, and to avoid vasovagal syncope, the treadmill speed
# was set back to the initial 5 km/h speed, and the participant was asked to
# walk.

### data dictionary for full data
# ID_test  	 |  	992 per ID
# ID  	 |  	857
# Age (years)  	 |  	27.10 [21.10, 36.32]
# Weight (kg)  	 |  	73.00 [66.00, 80.23]
# Height (cm)  	 |  	175.00 [170.00, 180.00]
# Humidity (%)  	 |  	47.00 [42.00, 54.00]
# Temperature (°C)  	 |  	22.90 [20.80, 24.40]
# Sex = 1 (Female) (%)  	 |  	149 (15.0)
# time  	 |  	Time since the measurement starts, in seconds
# Speed  	 |  	Speed of the treadmill, in km/h
# HR  	 |  	Heart rate, in beat per min
# VO2  	 |  	Oxygen consumption, in mL/min
# VCO2  	 |  	Carbon dioxide production, in mL/min
# RR  	 |  	Respiration rate, in respiration per minute
# VE  	 |  	Pulmonary ventilation, in L/min
# ID  	 |  	Participant identification
# ID_test  	 |  	Effort test identification



# ---- from_little_things_big_things_grow ----

### (simple) linear regression:

# y_i = b0 + b1 x_i + e_i

# where 
#    y_i are continuous values
#    e_i are normally distributed error/residuals
#       e_i ~ N(0, sigma^2)


# https://stats.oarc.ucla.edu/r/seminars/rsem/#s2

# y_1 = alpha * 1 + gamma_1 * x_1 + 1 * upzeta_1
#       upzeta_1 ~ N(0, psi_11)
#       x_1 ~ N(0, phi_11)


maxhr <- 
  read_csv(
    "2024-03-26_sem/maxhr.csv"
  )

maxhr

# make VO2 version in litres
maxhr$vo2 <- maxhr$VO2 / 1e+3



# pairwise plots of demographics
maxhr %>% 
  select(Age, Weight, Height, Humidity, Temperature, VO2, HR) %>% 
  na.omit(.) %>%
  ggpairs(
    ., 
    mapping = aes(alpha = 0.2)
  ) + 
  theme_bw() 


# model the data using lm()

lm_fit1 <- lm(VO2 ~ 1 + Weight, data = maxhr)
summary(lm_fit1)

lm_fit1a <- lm(vo2 ~ 1 + Weight, data = maxhr)
summary(lm_fit1a)


# model the data using sem() from lavaan

sem1 <-   '
  # regressions
    VO2 ~ 1 + Weight
'

maxhr_na_rm <- maxhr %>% select(VO2, Weight) %>% na.omit(.)

# sem_fit1 <- sem(sem1, data = maxhr)
sem_fit1 <- sem(sem1, data = maxhr_na_rm)

# sample.cov:
# Numeric matrix. A sample variance-covariance matrix. The rownames
# and/or colnames must contain the observed variable names. For a multiple group
# analysis, a list with a variance-covariance matrix for each group. 
# sample.mean
# A sample mean vector.  For a multiple group analysis, a list with a mean vector
# for each group.
# sample.nobs	
# Number of observations if the full data frame is missing and only sample 
# moments are given. For a multiple group analysis, a list or a vector with 
# the number of observations for each group.


summary(sem_fit1, fit.measures = FALSE)
summary(sem_fit1, fit.measures = TRUE)

(samp_m <- colMeans(maxhr_na_rm, na.rm = TRUE))
(samp_vc <- var(maxhr_na_rm, na.rm = TRUE))
(samp_n <- nrow(maxhr_na_rm))
# cov = corr * sd1 * sd2


# alternative way to fit
sem_fit2 <- 
  sem(
    sem1, 
    sample.cov = samp_vc, 
    sample.mean = samp_m, 
    sample.nobs = samp_n
  )

# same result!
summary(sem_fit2, fit.measures = FALSE)


# Plot the mediation diagram with path estimates
semPaths(sem_fit1, intAtSide = TRUE)
semPaths(sem_fit1, whatLabels = "est", style = "lisrel", intercepts = FALSE)
semPaths(sem_fit1, what = "paths", residuals = TRUE)
# Standardized estimates:
semPaths(sem_fit1,"std","est", intAtSide=TRUE)







