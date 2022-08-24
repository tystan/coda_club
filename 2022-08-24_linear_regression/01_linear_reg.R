

# ---- preamble -----

### Simple Linear Regression
# Breakdown:
# Simple = one (predictor) variable
# Linear = fitting a straight line
# Regression = fancy/stats way of saying quantifying a relationship between variables

### approximate synonyms [will try to use first one for consistency]
# outcome variable == outcome == response variable == dependent variable
# predictor variable == predictor == independent variable
# parameter estimates == coefficients (which are the intercept/slope)

### simple linear regression models are sometimes presented like this
# y_i = β_0 + β_1 x_i + e_i
# or
# y_i = b0 + b1 x_i + e_i

### remind you of early high school maths?
# y = m * x + c  
# (just an equation of a straight line)

### here: 
# y is outcome (in dataset), 
# x is predictor (in dataset),
# m = b1, (slope) [model estimates this from dataset]
# c = b0, (intercept) [model estimates this from dataset]

### BUT NOTE all of these characters/letters are just place holders for whatever
### variables we are interested in, e.g.,
# BMI              = (intercept) + (slope) * exercise_in_hours,    or
# blood_pressure   = (intercept) + (slope) * dietary_salt_intake,  or
# depression_score = (intercept) + (slope) * exercise_in_hours

### to fit all of the models specified above, the syntax for R is simple:
# lm(y ~ x, data = data_containing_xy)
# [where y is response/outcome and x is predictor/independent variable]

# the above regression model estimates the (intercept) and (slope) of
# y = (intercept) + (slope) * x
#
# <also the e_i from before, but let's keep it simple>

### let's get into the examples and we can revisit the above


# ---- libs ----

library("dplyr")
library("ggplot2")

# install.packages("betareg")
library("betareg") # using this for the "StressAnxiety" dataset

### package for assumption checking (thanks to Caitlin for finding this)
# install.packages("performance")
library("performance") 
### will be using check_model() function from the performance package

# ---- example_data_1 ----

# load data
data("StressAnxiety", package = "betareg")
as_tibble(StressAnxiety)


stress_dat <- # create a copy we can play with (and changing units)
  as_tibble(StressAnxiety) %>%
  # makes percentages of Depression Anxiety Stress Scales
  mutate(anxiety = 100 * anxiety, stress = 100 * stress) 

stress_dat

### details from ?StressAnxiety
# Stress and anxiety among nonclinical women in Townsville, Queensland, Australia
# 166 observations on 2 variables
# ---
# Smithson, M., and Verkuilen, J. (2006). A Better Lemon Squeezer? 
# Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
# Psychological Methods, 11(7), 54–71.

# ---- explore_data ----


# before doing any analysis -- always explore the data!!
# all models have assumptions, knowing what the data look like is a sanity check

# summary stats:
summary(stress_dat)

stress_dat %>%
  summarise(
    mean_stress = mean(stress),
    sd_stress = sd(stress),
    mean_anxiety = mean(anxiety),
    sd_anxiety = sd(anxiety)
  )


# what about a plot?

stress_dat %>%
  ggplot(., aes(x = stress, y = anxiety)) +
  geom_point(alpha = 0.5) +
  theme_bw()

(jitter_plot <-
  stress_dat %>%
  ggplot(., aes(x = stress, y = anxiety)) +
  geom_jitter(alpha = 0.5, width = 5, height = 0) +
  theme_bw())

?cor
cor(stress_dat)

ct1 <- cor.test(stress_dat$stress, stress_dat$anxiety)
str(ct1)
ct1$p.value
ct1$estimate



# ---- fit_lm ----

# linear model a good idea?

mod1 <- lm(anxiety ~ stress, data = stress_dat)
summary(mod1)
# Call:
# lm(formula = anxiety ~ stress, data = stress_dat)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -26.755  -5.502  -1.366   4.159  31.585 
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.64188    1.23626  -2.946  0.00369 ** 
# stress       0.48303    0.03776  12.791  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 9.404 on 164 degrees of freedom
# Multiple R-squared:  0.4994,	Adjusted R-squared:  0.4963 
# F-statistic: 163.6 on 1 and 164 DF,  p-value: < 2.2e-16


### some questions to explain the output:
# (1) Is `stress` a significant predictor of `anxiety`?
2e-16 < 0.05

# (2) If `stress` increases, does the model predict `anxiety` will increase or 
#   decrease (on average)?
# increase!

# (3) what is the estimated slope (b1 or m) coefficient?
0.48

# (4) What is the estimated intercept (b0 or c) coefficient?
-3.41

# (5) For every 1 unit increase in `stress`, 
#   what is the estimated increase/decrease in `anxiety`?
0.48

# (6) What is the estimated regression line/formula?

# anxiety =  intercept + slope * stress
# anxiety =  -3.64188 + 0.48303 * stress

jitter_plot +
  geom_abline(intercept = -3.64188, slope = 0.48303, col = "dodgerblue")

# (7) What is the interpretation of the estimated intercept coefficient?

# anxiety =  -3.64188 + 0.48303 * stress
# if stress == 0
# then anxiety =  -3.64188 + 0.48303 * 0 = -3.64188 + 0 = -3.64188

# (8) What is the predicted `anxiety` for a `stress` value of 35?

# can use model equation:
(anxiety_prediction =  -3.64188 + 0.48303 * (35))

# or use function that does the hard work for you
predict(mod1, newdata = data.frame(stress = 35))

# (8a) sub-question: does the prediction agree with the plot of the regression line?

# go up from 35 for stress (x-axis) and see the corresponding approx 13 value (y-axis)
TRUE 

# (9) What is the model R^2?
0.4994



# ---- check_assumptions ----


### checking the model assumptions:
# 1. Independence of observations
# 2. Linearity: look at residuals vs fitted
# 3. Homoscedasticity: look at scale-location
# 4. Normality: look at Q-Q plot

par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))

# alternatively: use the performance package
# much prettier with colours and gives hints for what you are looking for
check_model(mod1) 

### NOTE: I have saved the output of these diagnositic plots to the directory:
# 2022-08-24_linear_regression/fig/

# ---- not_covered ----


# what if the outcome variable isn't continuous?
# what if one of the predictors is not continuous (i.e., discrete)?
# is it sensible to say x affects y (does y affect x, or even are they just correlated?)
# model selection (remove or add terms) or comparing competing models
# testing multiple variables at once (anova() function)
# observational data vs experimental (and "covariates")
# repeated measures




# ---- multiple_linear_regression ----

# it's the same idea but just more predictor variables (and thus coefficients)
# y_i = β_0 + β_1 x_ {1i} + β_2 x_{2i} + ... + β_p x_{pi} + e_i

# adding another variable (fake but for illustration later on)
set.seed(1234567)
stress_dat <-
  stress_dat %>%
  mutate(
    income = rchisq(n = n(), df = 50 / (anxiety / 100 + 1 + runif(n())))
  )

mod2 <- lm(anxiety ~ stress + income, data = stress_dat)
summary(mod2)

stress_dat %>%
  ggplot(., aes(x = stress, y = anxiety, col = income)) +
  geom_jitter(alpha = 0.75, width = 5, height = 0, size = 2.5) +
  scale_color_distiller(palette = "Spectral") + 
  theme_bw()

### checking the model assumptions is the same:
# 1. Independence of observations
# 2. Linearity: look at residuals vs fitted 
#    (technically should check residuals vs each predictor variable too)
# 3. Homoscedasticity: look at scale-location
# 4. Normality: look at Q-Q plot

check_model(mod2) 

# alternatively using base R:
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))





