

# ---- preamble -----

### Multiple Linear Regression
# Breakdown:
# Multiple = many (predictor) variables
# Linear = fitting a straight line
# Regression = fancy/stats way of saying quantifying a relationship between variables

# it's the same idea but just more predictor variables (and thus coefficients)
# y_i = β_0 + β_1 x_ {1i} + β_2 x_{2i} + ... + β_p x_{pi} + e_i

# or in simpler but less correct terms
# y = b0 + b1 * x1 + b2 * x2 + ... + bp * xp 

### here: 
# y is outcome (in dataset), 
# x1 is predictor 1 (in dataset),
# x2 is predictor 2 (in dataset),
# ...
# xp is predictor p (in dataset),
# b0, (intercept) [model estimates this from dataset]
# b1, (coefficient of x1) [model estimates this from dataset]
# b2, (coefficient of x2) [model estimates this from dataset]
# ...
# bp, (coefficient of xp) [model estimates this from dataset]

### BUT NOTE all of these characters/letters are just place holders for whatever
### variables we are interested in, e.g.,
# BMI = (b0) + (b1) * exercise_in_hours + (b2) * depression_score 

### to fit the model specified above, the syntax for R is simple:
# lm(y ~ x1 + x2 + x3 + x4, data = data_containing_xy)
# [where y is response/outcome and x1, x2, x3, x4 are predictor/independent variables]


# ---- libs ----

library("dplyr")
library("ggplot2")

# install.packages("betareg")
library("betareg") # using this for the "StressAnxiety" dataset

### package for assumption checking (thanks to Caitlin for finding this)
# install.packages("performance")
library("performance") 
### will be using check_model() function from the performance package

# install.packages("GGally")
library("GGally") # plotting pairwise scatterplots of variables


# ---- example_data_1 ----

# load data
data("StressAnxiety", package = "betareg")
?StressAnxiety

stress_dat <- # create a copy we can play with (and changing units)
  as_tibble(StressAnxiety) %>%
  # makes percentages of Depression Anxiety Stress Scales
  mutate(anxiety = 100 * anxiety, stress = 100 * stress) 


### details from ?StressAnxiety
# Stress and anxiety among nonclinical women in Townsville, Queensland, Australia
# 166 observations on 2 variables
# ---
# Smithson, M., and Verkuilen, J. (2006). A Better Lemon Squeezer? 
# Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
# Psychological Methods, 11(7), 54–71.


# adding another variable (fake but for illustration later on)
set.seed(123456)
stress_dat <-
  stress_dat %>%
  mutate(
    income = rchisq(n = n(), df = 50 / (anxiety / 100 + 1 + runif(n()))),
    sex = sample(c("F", "M"), n(), prob = c(0.6, 0.4), replace = TRUE)
  )

stress_dat

# note about factors/categorical variables:
head(stress_dat[, "sex"])
head(model.matrix(~ 0 + sex, data = stress_dat))

stress_dat[["sex"]]
class(stress_dat[["sex"]])
# stress_dat[["sex"]] <- factor(stress_dat[["sex"]])
stress_dat[["sex"]]
class(stress_dat[["sex"]])


# stress_dat[["sex"]] <- relevel(stress_dat[["sex"]], ref = "M")
stress_dat[["sex"]]
class(stress_dat[["sex"]])

stress_dat[["sex"]]

str(stress_dat)



# ---- explore_data ----


# before doing any analysis -- always explore the data!!
# all models have assumptions, knowing what the data look like is a sanity check

# summary stats:
summary(stress_dat)


# what about a plot?
stress_dat %>%
  ggplot(., aes(y = anxiety, x = stress, col = income, shape = sex)) +
  geom_jitter(alpha = 0.75, width = 5, height = 0, size = 2.5) +
  scale_color_distiller(palette = "Spectral") + 
  theme_bw()


ggpairs(stress_dat[, c("stress", "income", "sex", "anxiety")]) +
  theme_bw()

# ---- fit_simple_lms ----

# before *multiple* linear regression...
# we can try "simple" (univariate) regression models

summary(lm(anxiety ~ stress, data = stress_dat))
summary(lm(anxiety ~ income, data = stress_dat))
summary(lm(anxiety ~ sex, data = stress_dat))


check_model(lm(anxiety ~ stress, data = stress_dat)) 

# ---- multiple_linear_regression ----


# fit a multiple linear regression
mod1 <- lm(anxiety ~ stress + income + sex, data = stress_dat)
summary(mod1)
# Call:
# lm(formula = anxiety ~ stress + income + sex, data = stress_dat)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -25.2129  -5.9190  -0.0909   4.1003  28.5165 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.06536    2.73973   0.389   0.6979    
# stress       0.46120    0.03788  12.176   <2e-16 ***
# income      -0.15224    0.06607  -2.304   0.0225 *  
# sexM         2.31409    1.46173   1.583   0.1153    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 9.236 on 162 degrees of freedom
# Multiple R-squared:  0.5231,	Adjusted R-squared:  0.5142 
# F-statistic: 59.22 on 3 and 162 DF,  p-value: < 2.2e-16


### some questions to explain the output:
# (1) What is the estimated regression line/formula?

# anxiety = intercept + stress_coef * stress + income_coef * income + sex_coef * I(sex == M)
# anxiety = 1.06536 + 0.46120 * stress + -0.15224 * income + 2.31409 * I(sex == M)

# (2) What is the estimated regression line/formula for women?

# anxiety = 
#   1.06536 + 0.46120 * stress + -0.15224 * income + 2.31409 * 0 =
#   1.06536 + 0.46120 * stress + -0.15224 * income

# (3) What is the estimated regression line/formula for men?

# anxiety = 
#   1.06536 + 0.46120 * stress + -0.15224 * income + 2.31409 * 1 =
#   1.06536 + 0.46120 * stress + -0.15224 * income + 2.31409 =
#   (2.31409 + 1.06536) + 0.46120 * stress + -0.15224 * income =
#   3.37945 + 0.46120 * stress + -0.15224 * income 

# (4) according to the regression formula above, all other things being equal,
#     do men or women have higher predicted anxiety on average?

# men as coef is positive

# (5) What is the interpretation of the estimated intercept coefficient?

# the model predicted mean anxiety for not-male, $0k income, 0 stress persons

# (6) Is `income` a significant predictor of `anxiety`?

0.0225 < 0.05 # TRUE

# (7) If `income` increases, does the model predict `anxiety` will increase or 
#   decrease (on average)?

# decrease as `income` coef is negative (-0.15224)

# (8) what is the estimated coefficient of `income`?

-0.15224

# (9) For every 1 unit increase in `income`, 
#   what is the estimated increase/decrease in `anxiety`?

-0.15224

# (10) What is the predicted `anxiety` for a women with stress=35 and income=16?

# anxiety = 
  # 1.06536 + 0.46120 * (35) + -0.15224 * (16) + 2.31409 * (0) =
  14.77152

# (10(b)) same but male?

# anxiety = 
  # 1.06536 + 0.46120 * (35) + -0.15224 * (16) + 2.31409 * (1) =
  17.08561

# (11) What is the model R^2?

0.5231

# ---- check_assumptions ----

### checking the model assumptions is the same:
# 1. Independence of observations
# 2. Linearity: look at residuals vs fitted 
#    (technically should check residuals vs each predictor variable too)
# 3. Homoscedasticity: look at scale-location
# 4. Normality: look at Q-Q plot

check_model(mod1) 








