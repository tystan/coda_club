
# ---- libs ----

library("dplyr")
library("ggplot2")
library("readr")
library("compositions") # for creating isometric log ratios further down in script
library("performance") # package for model assumption checking
library("GGally") # plotting pairwise scatterplots of variables



# ---- load ----

### NOTE: this is the file Dot provided (not on Github)
fairclough <- read_csv("2022-08-24_linear_regression/dat/Fairclough.csv")

# ---- coda ----

# create a copy of the data and simplify names
fc <- 
  fairclough %>%
  rename(
    bmi = BMI,
    sed = Sedentary,
    lpa = Light,
    mvpa = MVPA 
  )
fc

# these are the compositional variables
comp_parts <- fc[, c("sleep", "sed", "lpa", "mvpa")]
head(comp_parts) # have a peak
hist(rowSums(comp_parts)) # check they add up to 1440 minutes in the day

# not all rows add up to 1440, so will proportionally adjust to make 1400
# (ignore the code, don't need to understand)
standardised_comp_parts <- 
  1440 * t(apply(comp_parts, 1, function(x) x / sum(x)))

# check old and new values
cbind(comp_parts, standardised_comp_parts)

# update dataset with standardised 1440 per row
fc[, c("sleep", "sed", "lpa", "mvpa")] <- standardised_comp_parts



# ---- explore_data ----


# before doing any analysis -- always explore the data!!
# all models have assumptions, knowing what the data look like is a sanity check

# summary stats:
summary(fc)

ggpairs(fc[, c("sleep", "sed", "lpa", "mvpa", "bmi")]) +
  theme_bw()



# ---- attempt_at_model ----

# now fit a model for the outcome of BMI using the compositional variables
fc %>% select(bmi, sleep, sed, lpa, mvpa)
fc_mod <- lm(bmi ~ sleep + sed + lpa + mvpa, data = fc)
summary(fc_mod) # why is there NAs in there?


# ---- workaround1 ----

# fit the model (substitution model) without "mvpa"
# how does this change the interpretation of the coefficients?
fc_mod2 <- lm(bmi ~ sleep + sed + lpa, data = fc)
summary(fc_mod2)

check_model(fc_mod2) 



# fit the model (substitution model) without "sleep"
fc_mod3 <- lm(bmi ~ sed + lpa + mvpa, data = fc)
summary(fc_mod3)

check_model(fc_mod3) 

# ---- workaround2 ----

# use isometric log-ratios
fc_ilrs <-
  fc %>%
  select(bmi, sleep, sed, lpa, mvpa)

# this is creating an ilr basis
V <-
  t(matrix(
    c(
      +1, -1, -1, -1,
       0, +1, -1, -1,
       0,  0, +1, -1
    ),
    byrow = TRUE,
    nrow = 3
  ))
psi <- compositions::gsi.buildilrBase(V)

# create ilrs using the compositions package (and psi above)
acomp_vals <- acomp(fc_ilrs[, c("sleep", "sed", "lpa", "mvpa")], total = 1)
ilr_vals <- ilr(acomp_vals, V = psi)

# update column names of ilrs
colnames(ilr_vals) <- paste0(".", 1:3)

# add on ilrs to data.frame
fc_ilrs$ilr <- ilr_vals
# do the ilrs make sense compared to the original composition vars?
fc_ilrs

# now fit an ilr model: bmi ~ ilr.1 + ilr.2 + ilr.3
fc_mod4 <- lm(bmi ~ ilr, data = fc_ilrs)
summary(fc_mod4)
anova(fc_mod4)

check_model(fc_mod4) 


