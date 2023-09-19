


# ---- pls_ignore ----

# just for nicer plotting of colours
add_alpha <- function(col, alpha = 1) {
  apply(
    sapply(col, col2rgb) / 255,
    2, # by column
    function(x) rgb(x[1], x[2], x[3], alpha = alpha)
  )
}

#make colour scheme over numeric vector
col_scheme_for_numeric_vector <- function(vec, alpha = 0.4) {
  intervals <- seq(min(vec), max(vec), length = 11)
  ints <- findInterval(vec, intervals, all.inside = TRUE)
  gradient <- colorRampPalette(colors = rev(c("red", "blue")))
  colours <- gradient(length(intervals))
  outc_y_col <- add_alpha(colours[ints], alpha)
  return(outc_y_col)
}

# testing
n <- 100
x <- rnorm(n)
plot(
  1:n, x, 
  col = col_scheme_for_numeric_vector(x), 
  pch = 16, cex = 5, bty = "n", axes = FALSE, xlab = "", ylab = ""
)


row_wise_closure <- function(y, clo_val = 1) {
  clo_val * t(apply(y, 1, function(x) x / sum(x)))
}


# ---- libs ----

library("dplyr")        # tidyverse packages
library("ggplot2")
library("readr")

library("codaredistlm") # for the fairclough data
library("compositions") # for creating isometric log ratios further down in script
library("performance")  # package for model assumption checking
library("GGally")       # plotting pairwise scatterplots of variables



# ---- load ----

# this loads the data "fairclough" so it can be used
data("fairclough", package = "codaredistlm")
?fairclough
head(fairclough)
colnames(fairclough)

# ---- aims ----

# To regress bmi on time-use compositions and covariates
#  + model diagnostics
#  + model predictions

### ALSO:
# pivot ilrs
# poly terms
# alternative ilr bases
# naive models


# ---- coda ----

# create a copy of the data (in case we need the original) and simplify names
fc <- fairclough 

# these are the compositional variables
comp_parts <- fc[, c("sleep", "sed", "lpa", "mvpa")]
head(comp_parts) # have a peak
rowSums(comp_parts)
hist(rowSums(comp_parts)) # check they add up to 1440 minutes in the day

# not all rows add up to 1440, so will proportionally adjust to make 1400
# (ignore the code, don't need to understand)
standardised_comp_parts <- row_wise_closure(comp_parts, clo_val = 1440)


# check old and new values
cbind(comp_parts, standardised_comp_parts)

# update dataset with standardised 1440 per row
fc[, c("sleep", "sed", "lpa", "mvpa")] <- standardised_comp_parts



# ---- explore_data ----


# before doing any analysis -- always explore the data!!
# all models have assumptions, knowing what the data look like is a sanity check

# summary stats:
summary(fc)

# compositions and bmi
ggpairs(fc[, c("sleep", "sed", "lpa", "mvpa", "bmi")]) +
  theme_bw()


plot(
  acomp(fc[, c("sleep", "sed", "lpa", "mvpa")]), 
  col = col_scheme_for_numeric_vector(fc$bmi, 0.2), 
  pch = 16
)



# potential covariates and bmi
ggpairs(fc[, c("sex", "decimal_age", "imd_decile", "shuttles_20m", "bmi")]) +
  theme_bw()

# make sure categories are coded as factors
table(fc$sex, useNA = "ifany")
table(fc$imd_decile, useNA = "ifany")

# sex and imd as factors
fc <- 
  fc %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    imd_tri = factor(if_else(imd_decile > 3, 3, imd_decile))
  )

table(fc$sex, useNA = "ifany")
table(fc$imd_tri, useNA = "ifany")

# potential covariates and bmi now variables are factors
ggpairs(fc[, c("sex", "decimal_age", "imd_tri", "shuttles_20m", "bmi")]) +
  theme_bw()




# ---- bmi_regress ----

outc <- "bmi"
comp_parts <- c("sleep", "sed", "lpa", "mvpa")
covs <- c("sex", "decimal_age", "imd_tri", "shuttles_20m")
D <- length(comp_parts)

# limit data to columns we want
fc <-
  fc %>%
  select(all_of(c(outc, comp_parts, covs)))

head(fc)

# isometric log-ratio time!
ilr_nms <- paste0("ilr", 1:(D - 1))

# this is creating an ilr basis using a "sequential binary partition"
sbp_mat <-
  matrix(c(
      +1,  0,  0,
      -1, +1,  0,
      -1, -1, +1,
      -1, -1, -1),
      ### balance coordinates
      # +1, +1,  0,
      # +1, -1,  0,
      # -1,  0, +1,
      # -1,  0, -1),
    byrow = TRUE,
    ncol = 3,
    dimnames = list(comp_parts, ilr_nms)
  )
sbp_mat

# turn seq binary partition matrix to ilr basis matrix
V <- compositions::gsi.buildilrBase(sbp_mat)
V # peak

### default basis
ilrBase(D = 4)

### fun properties
# sum(V[, 1] ^ 2)      # unit length
# sum(V[, 1] * V[, 2]) # orthogonal

# create ilrs using the compositions package (and V above)
ilr_vals <- ilr(fc[, comp_parts], V = V)
head(ilr_vals)

# check we agree with the above values
cmp <- fc[, comp_parts]
ilr1_check <- sqrt(3 / 4) * log(cmp[, 1] / ((cmp[, 2] * cmp[, 3] * cmp[, 4]) ^ (1/3)))
ilr2_check <- sqrt(2 / 3) * log(cmp[, 2] / ((cmp[, 3] * cmp[, 4]) ^ (1/2)))
ilr3_check <- sqrt(1 / 2) * log(cmp[, 3] / cmp[, 4])

# check agreement
head(cbind(ilr_vals$ilr1, ilr1_check))

     
# add on ilrs to data.frame
fc$ilr <- ilr_vals
fc <- cbind(fc, ilr_vals) # optional not nested version

# do the ilrs make sense compared to the original composition vars?
head(fc)
colnames(fc)
head(fc)

colnames(fc$ilr)
fc$ilr
fc$ilr.1
fc$ilr[, 1]
# compositions and bmi
ggpairs(cbind(as.data.frame(ilr_vals), bmi = fc$bmi)) +
  theme_bw()



# now fit an ilr model: bmi ~ ilr.1 + ilr.2 + ilr.3
fc_mod_0 <- lm(bmi ~ ilr, data = fc)
fc_mod_0 <- lm(bmi ~ cbind(ilr1, ilr2, ilr3), data = fc)
anova(fc_mod_0)
summary(fc_mod_0)

check_model(fc_mod_0) 

# now fit an ilr model: bmi ~ covariates + ilr.1 + ilr.2 + ilr.3 
fc_mod_1 <- 
  lm(
    bmi ~ 
      sex + decimal_age + imd_tri + shuttles_20m + ilr, 
    data = fc
  )
anova(fc_mod_1) # ordering of terms matters
car::Anova(fc_mod_1, type = "II") # ordering of terms do not matter
summary(fc_mod_1)
AIC(fc_mod_1) # ?AIC
BIC(fc_mod_1)

check_model(fc_mod_1) 

# check linearity of predictors
par(mfrow = c(2, 2))
plot(fc$ilr[, 1], residuals(fc_mod_1)); abline(h = 0)
plot(fc$ilr[, 2], residuals(fc_mod_1)); abline(h = 0)
plot(fc$ilr[, 3], residuals(fc_mod_1)); abline(h = 0)
plot(fc$shuttles_20m, residuals(fc_mod_1)); abline(h = 0)
par(mfrow = c(1, 1))


### predictions

fc <-
  fc %>%
  mutate(pred_bmi = predict(fc_mod_1)) %>%
  select(bmi, pred_bmi, everything())

head(fc)


### time-use (isotemporal) substitution predictions

# proportional reallocation
preds_df_prop <- 
  predict_delta_comps(
    dataf = fc,
    y = "bmi",
    comps = c("sleep", "sed", "lpa", "mvpa"),
    covars = c("sex", "decimal_age", "imd_tri", "shuttles_20m"),
    deltas = seq(-20, 20, by = 10) / (24 * 60),
    comparisons = "prop-realloc",
    alpha = 0.05
  )

plot_delta_comp(
  dc_obj = preds_df_prop,
  comp_total = 24 * 60,
  units_lab = "min"
)

# one-for-one reallocation
preds_df_prop <- 
  predict_delta_comps(
    dataf = fc,
    y = "bmi",
    comps = c("sleep", "sed", "lpa", "mvpa"),
    covars = c("sex", "decimal_age", "imd_tri", "shuttles_20m"),
    deltas = seq(-20, 20, by = 10) / (24 * 60),
    comparisons = "one-v-one",
    alpha = 0.05
  )

plot_delta_comp(
  dc_obj = preds_df_prop,
  comp_total = 24 * 60,
  units_lab = "min"
)


# ---- manova ----

# this is a statistical test of whether the average time-use 
# compositions differ between sexes

multivar_outc_mod <- 
  manova(
    cbind(ilr[, 1], ilr[, 2], ilr[, 3]) ~ sex, 
    data = fc
  )
summary(multivar_outc_mod)

# compositional means

(ilr_f_mean <- colMeans(subset(fc$ilr, fc$sex == "Female")))
(comp_f_mean <- 1440 * unclass(ilrInv(ilr_f_mean, V = V)))

(ilr_m_mean <- colMeans(subset(fc$ilr, fc$sex == "Male")))
(comp_m_mean <- 1440 * unclass(ilrInv(ilr_m_mean, V = V)))



# ---- bmi_regress_pivot ----


# now make numerator MVPA for first ilr!
comp_parts <- c("mvpa", "sleep", "sed", "lpa")
outc <- "bmi"
covs <- c("sex", "decimal_age", "imd_tri", "shuttles_20m")
D <- length(comp_parts)


# isometric log-ratio time!
ilr_nms <- paste0("ilr", 1:(D - 1))

# this is creating an ilr basis
sbp_mat <-
  matrix(c(
    +1,  0,  0,
    -1, +1,  0,
    -1, -1, +1,
    -1, -1, -1),
    byrow = TRUE,
    ncol = 3,
    dimnames = list(comp_parts, ilr_nms)
  )
sbp_mat
V <- compositions::gsi.buildilrBase(sbp_mat)
V # peak

# create ilrs using the compositions package (and V above)
ilr_vals <- ilr(fc[, comp_parts], V = V)
head(ilr_vals)



# add on ilrs to data.frame
fc$ilr <- ilr_vals
# do the ilrs make sense compared to the original composition vars?
head(fc)

# compositions and bmi
ggpairs(cbind(as.data.frame(ilr_vals), bmi = fc$bmi)) +
  theme_bw()


# now fit an ilr model: bmi ~ covariates + ilr.1 + ilr.2 + ilr.3 
fc_mod_2 <- 
  lm(
    bmi ~ 
      sex + decimal_age + imd_tri + shuttles_20m + ilr, 
    data = fc
  )
anova(fc_mod_2) # ordering of terms matters
car::Anova(fc_mod_2, type = "II") # ordering of terms do not matter
summary(fc_mod_2)



# preds the same even though different ratios etc (pivot)
head(cbind(predict(fc_mod_1), predict(fc_mod_2)))



# ---- bmi_regress_poly_ilrs ----

outc <- "bmi"
comp_parts <- c("sleep", "sed", "lpa", "mvpa")
covs <- c("sex", "decimal_age", "imd_tri", "shuttles_20m")
D <- length(comp_parts)

# limit data to columns we want
fc <-
  fc %>%
  select(all_of(c(outc, comp_parts, covs)))

head(fc)

# isometric log-ratio time!
ilr_nms <- paste0("ilr", 1:(D - 1))

# this is creating an ilr basis
sbp_mat <-
  matrix(c(
    +1,  0,  0,
    -1, +1,  0,
    -1, -1, +1,
    -1, -1, -1),
    byrow = TRUE,
    ncol = 3,
    dimnames = list(comp_parts, ilr_nms)
  )
sbp_mat
V <- compositions::gsi.buildilrBase(sbp_mat)
V # peak

# create ilrs using the compositions package (and V above)
ilr_vals <- ilr(fc[, comp_parts], V = V)
head(ilr_vals)




# add on ilrs to data.frame
fc$ilr <- ilr_vals
# do the ilrs make sense compared to the original composition vars?
head(fc)




# now fit an ilr model: bmi ~ covariates + (ilr.1 + ilr.2 + ilr.3) ^ 2 
fc_mod_1_poly <- 
  lm(
    bmi ~ 
      sex + decimal_age + imd_tri + shuttles_20m + 
      poly(ilr, degree = 2), 
    data = fc
  )
anova(fc_mod_1_poly) # ordering of terms matters
car::Anova(fc_mod_1_poly, type = "II") # ordering of terms do not matter
summary(fc_mod_1_poly)

check_model(fc_mod_1_poly) 

# check linearity of predictors
par(mfrow = c(2, 2))
plot(fc$ilr[, 1], residuals(fc_mod_1_poly)); abline(h = 0)
plot(fc$ilr[, 2], residuals(fc_mod_1_poly)); abline(h = 0)
plot(fc$ilr[, 3], residuals(fc_mod_1_poly)); abline(h = 0)
plot(fc$shuttles_20m, residuals(fc_mod_1_poly)); abline(h = 0)
par(mfrow = c(1, 1))

# check shape of predictions with predictors
par(mfrow = c(2, 2))
plot(fc$ilr[, 1], predict(fc_mod_1_poly)); abline(h = 0)
plot(fc$ilr[, 2], predict(fc_mod_1_poly)); abline(h = 0)
plot(fc$ilr[, 3], predict(fc_mod_1_poly)); abline(h = 0)
plot(fc$shuttles_20m, predict(fc_mod_1_poly)); abline(h = 0)
par(mfrow = c(1, 1))

# now asking the question: are the quadratic terms significant?

anova(fc_mod_1, fc_mod_1_poly)

# same check just another way to do it
fc_mod_1_poly_null <- 
  update(
    fc_mod_1_poly, 
    . ~ . - poly(ilr, degree = 2) + poly(ilr, degree = 1)
  )

summary(fc_mod_1_poly_null)
anova(fc_mod_1_poly_null, fc_mod_1_poly)

# ---- naive_model_0 ----

# now fit a model for the outcome of BMI using the compositional variables
fc %>% select(bmi, sleep, sed, lpa, mvpa)
fc_mod <- lm(bmi ~ sleep + sed + lpa + mvpa, data = fc)
summary(fc_mod) # why is there NAs in there?
AIC(fc_mod)


fc_mod <- lm(bmi ~ mvpa + sleep + sed + lpa, data = fc)
summary(fc_mod) # why is there NAs in there?

# ---- naive_model_1 ----

# fit the model (substitution model) without "mvpa"
# how does this change the interpretation of the coefficients?
fc_mod_n1 <- lm(bmi ~ sleep + sed + lpa, data = fc)
summary(fc_mod_n1)

check_model(fc_mod_n1) 



# fit the model (substitution model) without "sleep"
fc_mod_n2 <- lm(bmi ~ sed + lpa + mvpa, data = fc)
summary(fc_mod_n2)

check_model(fc_mod_n2) 

cbind(predict(fc_mod_n1), predict(fc_mod_n2))









