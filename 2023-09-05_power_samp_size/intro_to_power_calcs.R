
# ---- libs ----

# install.packages("pwr")
library("pwr")
library("dplyr")
?`pwr-package` # package help file landing page



# ---- hA_true_example ----



npg <- 20
delta <- 3
sigma <- 5
d <- delta / sigma # "effect-size"


# we want to check whether people in Adelaide aged 20-29 spend more (or different) 
# amount of time in physical activity than people in Adelaide aged 30-39

# a power calc for a fixed number of people
(power_result <- pwr.t.test(
  n = npg, 
  d = delta / sigma, 
  sig.level = 0.05, 
  power = NULL, 
  type = "two.sample",
  alternative = "two.sided"
))


# a fixed power to find required n (per group)
pwr.t.test(
  n = NULL, 
  d = delta / sigma, 
  sig.level = 0.05, 
  power = 0.95, 
  type = "two.sample",
  alternative = "two.sided"
)



# so we randomly sample 20 people from each age group and get the following
# 7-day average daily physical activity times

(pa_age20s <- round(rnorm(npg, 25, sigma)))
(pa_age30s <- round(rnorm(npg, 25 - delta, sigma)))
pa_dat <-
  tibble(
    age_cat = rep(c("grp20", "grp30"), each = npg),
    pa = c(pa_age20s, pa_age30s)
  )
pa_dat


### this side of the divider we don't have access to in our studies
### (the above is just a thought experiment about how our data might
### "come to be")


# -------------------------------------------------------------------
# -------------------------------------------------------------------


### this side of the divider is the data we collect to in our studies
### (and we don't know if either H_0 or H_A is the truth)

# so to check our above question we could do a t-test:
boxplot(pa ~ age_cat, data = pa_dat)

t.test(x = pa_age20s, y = pa_age30s, var.equal = TRUE)
t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)
str(t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE))


t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value
t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value < 0.05


### let's repeat the above lots and lots to see how often we reach "significance" as a proportion

reps <- 1000
(is_sig <- rep(NA, reps))
# is_sig[3] <- "test" # inserting a value in the third spot in vector for demonstration
# is_sig
for (i in 1:reps) { # i <- 1
  # create new randomly generated dataset
  pa_dat <-
    tibble(
      age_cat = rep(c("grp20", "grp30"), each = npg),
      pa = c(rnorm(npg, 25, sigma), rnorm(npg, 25 - delta, sigma))
    )
  # is the t-test significant -- do we reject H0?
  ### remember: H_A is true so we DO want significance (i.e. reject H_A)
  is_sig[i] <- t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value < 0.05
}
# is_sig
# as.integer(is_sig)
sum(as.integer(is_sig))
sum(as.integer(is_sig)) / reps



plot(
  1:reps, 
  cumsum(is_sig) / cumsum(rep(1, reps)), 
  type = ifelse(reps <= 100, "p", "l"), 
  ylab = "power estimate",
  xlab = "Number of simulations"
)
abline(h = power_result$power, col = "orange") # orange line is pwr.t.test() power result



# ---- h0_true_example ----


sigma <- 5
n <- 20


# we want to check whether people in Adelaide aged 20-29 spend more (or different) 
# amount of time in physical activity than people in Adelaide aged 30-39

# so we randomly sample 20 people from each age group and get the following
# 7-day average daily physical activity times
(pa_age20s <- round(rnorm(npg, 25, sigma)))
(pa_age30s <- round(rnorm(npg, 25, sigma))) # same mean here
pa_dat <-
  tibble(
    age_cat = rep(c("grp20", "grp30"), each = npg),
    pa = c(pa_age20s, pa_age30s)
  )
pa_dat


# so to check our above question we could do a t-test:
boxplot(pa ~ age_cat, data = pa_dat)
t.test(x = pa_age20s, y = pa_age30s, var.equal = TRUE)


reps <- 100
(is_sig <- rep(NA, reps))

for (i in 1:reps) { 
  # create new randomly generated dataset
  pa_dat <-
    tibble(
      age_cat = rep(c("grp20", "grp30"), each = npg),
      pa = c(rnorm(npg, 25, sigma), rnorm(npg, 25, sigma))
    )
  # is the t-test significant -- do we reject H0?
  ### remember: H_0 is true so we DO NOT want significance
  is_sig[i] <- t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value < 0.05
}
# is_sig
# as.integer(is_sig)
sum(as.integer(is_sig))
sum(as.integer(is_sig)) / reps # should be close to alpha = 0.05


