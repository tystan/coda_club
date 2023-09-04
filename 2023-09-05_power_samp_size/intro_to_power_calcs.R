
# ---- libs ----

# install.packages("pwr")
library("pwr")
?`pwr-package` # package help file landing page

# ---- hA_true_example ----





npg <- 20
delta <- 3
sigma <- 5
d <- delta / sigma # "effect-size"



# we want to check whether people in Adelaide aged 20-29 spend more (or different) 
# amount of time in physical activity than people in Adelaide aged 30-39

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


# so to check our above question we could do a t-test:
boxplot(pa ~ age_cat, data = pa_dat)
t.test(x = pa_age20s, y = pa_age30s, var.equal = TRUE)
t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)
str(t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE))
t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value
t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value < 0.05



pwr.t.test(
  n = npg, 
  d = delta / sigma, 
  sig.level = 0.05, 
  power = NULL, 
  type = "two.sample",
  alternative = "two.sided"
)


reps <- 10
(is_sig <- rep(NA, reps))
# is_sig[3] <- "fdglskjhbgdf"
# is_sig
for (i in 1:reps) { # i <- 
  pa_dat <-
    tibble(
      age_cat = rep(c("grp20", "grp30"), each = npg),
      pa = c(round(rnorm(npg, 25, sigma)), round(rnorm(npg, 25 - delta, sigma)))
    )
  is_sig[i] <- t.test(pa ~ age_cat, data = pa_dat, var.equal = TRUE)$p.value < 0.05
}
is_sig
as.integer(is_sig)
sum(as.integer(is_sig))
sum(as.integer(is_sig)) / reps



plot(
  1:reps, 
  cumsum(is_sig) / cumsum(rep(1, reps)), 
  type = ifelse(reps <= 100, "p", "l"), 
  ylab = "power estimate",
  xlab = "Number of simulations"
)
abline(h = 0.4560341, col = "orange")


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





