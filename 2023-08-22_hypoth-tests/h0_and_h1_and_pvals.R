
# ---- libs ----

library("dplyr")
library("ggplot2")


# ---- example ----

# we want to check whether people in Adelaide aged 20-29 spend more (or different) 
# amount of time in physical activity than people in Adelaide aged 30-39

# so we randomly sample 20 people from each age group and get the following
# 7-day average daily physical activity times
set.seed(1234)
(pa_age20s <- round(rnorm(20, 25, 5)))
(pa_age30s <- round(rnorm(20, 25, 3)))

# so to check our above question we could do a t-test:
t.test(x = pa_age20s, y = pa_age30s, var.equal = TRUE)


### Questions
# what are the null and alternative hypotheses?
# what is the test statistic?
# what is the distribution of the test statistic _if_ the null hypothesis is true?
# what is the p-value?
# what does the p-value represent?
# what do we conclude from this fake data analysis?
# are there any assumptions associated with the t-test? the p-value?



# ---- preamble ----

### the below is a 10,000 foot view and will have caveats not touched on...

"hypothesis testing is the backbone of statistics" 
# and is an extension of the scientific method in some senses:
# we assume "nothing is going on" until evidence presents itself to say otherwise
#
# (there are infinite things that COULD be true [and likely not mutually compatible]
# but the set of things that ARE true is a very, very, very small subset of the 
# "COULD" so the optimal process of establishing "ARE" true is:
#     to build up/add to the "ARE" true when we have sufficient evidence)
# 
# e.g., we need to have evidence to:
#  - show paracetamol is an effective analgesic 
#  - demonstrate an intervention increases active time in adults 
# 
# put in a statistical framework we 
#    0. construct a null hypothesis (labelled H0) as the "nothing going on" possibility
#    1. construct a alternative hypothesis (labelled H1 or HA) as the "something going on" possibility
# 
# NB:
#   (a) we assume H0 unless we have evidence to go with H1
#   (b) all possibilities should be covered by H0 and H1
#   (c) statistical hypothesis testing is normally about underlying population
#       means (that is, we are making inference about _unknown_ quantities based on a 
#       subset of the population a.k.a. the sample) - hypothesis tests won't be about 
#       individuals in the sample or population
#   (d) to differentiate between unknown population means and sampled/estimated means,
#       Greek letters (e.g. mu, beta, ...) are used for the former and Greek letters WITH
#       a hat on top or modern English letters with a bar are used (e.g. mu with a hat, 
#       beta with a hat, x with a bar)
# 
# Now we have given the rough idea, let's think about how to create H0 and H1 in the above
# examples

"show paracetamol is an effective analgesic we need to have evidence"
# H0: the (underlying) mean pain relief for people using paracetamol is equal to the 
#     (underlying) mean pain relief for people using a placebo pill
#                                  v.
# H1: the (underlying) mean pain relief for people using paracetamol is NOT equal to the 
#     (underlying) mean pain relief for people using a placebo pill
# 
# REWRITTEN:
# H0: mu1 = mu2            v.     H1: mu1 =/= mu2
# H0: mu1 - mu2 = 0        v.     H1: mu1 - mu2 =/= 0
#      where mu1 is the (underlying population) mean pain relief for people using paracetamol
#            mu2 is the (underlying population) mean pain relief for people using a placebo pill
#
# so finally we can "test" these hypotheses by creating sample means from these two
# populations and decide on the probability H0 being true (in a very rough sense)

"all p-values are associated with a null hypothesis"
# the above is where p-values come in
# the p-value is generated from the data (under the assumptions of the test), and represents:
"p-value = the probability of obtaining the observed test statistic (or more extreme) if H0 is true"


# "extraordinary claims require extraordinary evidence"


# ---- more_examples ----

pa_dat <-
  tibble(
    age_cat = rep(c("grp20", "grp30"), each = 20),
    pa = c(pa_age20s, pa_age30s)
  )
pa_dat

pa_dat$age_cat <- factor(pa_dat$age_cat)
pa_dat$age_cat <- relevel(pa_dat$age_cat, ref = "grp30")


pa_dat %>%
  ggplot(aes(x = age_cat, y = pa, col = age_cat)) +
  geom_jitter(width = 0.1) +
  theme_bw()


# pa = beta0 + beta1 * (age_cat == "30") + error
summary(lm(pa ~ age_cat, data = pa_dat))

# H0: beta1  = 0 (= mu1 - mu2)     v.     H1: beta1 =/= 0

### NB the above hypothesis test is equivalent to the t-test:
# H0: mu1 = mu2            v.     H1: mu1 =/= mu2
# t.test(x = pa_age20s, y = pa_age30s, var.equal = TRUE)


# ---- testing_more_than_2_means ----

# add another age group
set.seed(5678)
(pa_age40s <- round(rnorm(20, 21, 5)))

pa_dat <-
  tibble(
    age_cat = rep(c("grp20", "grp30", "grp40"), each = 20),
    pa = c(pa_age20s, pa_age30s, pa_age40s)
  )
head(pa_dat)
tail(pa_dat)

pa_dat %>%
  ggplot(aes(x = age_cat, y = pa, col = age_cat)) +
  geom_jitter(width = 0.1) +
  theme_bw()

### Q: what are the null hypotheses of the p-values seen in the output below
# note the model being fit is:
# pa = beta0 + beta1 * (age_cat == "30") + beta2 * (age_cat == "40") + error
threemean_lm <- lm(pa ~ age_cat, data = pa_dat)
summary(threemean_lm)
anova(threemean_lm)
summary(aov(pa ~ age_cat, data = pa_dat))


### now change the reference level to oldest group
pa_dat$age_cat <- factor(pa_dat$age_cat)
pa_dat$age_cat <- relevel(pa_dat$age_cat, ref = "grp40")

### Q: what are the null hypotheses of the p-values seen in the output below
# note the model being fit is:
# pa = beta0 + beta1 * (age_cat == "20") + beta2 * (age_cat == "30") + error
threemean_lm_v2 <- lm(pa ~ age_cat, data = pa_dat)
summary(threemean_lm_v2)
anova(threemean_lm_v2)
summary(aov(pa ~ age_cat, data = pa_dat))




