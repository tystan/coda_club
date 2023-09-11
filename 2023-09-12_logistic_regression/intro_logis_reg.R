
# ---- preamble ----

### (simple) linear regression:

# y_i = b0 + b1 x_i + e_i

# where 
#    y_i are continuous values
#    e_i are normally distributed error/residuals


### (simple) logistic regression:

# log_odds(p_i) = b0 + b1 x_i 
# y_i ~ Bernoulli(p_i)

# where 
#    y_i are either 0 or 1
#    p_i is the underlying probability of y_i == 1
# and note
#    there is no e_i term! (the error is the diff between {0 or 1} and p_i)


# ---- libs ----

# install.packages("aod")

library("dplyr")
library("tidyr")
library("ggplot2")
library("ggthemes")

library("aod")


# ---- transform -----

logis <- function(p) log(p / (1 - p))
inv_logis <- function(x) exp(x) / (1 + exp(x))

p <- seq(0.01, 0.99, 0.01)
plot(p, logis(p))


# ---- some_data ----



# we want to check the probability/proportion of people in Adelaide aged 21-70 
# spend enough time in physical activity (0 = no, 1 = yes)

set.seed(12345)
n <- 200

modelling_dat <- 
  tibble(
    gndr = sample(x = c("M", "F", "NB"), size = n, prob = c(0.4, 0.4, 0.2), replace = TRUE),
    age = sample(21:70, size = n, replace = TRUE)
  ) %>%
  mutate(
    log_odds = 0 + 0.5 * (gndr == "F") - 0.5 *(gndr == "M") - 0.05 * (age - 45) + 0.01 * (gndr == "F") * (age - 45),
    prob = inv_logis(log_odds),
    phys_guides = rbinom(n = nrow(.), size = 1, prob = prob)
  ) 

# modelling_dat %>%
#   ggplot(., aes(x = log_odds, fill = gndr)) +
#   geom_histogram()

dat <-
  modelling_dat %>%
  select(-prob, -log_odds)

dat



# ---- explore ----



ggplot(dat, aes(x = age, y = phys_guides, col = gndr)) +
  geom_jitter(height = 0.1) +
  theme_bw() +
  scale_colour_colorblind()


ggplot(dat, aes(x = age, y = phys_guides, col = gndr)) +
  geom_jitter(height = 0.1) +
  theme_bw() +
  scale_colour_colorblind()



dat <-
  dat %>%
  mutate(age_cat = cut(age, seq(20, 70, by = 10)))

dat

with(dat, table(age, age_cat))


# test
mean(c(0, 0, 0, 1, 1))

dat %>%
  group_by(age_cat) %>%
  summarise(prop_guide = mean(phys_guides), .groups = "drop") %>%
  ggplot(., aes(x = age_cat, y = prop_guide)) +
  geom_point() +
  theme_bw()


dat %>%
  group_by(gndr) %>%
  summarise(prop_guide = mean(phys_guides), .groups = "drop") %>%
  ggplot(., aes(x = gndr, y = prop_guide)) +
  geom_point() +
  theme_bw()

dat %>%
  group_by(age_cat, gndr) %>%
  summarise(prop_guide = mean(phys_guides), .groups = "drop") %>%
  ggplot(., aes(x = interaction(age_cat, gndr), y = prop_guide)) +
  geom_point() +
  theme_bw()


dat %>%
  group_by(age_cat, gndr) %>%
  summarise(prop_guide = mean(phys_guides), .groups = "drop") %>%
  ggplot(., aes(x = interaction(age_cat, gndr), y = prop_guide, col = gndr)) +
  geom_point() +
  theme_bw() +
  scale_colour_colorblind()



dat %>%
  group_by(age_cat, gndr) %>%
  summarise(
    prop_guide = mean(phys_guides), 
    n = n(),
    .groups = "drop") %>%
  ggplot(., aes(x = interaction(age_cat, gndr), y = prop_guide, col = gndr)) +
  geom_point() +
  geom_label(aes(label = n)) +
  theme_bw() +
  scale_colour_colorblind()



dat %>%
  group_by(age_cat, gndr) %>%
  summarise(
    prop_guide = mean(phys_guides), 
    n = n(),
    .groups = "drop") %>%
  ggplot(., aes(x = age_cat, y = prop_guide, col = gndr)) +
  geom_point() +
  geom_label(aes(label = n)) +
  facet_wrap(~ gndr, nrow = 1) +
  theme_bw() +
  scale_colour_colorblind()


# ---- model ----



logis_mod1 <- glm(phys_guides ~ age + gndr, data = dat, family = "binomial")
summary(logis_mod1)

## odds ratios 
exp(coef(logis_mod1))

## odds ratios and 95% CI
exp(cbind(OR = coef(logis_mod1), confint(logis_mod1)))


logis_mod2 <- glm(phys_guides ~ age_cat + gndr, data = dat, family = "binomial")
summary(logis_mod2)

logis_mod3 <- glm(phys_guides ~ age * gndr, data = dat, family = "binomial")
summary(logis_mod3)


anova(logis_mod1, logis_mod3, test = "Chisq")


# ---- predictions ----

# new_dat <- expand_grid(...)

mod1_preds <-
  dat %>%
  mutate(
    mod_pred_logodds = predict.glm(logis_mod1, type = "link"),
    mod_pred_probs = predict.glm(logis_mod1, type = "response")
  )

mod1_preds

mod2_preds <-
  dat %>%
  mutate(
    mod_pred_logodds = predict.glm(logis_mod2, type = "link"),
    mod_pred_probs = predict.glm(logis_mod2, type = "response")
  )

mod2_preds
