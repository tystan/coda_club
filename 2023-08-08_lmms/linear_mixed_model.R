
# ---- libs ----


# also require "nlme" for the data
library("lme4")     # linear mixed effect model package
library("lmerTest") # adds p-values to summary of lmer() models
library("tidyr")
library("dplyr")    # data manipulation and pipe (`%>%`)
library("ggplot2")  # plotting
library("ggthemes") # colour palettes for ggplot



# ---- the_data ----

# Investigators at the University of North Carolina Dental School 
# followed the growth of 27 children (16 males, 11 females) 
# from age 8 until age 14. Every two years they measured the 
# distance between the pituitary and the pterygomaxillary fissure (in mm), 
# two points that are easily identified on x-ray exposures of the 
# side of the head.

# load data
data(Orthodont, package = "nlme")

# peak at first 10 rows
head(Orthodont, n = 10)


# ---- subset_data ----

orth_f <-
  Orthodont %>%
  as_tibble(.) %>%
  dplyr::filter(Sex == "Female") %>%
  mutate(Subject = as.character(Subject))

orth_f

# sample size
(n_partic <- length(unique(orth_f$Subject)))
(n_orth <- nrow(orth_f))

# have a look at data
# should always do version of this berfore fitting models!
orth_f %>%
  ggplot(., aes(x = age, y = distance, col = Subject)) +
  facet_wrap(~ Subject, ncol = 3) +
  geom_point() +
  scale_colour_tableau(palette = "Tableau 20") +
  theme_bw() 


# ---- fit_standard_linear_regression ----

### what if we fit a linear regression as before? Why is this not appropriate?

summary(lm(distance ~ age, data = orth_f))

# fyi this is variance of difference between observed and predicted
2.164 ^ 2 
sigma(lm(distance ~ age, data = orth_f)) ^ 2 # extraced from model instead of output


# ---- fit_linear_mixed_effects ----

summary(m1_f <- lmer(distance ~ age + (1 | Subject), data = orth_f))
# fyi this is the total variance of difference between observed and 
# (population/fixed) predicted
2.068 ^ 2 + 0.780 ^ 2 


# add LME values to original dataset (ignore, just for plotting)
orth_f <- 
  orth_f %>% 
  mutate(
    ### SAME: 
    # pop_pred = 17.37273 + 0.47955 * age
    pop_pred = predict(m1_f, re.form = ~ 0)
  )
orth_f_ranef <-
  tibble(
    Subject = rownames(ranef(m1_f)$Subject),
    ran_int = ranef(m1_f)$Subject[["(Intercept)"]]
  )
orth_f <- 
  inner_join(orth_f, orth_f_ranef, "Subject") %>%
  mutate(resid_err = distance - (pop_pred + ran_int))


# ---- connect_data_and_model_output ----


# store a plot of the data
fem_plot <-
  orth_f %>%
  ggplot(., aes(x = age, y = distance, col = Subject)) +
  geom_point() +
  scale_colour_tableau(palette = "Tableau 20") +
  theme_bw() 

# "print" basic plot
fem_plot

# now let's add a (population/fixed) predicted line
fem_plot +
  geom_line(aes(y = pop_pred, group = Subject), size = 3, col = "black")


# now have random intercept offsets for each subject 
# as well as (population/fixed) predicted line
fem_plot +
  annotate(
    geom = "segment", 
    x = 8, y = 17.37273 + 0.47955 * 8,
    xend = 14, yend = 17.37273 + 0.47955 * 14,
    size = 3
  ) +
  geom_line(aes(y = pop_pred + ran_int, group = Subject)) 


### question: how does the model values relate to the summary output?!?

summary(m1_f)
# fixef intercept = 17.37273
# fixef slope =  0.47955
# ranef intercept sd: 2.068
orth_f_ranef %>% summarise(ranef_sd = sd(ran_int))
# ranef residual error sd: 0.780
orth_f %>% summarise(resid_sd = sd(resid_err))





# ---- different_models_for_full_data ----



summary(m1 <- lmer(distance ~ age + Sex + (1  |Subject), data=Orthodont))
summary(m2 <- lmer(distance ~ age       + (1  |Subject), data=Orthodont))
summary(m3 <- lmer(distance ~ age + Sex + (age|Subject), data=Orthodont))

BIC(m1, m2, m3)
AIC(m1, m2, m3)

## note: re.form = NULL means include all random effects in preds
## note: re.form = NA means IGNORE random effects in preds (i.e. population prediction)
orth_preds <- 
  cbind(
    Orthodont,
    m1_pred     = predict(m1, re.form = NULL),
    m1_pred_re0 = predict(m1, re.form = NA),
    # m2_pred     = predict(m2, re.form = NULL),
    m3_pred     = predict(m3, re.form = NULL)
  )

orth_preds_lng <-
  orth_preds %>%
  pivot_longer(cols = c(distance, m1_pred, m1_pred_re0, m3_pred)) %>% # m2_pred, 
  rename(distance = value) %>%
  mutate(name = ifelse(name == "distance", "observed", name))


orth_preds_lng %>%
  dplyr::filter(Subject %in% c("M10", "M13", "M15", "F09", "F10", "F11")) %>%
  ggplot(., aes(x = age, y = distance, group = name, col = name)) +
    geom_line(alpha = 0.2) +
    geom_point() +
    facet_wrap( ~ Subject) +
    theme_bw() +
    scale_colour_manual(values = c("darkorange", "purple", "cyan4", "black"))


plot(
  1:n_orth, predict(m1, re.form = NULL), col = "orange",
  type = "p", xlab = "obs. number", ylab = "predicted `distance`"
) 
points(1:n_orth, predict(m2, re.form = NULL), col = "dodgerblue", type = "p") 
points(1:n_orth, predict(m3, re.form = NULL), col = "green", type = "p") 


