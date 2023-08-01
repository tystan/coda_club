
# also require "nlme" for the data
library("lme4")
library("tidyr")
library("dplyr")
library("ggplot2")

# Investigators at the University of North Carolina Dental School 
# followed the growth of 27 children (16 males, 11 females) 
# from age 8 until age 14. Every two years they measured the 
# distance between the pituitary and the pterygomaxillary fissure (in mm), 
# two points that are easily identified on x-ray exposures of the 
# side of the head.
data(Orthodont, package = "nlme")
head(Orthodont, n = 10)

(n_orth <- nrow(Orthodont))
(n_partic <- length(unique(Orthodont$Subject)))


Orthodont %>%
  ggplot(., aes(x = age, y = distance, group = Subject, col = Sex)) +
  geom_line(alpha = 0.5) +
  geom_point() +
  facet_wrap( ~ Subject) +
  theme_bw() 






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


