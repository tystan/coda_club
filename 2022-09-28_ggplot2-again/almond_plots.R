

# ---- libs ----

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")


# ---- read ----

alm_dat <- read_csv("2022-09-28_ggplot2-again/dat/Almondcog.csv")

alm_dat


# ---- cholesterol_plots ----

# TCHOLHDLRatio (cholesterol) over time by intenvention group

# y: TCHOLHDLRatio
# x: VISIT
# grouping: ParticipantNumber
# facet: GROUP  


# how many rows per person using pat no
alm_dat %>%
  group_by(ParticipantNumber) %>%
  summarise(n = n())

# how many rows per person using `ID`
alm_dat %>%
  group_by(ID) %>%
  summarise(n = n())


# plot cholesterol values at each visit number, colour by intervention group
# (make group factor so "Control" is second level)
alm_dat %>%
  mutate(
    VISIT = factor(VISIT),
    GROUP = ifelse(GROUP == 1, "Intervention", "Control"),
    GROUP = factor(GROUP, levels = c("Intervention", "Control"))
  ) %>%
  ggplot(., aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP)) +
  theme_classic() +
  geom_point()

# plot cholesterol values (and violin plot) at each visit number,
# jittered points to avoid overlap,
# facet and colour by intervention group
# (make group factor so "Control" is second level)
alm_dat %>%
  mutate(
    VISIT = factor(VISIT),
    GROUP = ifelse(GROUP == 1, "Intervention", "Control"),
    GROUP = factor(GROUP, levels = c("Intervention", "Control"))
  ) %>%
  ggplot(., aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP)) +
  theme_classic() +
  geom_violin() +
  geom_jitter() +
  facet_grid( ~ GROUP)

# same as above but also facet by gender
# (points also given transperancy)
alm_dat %>%
  mutate(
    VISIT = factor(VISIT),
    GROUP = ifelse(GROUP == 1, "Intervention", "Control"),
    GROUP = factor(GROUP, levels = c("Intervention", "Control"))
  ) %>%
  ggplot(., aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP)) +
  theme_classic() +
  geom_violin() +
  # geom_point(alpha = 0.25) +
  geom_jitter(alpha = 0.25) +
  facet_grid(GROUP ~ Gender, labeller = label_both)

# update dataset for processing
alm_dat_plot <-
  alm_dat %>%
  mutate(
    VISIT = factor(VISIT),
    GROUP = ifelse(GROUP == 1, "Intervention", "Control"),
    GROUP = factor(GROUP, levels = c("Intervention", "Control"))
  ) 

# calculate a summary dataset to also include in plot
alm_dat_plot_means <-
  alm_dat_plot %>%
  group_by(VISIT, GROUP, Gender) %>%
  summarise(TCHOLHDLRatio = mean(TCHOLHDLRatio, na.rm = TRUE), .groups= "drop")

# demonstrating that each layer can have its own data input and aesthetics
ggplot() +
  theme_classic() +
  geom_violin(
    aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP), 
    data = alm_dat_plot
  ) +
  geom_point(
    aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP), 
    data = alm_dat_plot, 
    alpha = 0.25
  ) +
  # this is adding the mean values for {group,sex,visit}-tuple
  geom_point(
    aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP), 
    data = alm_dat_plot_means, 
    size = 20, 
    pch = "_"
  ) +
  facet_grid(GROUP ~ Gender)

# now add lines between cholesterol values within each ID (person) to
# see change over time/visits
# cholesterol y-axis uses log2 spacing
alm_dat_plot %>%
  ggplot(., aes(x = VISIT, y = TCHOLHDLRatio, col = GROUP)) +
  theme_bw() +
  # geom_violin() +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = ParticipantNumber), lty = 1, alpha = 0.2) +
  facet_grid( ~ Gender, labeller = label_both) +
  labs(x = "Visit number", col = "Intervention\ngroup") +
  # scale_colour_brewer(palette = "Set1") + # this is a pre-defined palette
  # scale_colour_manual(values = c("cyan", "magenta"))
  scale_colour_manual(values = c("firebrick1", "dodgerblue")) +
  scale_y_continuous(trans = "log2")


# to save plots, use `ggsave()`
# recommended outputs are png (lossless raster) or pdf (vector)
ggsave(
  "2022-09-28_ggplot2-again/fig/cholesterol_by_visit_group_gender_within_id.png", 
  plot = last_plot(),
  width = 5, 
  height = 6, 
  dpi = 400
)



# ---- bmi_plots ----



alm_dat %>%
  ggplot(., aes(x = weight, y = height, col = BMI)) +
  geom_point() +
  scale_colour_viridis_b() +
  theme_bw()


alm_dat %>%
  ggplot(., aes(x = weight, y = height, col = BMI, shape = factor(Gender))) +
  geom_point(alpha = 0.5) +
  scale_colour_viridis_b() +
  theme_bw()

alm_dat %>%
  ggplot(., aes(x = weight, y = height, col = BMI)) +
  geom_point() +
  facet_wrap(~Gender) +
  scale_colour_viridis_b() +
  theme_bw()


# ---- plot_oscillations_example ----

  
# create tibble of sine wave
n_points <- 301
ot_dat <-
  tibble(
    time = seq(0, 30, length = n_points),
    f = 3 * sin(time) +  sin(8 * time) + rnorm(n_points)
  )

ot_dat %>%
  ggplot(., aes(x = time, y = f)) +
  geom_line(col = "orange", alpha = 0.5) +
  geom_point(shape = "$", col = "purple", size = 2) +
  theme_bw() +
  # things look fancier with serif font
  theme(
    text = element_text(family = "serif")
  )


ggsave(
  "2022-09-28_ggplot2-again/fig/oscillation_example.pdf", 
  plot = last_plot(),
  width = 7, 
  height = 4
)


  
  
  
  
  
