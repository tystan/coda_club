
# ---- libs ----

library("dplyr")
library("ggplot2")
library("ggthemes")
library("forcats")



# ---- apologies-for-the-preamble ----


### What is a random variable (RV)?
# A quantity yet to be observed or measured that can take
# different outcomes/values because of "chance"

### examples
# a die roll
# coin flip
# weight of randomly chosen dog
# the time you fall asleep on a given night
# your blood sugar level at a point in time
# the time you spend in light physical activity on a given day
# count of people entering a store
# number of purple cars in a car park
# the number of goals scored in a soccer match
# the time it takes for you to swim 50m on a given day


### properties:
# some RV are discrete, some are continuous
# we use probabilities to describe possible outcomes/values it takes
# probability can be thought of a the proportion of times we expect something 
#    to happen if we "test" it out lot-and-lots of times
#   - e.g. if we roll 3 on a die 10 times from 50 rolls we might estimate
#          the probability of rolling a 3 is p = 10/50 = 0.2 


### the possible RV values are approximated/described using "distributions"
# normally there are parameters that describe the :
#    location (e.g. mean) and
#    spread/dispersion (e.g. variance/SD) parameters

### commonly used distributions...

# ---- normal/Gaussian ----

# e.g. the weight of a randomly chosen adult German shepherd (kg)

# theoretical probability "density"
x <- seq(10, 60, by = 0.1)
fx <- dnorm(x, mean = 35, sd = 5)
par(mfrow = c(1, 2))
plot(x, fx, type = "l", col = "dodgerblue")

# since probs are proprtions over lot-and-lots of trials we could also
# randomly generate lots of german shepard weights and look at the histogram!
rand_gsw <- rnorm(n = 10000, mean = 35, sd = 5)
head(rand_gsw)
hist(rand_gsw, border = "dodgerblue")


# ---- uniform ----


# e.g. the weight of a randomly chosen adult German shepherd (kg)

# theoretical probability "density"
x <- seq(10, 60, by = 0.1)
fx <- dunif(x, min = 20, max = 50)
par(mfrow = c(1, 2))
plot(x, fx, type = "l", col = "firebrick")

# since probs are proprtions over lot-and-lots of trials we could also
# randomly generate lots of german shepard weights and look at the histogram!
rand_gsw <- runif(n = 10000, min = 20, max = 50)
head(rand_gsw)
hist(rand_gsw, border = "firebrick")


# ---- Chi-square ----


# e.g. the weight of a randomly chosen adult German shepherd (kg)

# theoretical probability "density"
x <- seq(0, 60, by = 0.1)
fx <- dchisq(x, df = 20)
par(mfrow = c(1, 2))
plot(x, fx, type = "l", col = "orange")

# since probs are proprtions over lot-and-lots of trials we could also
# randomly generate lots of german shepard weights and look at the histogram!
rand_gsw <- rchisq(n = 10000, df = 20)
head(rand_gsw)
hist(rand_gsw, border = "orange")


# ---- F ----

### NB: not particularly realistic for dog weights...
# theoretical probability "density"
x <- seq(0, 60, by = 0.1)
fx <- df(x, df1 = 15, df2 = 15)
par(mfrow = c(1, 2))
plot(x, fx, type = "l", col = "purple")

# since probs are proprtions over lot-and-lots of trials we could also
# randomly generate lots of german shepard weights and look at the histogram!
rand_gsw <- rf(n = 10000, df1 = 15, df2 = 15)
head(rand_gsw)
hist(rand_gsw, border = "purple")




# ---- binomial ----




# theoretical probability "density"
x <- 0:10 # discrete distribution
# this is prob of the number of 6s rolled after 10 rolls
fx <- dbinom(x, size = 10, p = 1 / 6) 
par(mfrow = c(1, 2))
plot(x, fx, type = "h", ylab = "probability",  col = "turquoise4")

# since probs are proprtions over lot-and-lots of trials we could also
# randomly generate lots of german shepard weights and look at the histogram!
rand_gsw <- rbinom(n = 10000, size = 10, p = 1 / 6)
head(rand_gsw) # these are the random counts
(no_of_sixes <- table(rand_gsw))
plot(
  x = as.numeric(names(no_of_sixes)), 
  y = c(no_of_sixes), 
  type = "h", ylab = "count",  col = "turquoise4"
)



# ---- beta ----

# e.g. proportion (or percentage) of satisfaction from a user survey

# theoretical probability "density"
x <- seq(-0.1, 1.1, by = 0.01)
fx <- dbeta(x, shape1 = 4, shape2 = 2)
par(mfrow = c(1, 2))
plot(x, fx, type = "l", col = "magenta")

rand_gsw <- rbeta(n = 10000, shape1 = 4, shape2 = 2)
head(rand_gsw)
hist(rand_gsw, border = "magenta")


# ---- plot_of_all_examples ----


n_samples <- 100000

examples_df <-
  bind_rows(
    tibble(dist = "U(20, 50)\n[continuous, values: min to max]", rand_vals = runif(n = n_samples, min = 20, max = 50)),
    tibble(dist = "N(35, 5)\n[continuous, values: -Inf to +Inf]", rand_vals = rnorm(n = n_samples, mean = 35, sd = 5)),
    tibble(dist = "X^2(df = 20)\n[continuous, values: 0 to +Inf]", rand_vals = rchisq(n = n_samples, df = 20)),
    tibble(dist = "F(df1 = 15, df2 = 15)\n[continuous, values: 0 to +Inf]", rand_vals = rf(n = n_samples, df1 = 15, df2 = 15)),
    tibble(dist = "Beta(a = 4, b = 2)\n[continuous, values: 0 to 1]", rand_vals = rbeta(n = n_samples, shape1 = 4, shape2 = 2)),
    tibble(dist = "Binom(n = 10, p = 1/6)\n[discrete, values: 0, 1, 2, ..., n]", rand_vals = rbinom(n = n_samples, size = 10, p = 1 / 6)),
    tibble(dist = "Poisson(lambda = 4)\n[discrete, values: 0, 1, 2, ...]", rand_vals = rpois(n = n_samples, lambda = 4)),
    tibble(dist = "NegBin(n = 5, p = 1/3)\n[discrete, values: 0, 1, 2, ...]", rand_vals = rnbinom(n = n_samples, size = 5, p = 1/3))
  )


examples_df %>%
  mutate(dist = fct_inorder(dist)) %>%
  ggplot(., aes(x = rand_vals, col = dist, fill = dist)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ dist, ncol = 2, scales = "free") +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  labs(col = "Distribution", fill = "Distribution") +
  theme_bw() +
  theme(legend.key.height = unit(3, "lines"))

ggsave(filename = "2023-08-15_rand-vars/example_dists.png", height = 10, width = 8)


# ---- next_time ----

# Explanation of what the notation like X ~ N(mu, sigma) means
# t-test of random observations we generate as an example


