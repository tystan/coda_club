
# ---- lookback ----

### what we've learnt so far:
# installing r
# installing r packages
# creating variables in R
# using R functions/maths operations
# creating vectors in R
# learnt about different data types (atomic and data.frames)
# subsetting/getting elements out of vectors/data.frames
# importing data into R (lots of formats)
# read one of Dot's excellent papers to get us thinking about CoDA data
# tidyverse so far:
#   arrange()
#   select()
#   distinct()

### NEW!
# filter
# mutate
# group_by/ungroup/summarise
# joins


# ---- lookforward ----

### today's plan:
# finish "tidyverse" and manipulating data in R 

### tidyverse to come:
# lengthening data (long data is tidyverse data)
# widening data
# ggplot2





# ---- libs ----

# install.packages("tidyverse")
# library("tidyverse")

library("readr")     # reading in data
library("tibble")    # improved data.frames
library("dplyr")     # data manipulation and joins
library("tidyr")     # mainly for lengthening and widening data frames
library("stringr")   # manipulating strings (character variables)
library("ggplot2")   # plotting using a "grammar of graphics"
library("lubridate") # manipulating date data
library("forcats")   # categorical variables
library("purrr")     # maping data to functions (don't worry about this one!)



# ---- readr ----

getwd()

# data is from 'Hands-On Programming with R' by Garrett Grolemund
# available here: http://bit.ly/deck_CSV

# deck <- read_csv("2022-07-20_tidyverse/dat/deck.csv")
deck <- read.csv("2022-07-20_tidyverse/dat/deck.csv")

deck


# ---- tibble ----

deck <- as_tibble(deck)
deck

trump_cards <-
  data.frame(
    face = rep("two", 4),
    suit = c("spades", "hearts", "diamonds", "clubs"),
    is_trump = TRUE,
    multiplier = 1:4
  )

trump_cards <-
  tibble(
    face = rep("two", 4),
    suit = c("spades", "hearts", "diamonds", "clubs"),
    is_trump = TRUE,
    multiplier = 1:4
  )

trump_cards <-
  tribble(
    ~face, ~suit, ~is_trump, ~multiplier,
    "two",   "spades", TRUE, 1,
    "two",   "hearts", TRUE, 2,
    "two", "diamonds", TRUE, 3,
    "two",    "clubs", TRUE, 4
  )



# ---- dplyr ----

# select function
select(deck, suit, value)



# alternatively using base R
# deck[ , c("suit", "value")]

### piping: %>%
# `%>%` means pass the thing to the left to the function on the right

# the dot "." represents the data passed as the first argument to the function
deck %>%
  select(., suit, value) 

# although you don't have to use the dot
deck %>%
  select(suit, value) 


# can remove columns using "!" or "-"
deck %>%
  select(!value) 

deck %>%
  select(-value, -suit, -face) 

# metaphor of the pipe operator (does a similar-ish thing)
2 + 3
`+`(2, 3)


# arrange function
deck %>%
  arrange(value) 
# order the other way
deck %>%
  arrange(desc(value))
# order by two variables
deck %>%
  arrange(value, suit) 

# non-pipe notation
arrange(deck, value, suit) 

# distinct function
deck %>%
  distinct(., suit) 

# Use two operations/functions sequentially
deck_small <-
  deck %>%
  distinct(., suit) %>%
  arrange(., suit)

# alternatively, non-pipe way
deck_small_intermediate <- distinct(deck, suit)
deck_small <- arrange(deck_small_intermediate, suit)
deck_small = arrange(deck_small_intermediate, suit)


### NEW STUFF FROM HERE

# filter


deck %>%
  tail(.)

deck %>%
  filter(suit == "spades")

# doesn't work
deck %>%
  filter(suit = "spades")

# doesn't work
deck %>%
  filter(suit == spades)

deck %>%
  filter(., value >= 10) %>%
  filter(., value >= 10)

deck %>%
  filter(., value >= 10, suit == "spades")

deck %>%
  filter(., value > 10) %>%
  arrange(., suit, face, desc(value))


# mutate

deck %>%
  mutate(
    ., 
    new_value = 10 * value,
    new_new_value = 10 * new_value
  )

# conditional

deck %>%
  mutate(
    new_value = ifelse(value > 10, value / 10, value * 2)
  )



# group_by/ungroup/summarise


deck %>%
  group_by(., suit)

deck %>%
  group_by(., suit) %>%
  summarise(
    ave_val = mean(value)
  ) %>%
  ungroup()

deck %>%
  mutate(new_value = ifelse(value > 10, value / 10, value * 2)) %>%
  group_by(., suit) %>%
  summarise(
    ave_val = mean(value),
    sd_val = sd(value),
    ave_new_val = mean(new_value),
    sd_new_val = sd(new_value)
  ) %>%
  ungroup()


# putting it all together
# what is the [total value] of the [non-royal cards] [in each suit]?

# (let's break this into smaller steps)

deck %>%
  filter(., value <= 10)


deck %>%
  filter(., value <= 10) %>%
  group_by(., suit) 


# turtning a bunch of numbers into a single number ("boiling down" -- some guy)
mean(c(1, 2, 3))
sd(c(1, 2, 3))
sum(c(1, 2, 3))


deck %>%
  filter(., value <= 10) %>%
  group_by(., suit) %>%
  summarise(., total_val = sum(value))

# fun fact: 
# sum(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
# = (1 + 10) + (2 + 9) + (3 + 8) + (4 + 7) + (5 + 6)
# = (11) + (11) + (11) + (11) + (11) 
# = 5 * (11)
# = 55 as we see above!

### joins 
### reference: https://r4ds.had.co.nz/relational-data.html#understanding-joins
### (check the number of rows as a sanity check before and after!)
# inner
# left (or right)
# full
# anti


dat1 <-
  tibble(
    chars = c("A", "B", "C", "D"),
    dat1_indicator = 1
  )
dat2 <-
  tibble(
    chars = c("C", "D", "E"),
    dat2_indicator = 1
  )

dat1
dat2

# inner
inner_join(
  dat1,
  dat2,
  "chars"
)

# left (or right)
left_join(
  dat1,
  dat2,
  "chars"
)


# full
nrow(dat1)
nrow(dat2)
dat3 <-
  full_join(
    dat1,
    dat2,
    "chars"
  )
nrow(dat3)


# anti



# bigger join operations


inner_join(
  deck,
  trump_cards,
  c("face", "suit")
)

left_join(
  deck,
  trump_cards,
  c("face", "suit")
)

right_join(
  deck,
  trump_cards,
  c("face", "suit")
)

# not used very often
full_join(
  deck,
  trump_cards,
  c("face", "suit")
)


# ---- tidyr ----

### example links about "wide" and "long" data:
# https://environmentaldatainitiative.org/five-phases-of-data-publishing/phase-2/long-or-wide/
# https://towardsdatascience.com/long-and-wide-formats-in-data-explained-e48d7c9a06cb
# https://anvil.works/blog/tidy-data


# pivot_wider (spread)
(deck_wide <-
   deck %>%
   pivot_wider(names_from = face, values_from = value))


# pivot_longer (gather)
# use widen data to convert back!
(deck_long <-
  deck_wide %>%
  pivot_longer(cols = -suit, names_to = "face", values_to = "value"))



# ---- ggplot2 ----

### basic setup (REQUIRED)
# Data: long format
# Aesthetics: aes() function selecting x, y, col, shape, fill
# Geoms: layers for plot - what are we plotting (points, lines, tiles?)


### setup (OPTIONAL)
# Scales: change how aesthetics look/are presented
# Theme: change plot look from default
# Guides: modify the figure legend

### general ggplot2 call
# ggplot(
#   data = ..., 
#   aes(x = ..., ...)
# ) +
#   geom_xxxx() 


# list of geoms etc:
# https://ggplot2.tidyverse.org/reference/index.html

# handy cheat sheet
# https://github.com/rstudio/cheatsheets/blob/main/data-visualization-2.1.pdf


deck_long_spades <-
  deck_long %>%
  filter(suit == "spades") %>%
  mutate(new_value = ifelse(value > 10, value / 10, value * 2))

# scatterplot
ggplot(
  data = deck_long_spades, 
  aes(x = value, y = new_value)
) +
  geom_point() +
  theme_bw()

# scatterplot by name
# ordering no good?
ggplot(
  data = deck_long_spades, 
  aes(x = face, y = new_value)
) +
  geom_point() +
  theme_bw()

# deck_long_spades %>% arrange(value) %>% distinct(face) %>% pull()
ordered_face_vals <-
  c(
    "ace", "two", "three", 
    "four", "five", "six", 
    "seven", "eight", "nine", 
    "ten", "jack", "queen", 
    "king"
  )

deck_long_spades$face <- 
  factor(deck_long_spades$face, levels = ordered_face_vals)


# scatterplot by name
# better ordering now?
ggplot(
  data = deck_long_spades, 
  aes(x = face, y = new_value)
) +
  geom_point() +
  theme_bw()


# now let's use the full data to do fancier things!

deck_long_for_plot <-
  deck_long %>%
  mutate(new_value = ifelse(value > 10, value / 10, value * 2))

deck_long_for_plot$face <- 
  factor(deck_long_for_plot$face, levels = ordered_face_vals)


# jitter points so don't overlap
ggplot(
  data = deck_long_for_plot, 
  aes(x = face, y = new_value)
) +
  geom_jitter() +
  theme_bw()


# let's add colour! what about shapes too
ggplot(
  data = deck_long_spades, 
  aes(x = face, y = new_value, col = suit, shape = suit)
) +
  geom_jitter() +
  theme_bw()


# let's control the colour
ggplot(
  data = deck_long_spades, 
  aes(x = face, y = new_value, col = suit, shape = suit)
) +
  geom_jitter() +
  scale_color_manual(
    values = c(
      "clubs" = "black",
      "diamonds" = "red",
      "hearts" = "pink",
      "spades" = "grey50"
    )
  ) +
  theme_bw()









