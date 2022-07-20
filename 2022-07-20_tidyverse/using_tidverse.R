
# ---- previously ----

# https://github.com/tystan/coda_club/blob/main/2022-06-22_installing-r/2022-getting-started-with-r.pdf

# https://www.datacamp.com/freeweek

# ---- preamble ----

### today's topic:
# "tidyverse" and  manipulating data in R 
# (joining data/merging, lengthening, widening)

# https://www.tidyverse.org/


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
  select(-value) 

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


### got up to here

# filter


# mutate


# joins

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


# group_by/ungroup/summarise



# ---- tidyr ----


# pivot_wider (spread)
(deck_wide <-
  deck %>%
  pivot_wider(names_from = face, values_from = value))


# pivot_longer (gather)
# use widen data to convert back!
deck_wide %>%
  pivot_longer(-suit, names_to = "face", values_to = "value")



# ---- ggplot2 ----


# ---- stringr ----


# ---- lubridate ----





