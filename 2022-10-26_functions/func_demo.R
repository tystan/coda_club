
# ---- preamble ----

### my crappy discussion of "functions":
# /2022-10-26_functions/structure-of-functions.pdf 

### A nicely presented structure of functions in R:
# https://cosimameyer.com/media/CS_Functions.pdf 

### Auto code formatting:
# that a code formatter (a "linter" or "linting") can used by installing the `styler` package:
#   install.packages("styler")
# and then there is the `Addins` button up the top of RStudio (may need a restart), and you can go
#   `Addins` --> `Style selection`
# and whatever highlighted code will be auto formatted nicely!
  

# ---- libs ----

### only need to install once:
# install.packages("openintro") # contains lots of datasets to use
# install.packages("styler") # to get the linting/formatting package

# load packages for
library("dplyr") 
library("openintro")


# ---- data_exploration ----


# load data ("High School and Beyond survey")
data(hsb2)

# peek at data
hsb2

# info about data:
?hsb2

# create a copy of the data to manipulate without fear or favour
hsb_edit <- hsb2


# we are going to look at the subject scores so let's look how they are distributed
hist(hsb_edit$read)
hist(hsb_edit[["write"]])

# look at raw scores
table(hsb_edit[["write"]], useNA = "ifany")


# so if we want to centre and scale the `write`ing scores, we need
# to find the mean (to take away, "centring") and then divide by the
# standard deviation ("scale")
var_mean <- mean(hsb_edit[["write"]])
var_sd <- sd(hsb_edit[["write"]])
# create a new column to store the result
hsb_edit[["write_stdised"]] <- (hsb_edit[["write"]] - var_mean) / var_sd

# the same process to centre and scale for the `read`ing scores
var_mean <- mean(hsb_edit[["read"]])
var_sd <- sd(hsb_edit[["read"]])
hsb_edit[["read_stdised"]] <- (hsb_edit[["read"]] - var_mean) / var_sd

# check that the transformation has worked
hist(hsb_edit[["read_stdised"]])

### So the question is, can using a function make this process quicker, 
### easier to read, or less likely to make mistakes?

# I think so :-)



# ---- how_to_make_a_function ----

### the structure of a function in R
### note things between `<` and `>` are your choice!
# <function_name> <- function(<input>) {
#   
#   <do things, usually involving <input>>
#     
#   return(<some value(s)>)
#   
# }


# example function that takes in a value, and returns the squared value
ashs_sq_fun <- function(x) {
  
  y <- x ^ 2 
  
  return(y)
  
}

# how to use the function you created!
ashs_sq_fun(1) # 1
ashs_sq_fun(5) # 25
# you can even pass a vector of values to be squared element-wise
1:5
ashs_sq_fun(1:5)
# what about non-whole number?
ashs_sq_fun(1.6) # no probs
# squaring "a" doesn't make sense tho
ashs_sq_fun("a")

# another example but using two inputs (we can make as many inputs as we like!)
dots_pow_fun <- function(x, pow) {
  
  y <- x ^ pow 
  
  return(y)
  
}

# example usage
dots_pow_fun(3, 2)
dots_pow_fun(3, 4)


# ---- centre_and_scale_fn ----



# INPUTS: a vector of numeric scores
# OUTPUTS: a vector of the same length as the input but with the values centred and scaled
# note that the function body look very similar to what we previously did
standardise_test_scores <- function(scores) {
  
  var_mean <- mean(scores)
  var_sd <- sd(scores)
  scores_stdardised <- (scores - var_mean) / var_sd
  
  return(scores_stdardised)
  
}

# this is how use use our new function!
# so we can see with a little up-front effort we can get some economy of scales
# type efficiency in centre-and-scaling this way (as less chance of errors)
hsb_edit[["read_stdised"]] <- standardise_test_scores(hsb2[["read"]])
hsb_edit[["write_stdised"]] <- standardise_test_scores(hsb2[["write"]])
hsb_edit[["math_stdised"]] <- standardise_test_scores(hsb2[["math"]])
hsb_edit[["science_stdised"]] <- standardise_test_scores(hsb2[["science"]])
hsb_edit[["socst_stdised"]] <- standardise_test_scores(hsb2[["socst"]])

# have a peak
hsb_edit


# ---- lisas_hour_and_min_fn ----

# INPUT: 
#   x: a numeric offset from 12 midnight in minutes
# OUTPUT: 
#   a string in the form "hh:mm" of the corresponding bedtime (24 hour time) 

convert_bed_time <- function(x) {
  x <- x %% 1440                      # confine to [0, 1439] minutes range
  hr <- floor(x / 60)                 # calculate the whole number of hours
  min <- sprintf("%02.0f", x %% 60)   # calculate the remainder (minutes in excess of the hour)
  time_string <- paste0(hr, ":", min) # create "hh:mm" result
  return(time_string)
}

# example usage:
convert_bed_time(100)  # "1:40"
convert_bed_time(-100) # "22:20"
convert_bed_time(1400) # "23:20"
convert_bed_time(1111) # "18:31"
convert_bed_time(1440 / 2) # halfway through day "12:00"

# can we provide a vector of values? (YES!):
bed_times_mins <- c(100, -100, 1400, 1081, 720)
convert_bed_time(bed_times_mins) 
# [1] "1:40"  "22:20" "23:20" "18:31" "12:00"



# ---- skeleton_of_a_function_to_read_in_multiple_csvs ----

join_csvs <- function(csv_names) {
  
  csv1 <- read_csv(csv_names[1])
  csv2 <- read_csv(csv_names[2])
  csv3 <- read_csv(csv_names[3])
  csv4 <- read_csv(csv_names[4])
  
  all_csv <- rbind(csv1, csv2, csv3, csv4)
  
  return(all_csv)
  
}


getwd()
(my_csvs <- list.files()) # these are the theorectical files to import
join_csvs(my_csvs) # give the function the 4 csvs to add together








