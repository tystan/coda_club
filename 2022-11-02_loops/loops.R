
# ---- previously_on_coda_club ----

# functions!
# a way to automate the same process


# ---- today ----

# loops:
# a way to repeat the same process as many times as you like

# ---- libs ----

### only need to install once:
# install.packages("openintro") # contains lots of datasets to use
# install.packages("styler") # to get the linting/formatting package

# load packages for
library("dplyr") 
library("openintro")


# ---- examples ----

### structure:
# for (<var> in <values>) {
#   
#   # <do the things>
#   
# }


# e.g.,

for (i in 1:3) { # i <- 1
  
  # print(i)
  print(paste("The variable i has the value:", i))
  
}



# doesn't have to be numbers, can be a character vector to iterate over
for (i in c("rabbit", "dog", "cat")) { 
  
  # print(i)
  print(paste("The variable i has the value:", i))
  
}




# neater way

animals <- c("rabbit", "dog", "cat")
for (i in animals) { 
  
  print(i)
  # print(paste("The variable i has the value:", i))
  
}



# animals example but iterating over the indexes (element numbers)

animals <- c("rabbit", "dog", "cat")
indxs <- 1:length(animals)
for (a in indxs) { 
  
  print(a)
  print(animals[a])
  # print(paste("The variable i has the value:", i))
  
}


# nested loops for those feeling a bit fancy
for (i in 1:3) { # i <- 1
  
  print(paste("The variable i has the value:", i))
  
  for (j in 1:4) {
    
    # print(i)
    print(paste("The variable j has the value:", j))
    
  }
  
}



# ---- testing_loops ----

# when things don't work, look at each repetition individually 
# (and print out intermediate results)





# ---- advanced ----

### let's extend what we did last week
# we creaeted a function to centre and scale variables
# now we know how to iterate over a list/vector, let's apply the function in a loop

# load data ("High School and Beyond survey")
data(hsb2)

# peek at data
hsb2

# create a copy of the data to manipulate without fear or favour
hsb_edit <- hsb2

# samsies
head(hsb2)
head(hsb_edit)

# INPUTS: a vector of numeric scores
# OUTPUTS: a vector of the same length as the input but with the values centred and scaled
# note that the function body look very similar to what we previously did
standardise_test_scores <- function(scores) {
  
  var_mean <- mean(scores)
  var_sd <- sd(scores)
  scores_stdardised <- (scores - var_mean) / var_sd
  
  return(scores_stdardised)
  
}

# this is how use used our function last week (no looping)
hsb_edit[["read_stdised"]] <- standardise_test_scores(hsb2[["read"]])
hsb_edit[["write_stdised"]] <- standardise_test_scores(hsb2[["write"]])
hsb_edit[["math_stdised"]] <- standardise_test_scores(hsb2[["math"]])
hsb_edit[["science_stdised"]] <- standardise_test_scores(hsb2[["science"]])
hsb_edit[["socst_stdised"]] <- standardise_test_scores(hsb2[["socst"]])



# can use loops to do the above!
# plus some plotting stuff :-)
par(mfrow = c(5, 2))

col_names <- c("read", "write", "math", "science", "socst")
for (j in col_names) { # j <- "read"
  
  hist(hsb2[[j]], main = j, xlab = j)
  new_var_name <- paste0(j, "_stdised")
  hsb_edit[[new_var_name]] <- standardise_test_scores(hsb2[[j]])
  hist(hsb_edit[[new_var_name]], main = paste(new_var_name, "of hsb_edit df"))
  
}

par(mfrow = c(1, 1))


# an example of printing the summary stats of each column
for (j in col_names) { 
  
  temp_mean <- mean(hsb2[[j]])
  temp_sd <- sd(hsb2[[j]])
  temp_string <-
    paste(
      "The variable", 
      j, 
      "has mean", 
      round(temp_mean, 1), 
      "and sd", 
      round(temp_sd, 1)
    )
  
  print(temp_string)
  
}


# have a peek again: now different!
head(hsb2)
head(hsb_edit)


# ---- advanced_pro ----

# this example uses BOTH functions and loops to 
# automate reading in csv files!


### INPUT:
# csv_file_nms: a vector of csv files with relative paths (character vector)
### OUTPUT:
# a data.frame that contains all the imported csv files binded row-wise
### ASUMPTIONS:
# all the csvs have header rows and exactly the same columns names (otherwise errors will occur)

rowwise_bind_csvs <- function(csv_file_nms) {
  
  # this is the number of files to import
  n_files <- length(csv_file_nms)
  
  out_df <- data.frame() # start with an empty data.frame that we will keep adding to
  
  # read in each csv and the add data.frame to what has already been read in
  for (i in 1:n_files) {
    df_i <- read.csv(csv_file_nms[i]) # get the i^th dataset
    out_df <- rbind(out_df, df_i)     # append the i^th dataset to the rest of data
  }
  
  return(out_df)
  
}


# these are files in the dat folder
# should be: "card_1.csv" "card_2.csv" "card_3.csv" "card_4.csv" (in 2022-11-02_loops/dat/ directory)
csv_list <- list.files(path = "2022-11-02_loops/dat", full.names = TRUE)  
csv_list

# use our function that puts the 4 x 1-line csvs together
rowwise_bind_csvs(csv_list)


### how I created the csvs, pls ignore!
# deck <- read.csv("2022-07-20_tidyverse/dat/deck.csv")
# set.seed(1234)
# row_samp <- sample(nrow(deck), size = 4)
# for (i in 1:4) { # i <- 1
#   write.csv(deck[row_samp[i], ], file = paste0("2022-11-02_loops/dat/card_", i, ".csv"))
# }



