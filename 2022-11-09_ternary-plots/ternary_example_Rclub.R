# ---- libs ----

# useful packages from the "tidyverse" (https://www.tidyverse.org/packages/)

# note that you don't need all of these for this script, I just have a generic few lines of code to load packages :)
library("dplyr") # manipulate data
library("tidyr") # cousin of dplyr
library("readr") # read data in from csvs etc
library("ggplot2") # plotting functions
library("tibble") # this is loaded by dplyr but good to know it exists (it's enhanced data.frames)
library("lubridate") # way to handle dates better than default R way

# other useful packages
library("compositions") # CoDA package
library("foreach") # a faster/neater way to loop over variables in R
library("knitr") # specifically the function kable() prints tables nicely
library("skimr") # only recently found out this package: skim(dataset) summarises the data
library("GGally") 
library("car")


# load data

load('data/FinalData/final_dat.RData', verbose = TRUE)

# clean and refine data here

#Look for any invalid files (coded as NA or 0, as 1=valid)
table(final_dat$valid_file, useNA = 'ifany')
final_dat <- final_dat[final_dat$valid_file %in% 1, ] #remove participants that don't have valid accelerometry files
table(final_dat$valid_file, useNA = 'ifany') #check that they were removed 


#create new dataframe which only contains time use data
cols_pred <- c("all_days_sleeptime", "all_days_sedtime", "all_days_lighttime", "all_days_mvtime")
act_dat <- final_dat[, cols_pred]

#remove all NA values from act_dat
nrow(act_dat)
act_dat <- na.omit(act_dat) 
nrow(act_dat)

# 24-hour day == 1440 minutes
#rowSums = sums values in specified columns for ALL rows- to check if they add up to roughly 1440 
rowSums(final_dat[, 
                  c("all_days_sleeptime", "all_days_sedtime", "all_days_lighttime", "all_days_mvtime")
])

hist(rowSums(act_dat[, cols_pred])) #create histogram of all rows but only predictor variable columns

act_dat[rowSums(act_dat[, cols_pred]) > 1500, ] #show which rows add up to >1500 mins
act_dat[rowSums(act_dat[, cols_pred]) < 1400, ] #show which rows add up to <1400 mins

# locate participants with >1500 mins time use
final_dat[rowSums(final_dat[, cols_pred]) > 1500, ]

# get rows that are not NAs _AND_ have time use less than 1400 minutes
final_dat[
  !(is.na(rowSums(final_dat[, cols_pred]))) & rowSums(final_dat[, cols_pred]) < 1400, 
  c("id", "all_days_weartime", cols_pred)
]

# check that wear time is equal to non-sleeping time (columns should add up to same amount)
cbind(
  rowSums(final_dat[, c("all_days_sedtime", "all_days_lighttime", "all_days_mvtime")]),
  final_dat[,"all_days_weartime" ]
)

#calculate difference in wear time and total waking activity
sort(
  rowSums(final_dat[, c("all_days_sedtime", "all_days_lighttime", "all_days_mvtime")]) -
    final_dat[["all_days_weartime" ]]
) #all good :)


table(is.na(final_dat$all_days_weartime)) #check if there are any NA values in wear time column
hist(final_dat$all_days_weartime) 
final_dat$valid_file


# get rid of participants with more than 1500 minutes of time use data
nrow(act_dat)
act_dat <- act_dat[!(rowSums(act_dat[, cols_pred]) > 1500), ]
nrow(act_dat) # should be 2 less


#closure function (compositions package) - rescales so that all participants have 1440 mins time use
?clo
act_dat[, cols_pred] <- clo(act_dat[, cols_pred], total = 1440)
rowSums(act_dat[, cols_pred]) #checks that all participants have 1440 mins

#sum light PA and MVPA to create a total PA (tpa) variable
act_dat$tpa <- act_dat$all_days_lighttime + act_dat$all_days_mvtime

#create 'compositiondata' dataframe that only contains the three time-use variables you need for the plot
compositiondata <- act_dat %>% select(all_days_sleeptime, all_days_sedtime, tpa)

#make sure required packages for creating ternary plots are loaded

require(compositions)
require(ggplot2)
#install.packages("ggtern")
require(ggtern)

#express time spent in each behaviour as a proportion of the day
#we aren't creating a new dataframe at this stage - just to demonstrate what 'acomp' does

?acomp
acomp(compositiondata)

#First, this code performs the above 
#Then, finds the mean proportion of the day spent in each behaviour across all participants

m <- mean(acomp(compositiondata))

#convert composition data to a string
str(compositiondata)
compositiondata <- as.data.frame(compositiondata)

?ggtern

#plot the data as a ternary diagram
ggtern(data = compositiondata, aes(x = compositiondata[,1], y = compositiondata[,3], z = compositiondata[,2])) +
  geom_point( size=1.2, colour="darkgrey")+
  labs(x="Sleep",
       y="PA",
       z="SB")+
  annotate(geom='point',
           x=m[1],y=m[3],z=m[2],
           shape=16, size=4, colour="black")+
  geom_confidence_tern(col="black", alpha=1, size=0.3,breaks = c( 0.75, 0.95, 0.99))+
  theme_showarrows() + 
  theme_rgbw()  +
  #tweak guides
  guides(size = guide_legend(order   =2))+
  theme_clockwise()

# calculate compositional means of each time-use variable
mean(comp$all_days_sedtime)
mean(comp$all_days_sleeptime)
mean(comp$tpa)

clo(m, total=1440) 

