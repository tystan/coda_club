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

ternarydat <- read_csv("C://Users//melml004//OneDrive - University of South Australia//Fairclough.csv", col_names=TRUE) # read.csv

#look through column names
colnames(ternarydat)

#create new dataframe which only contains time use data
cols_pred <- c("Sedentary", "sleep", "Light", "MVPA")
ternaryplot <- ternarydat[, cols_pred]

#remove all NA values from ternaryplot
nrow(ternaryplot)
ternaryplot <- na.omit(ternaryplot) 
nrow(ternaryplot)

# 24-hour day == 1440 minutes
#rowSums = sums values in specified columns for ALL rows- to check if they add up to roughly 1440 
rowSums(ternaryplot[, 
                  c("Sedentary", "sleep", "Light", "MVPA")
])

hist(rowSums(ternaryplot[, cols_pred])) #create histogram of all rows but only predictor variable columns

#closure function (compositions package) - rescales so that all participants have 1440 mins time use
?clo
ternaryplot[, cols_pred] <- clo(ternaryplot[, cols_pred], total = 1440)
rowSums(ternaryplot[, cols_pred]) #checks that all participants have 1440 mins

#sum light PA and MVPA to create a total PA (tpa) variable
ternaryplot$tpa <- ternaryplot$Light + ternaryplot$MVPA

#create 'compositiondata' dataframe that only contains the three time-use variables you need for the plot
compositiondata <- ternaryplot %>% select(sleep, Sedentary, tpa)

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
mean(compositiondata$Sedentary)
mean(compositiondata$sleep)
mean(compositiondata$tpa)

clo(m, total=1440) 

