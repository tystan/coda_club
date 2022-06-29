
# download R from: https://cran.r-project.org

# download RStudio from: https://www.rstudio.com/products/rstudio/

# note that the code repo is at https://github.com/tystan/coda_club


# create a vector of 3 numbers:
1:3

# calculate the mean of those three numbers
mean(1:3)

# median even!
median(1:3)

# see help file for mean() function
?mean

# are there other mean() functions or similar?
??mean


# install packages without code:
# go "Tools -> Install packages..."

# To check where packages will be installed:
.libPaths()

### install packages using code (only have to install once on machine):

# explicitly demand the dependencies are installed beforehand
install.packages("dplyr", dependencies = TRUE)

# implicitly dependencies are installed here as well by default
install.packages("dplyr")

# install another package
install.packages("tidyr")

# to load a package in an R session, use library("package-name")
# note this need to be done every session so "library(...)" statements
# are best to go at the top of your scripts so you do this each time :-)
library("dplyr")




