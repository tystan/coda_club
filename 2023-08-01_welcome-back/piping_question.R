

# create a vector with 4 numbers
1:4

# calculate the mean value
mean(1:4)

# calculate the mean value AND save it to `x`
x <- mean(1:4)

# this is the piping way to calc the mean
1:4 |> mean()

# if you want to look at the default R piping help file
?`|>`

# import the package "dplyr" (assumes it is already installed)
# install.packages("dplyr")
library("dplyr")

# if you want to look at the "tidyverse"/dplyr piping help file
?`%>%`

# look at the first 6 rows of the dataset mtcars
head(mtcars)

# look at the first 6 rows of the dataset mtcars: piping stylez
mtcars |> head()                      

# look at the first 6 rows of the dataset mtcars: tidyverse piping stylez
mtcars %>% head() 

# look at the first 6 rows of the dataset mtcars: tidyverse piping stylez
# AND save the result as new_mtcars
new_mtcars <- mtcars %>% head() 





