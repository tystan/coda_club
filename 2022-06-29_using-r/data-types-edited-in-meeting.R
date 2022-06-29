

# calculator
1 + 2
1 * 2
1 - 2
1 / 2
2 ^ 3
1 + 1 / 2 # bodmas life

# but fancier, you can store values
x <- 1 - 2
# or
x <- -1
# or
x_100t2hdfgjnlkjcfnblkgcjfmn = -2
X <- 3

X
x 


# 3 main data-types (some others but we will ignore at the moment)

# type 1: numeric (aka numbers)
x1 <- 1.3
x1
class(x1) # or typeof(x1)

# type 2: character (aka letters)
x2 <- "1"
x2
class(x2) # or typeof(x2)

# type 3: logical (aka binary/boolean)
x3 <- TRUE # or FALSE
# FALSE <- 2
x3
class(x3) # or typeof(x3)


x1 + x2
x1 + x3 # see: https://rstudio-education.github.io/hopr/r-objects.html#coercion
x2 + x3


# there's built in values
pi
LETTERS
letters

# there's built in functions()
length(x3)
exp(1)
ls()
runif(1)
?rm
rm(list = ls())


# turns out what we have generated above were mainly length 1 vectors
# but we can create vectors of any length
c(1, 2, 3)
my_first_vector <- c(1, 2, 3)
length(my_first_vector)
# other ways to generate a vector:
1:3
1:(-3)
seq(1, 3, by = 1)
seq(0, 100, by = 25)


# mixed data types?
c("a", 2, FALSE)
c("a", 2, FALSE) * 2


# we can do calculator things to vectors as well
my_first_vector + 2
my_first_vector + c(2, 2, 100)
my_first_vector * 2
my_first_vector - 2
my_first_vector / 2
my_first_vector ^ 3
my_first_vector + 1 / 2 # bodmas life again


# extracting and changing elements: vec[i]
my_first_vector[1]
my_first_vector[2]
my_first_vector[3]
my_first_vector[4]
my_first_vector[-2]
my_first_vector[c(TRUE, FALSE, TRUE)]

# change the vector
my_first_vector[2] <- -100
my_first_vector[c(TRUE, FALSE, TRUE)] <- -100
my_first_vector # changed?


# ---- other_data_structures ----

# now have covered (atomic) data types and vectors...
# we have two more data structures to discuss:
# (1) matrix
# (2) data.frame


# ---- matrix ----

# a matrix is just a two dimensional vector/array
matrix(1:4, nrow = 2, ncol = 2)

# has to be all one type like vectors
matrix(c("a", 2, FALSE, TRUE), nrow = 2)

# calculator things again
my_first_mat <- matrix(1:9, ncol = 3)
my_first_mat + 2
my_first_mat * 2
my_first_mat - 2
my_first_mat / 2
my_first_mat ^ 3
my_first_mat + 1 / 2 # bodmas life again


# extracting and changing elements matrix[i-th-row, j-th-col]
my_first_mat[1, 1]
my_first_mat[1, 3]
my_first_mat[3, 1]
my_first_mat[3, 1:2]


my_first_mat[3, 1] <- -100
my_first_mat

# entire column
my_first_mat[, ]
my_first_mat[, 2]
# entire row
my_first_mat[2, ]
my_first_mat[2, , drop = FALSE]
my_first_mat[2, 1:3]
my_first_mat[2, c(TRUE, FALSE, TRUE)]

class(my_first_mat[2, , drop = FALSE])
class(my_first_mat[2, , drop = TRUE])
class(my_first_mat[2, ])


# ---- data_frames ----

# https://rstudio-education.github.io/hopr/r-objects.html#data-frames
df1 <- 
  data.frame(
    face = c("ace", "two", "six"),  
    suit = c("clubs", "clubs", "clubs"), 
    value = c(1, 2, 3)
  )


df1
df1$face
df1$`face`
df1[["face"]]
df1[, 1]

df1$value <- df1$value * 3
df1


df2 <- df1[2, 1:3]
class(df2)





