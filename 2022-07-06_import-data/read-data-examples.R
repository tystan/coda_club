
# ---- preamble ----

# https://stats.oarc.ucla.edu/other/annotatedoutput/ 
# https://stats.oarc.ucla.edu/other/dae/

# ---- last_episode ----

# https://rstudio-education.github.io/hopr/r-objects.html#data-frames
df1 <- 
  data.frame(
    face = c("ace", "two", "six"),  
    suit = c("clubs", "clubs", "clubs"), 
    value = c(1, 2, 3)
  )

df1
df1$face
df1[["face"]]
df1[, "face"]

# ---- libs ----


### only need to do once
# install.packages("haven")
# install.packages("foreign")
# install.packages("readr")
# install.packages("arrow")

# load packages for
library("haven") # Import and Export 'SPSS', 'Stata' and 'SAS' Files
library("foreign") # Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata',...
library("readr") # 'readr': to read rectangular data (like 'csv', 'tsv', and 'fwf')
library("readxl") # read xls/xlsx
library("arrow") # 'Apache Arrow' is a cross-language utility for in-memory data

# ---- paths ----

getwd()
list.files()

# abs path
"C:/Users/ty/Documents/Github/coda_club/2022-07-06_import-data/"
"~/Github/coda_club/2022-07-06_import-data/"

path.expand("~/Github/coda_club/2022-07-06_import-data/")
normalizePath("~/Github/coda_club/2022-07-06_import-data")
normalizePath("~/Github/coda_club/2022-07-06_import-data", winslash = "/")


# relative path
"../some_data.csv"
normalizePath("../", winslash = "/")
"data/data2/data3/text.txt"

normalizePath("../some_data.csv", winslash = "/")



# ---- reading ----

# csv

crime <- read.csv("~/GitHub/coda_club/2022-07-06_import-data/data/crime.csv")

crime <- read_csv("2022-07-06_import-data/data/crime.csv")

c(
"sid","state","crime",
"murder","pctmetro","pctwhite",
"pcths","poverty","single"
)


# tsv


# general delimeted

# xls/xlsx

# stata


# sas


# spss



# load

cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")


# ---- writing ----


# csv

write_csv
write.csv

# tsv

write_delim()


write_sas()

write.dta()



write_parquet()



save(crime, file = "2022-07-06_import-data/data/crime.RData")

