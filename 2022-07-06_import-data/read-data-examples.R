
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
# install.packages("readxl")
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
"C:/Users/ty/Documents/Github/coda_club/2022-07-06_import-data/data/datadata/datadatadata/"
"~/Github/coda_club/2022-07-06_import-data/"

path.expand("~/Github/coda_club/2022-07-06_import-data/")
normalizePath("~/Github/coda_club/2022-07-06_import-data")
normalizePath("~/Github/coda_club/2022-07-06_import-data", winslash = "/")


# relative path
"../some_data.csv"
normalizePath("../", winslash = "/")
normalizePath("../some_data.csv", winslash = "/")

# ---- reading ----

# csv

crime <- read.csv("~/GitHub/coda_club/2022-07-06_import-data/data/crime.csv")

crime <- read_csv("2022-07-06_import-data/data/crime.csv")

crime_no_header <-
  read.csv(
    "~/GitHub/coda_club/2022-07-06_import-data/data/crime_no_header.csv",
    header = FALSE
  )


# tsv

crime <- read.delim("~/GitHub/coda_club/2022-07-06_import-data/data/crime.txt")

# general delimited

crime_differnt_delim <- 
  read_delim(
    "2022-07-06_import-data/data/crime_differnt_delim.txt",
    delim = "|",
    escape_double = FALSE,
    trim_ws = TRUE
  )

# xls/xlsx

crime <- read_excel("2022-07-06_import-data/data/crime.xlsx")

# stata

hsb2 <- read_dta("2022-07-06_import-data/data/hsb2.dta")

# sas

?read_sas
hsb2demo <- read_sas("2022-07-06_import-data/data/hsb2demo.sas7bdat", NULL)

# spss

hsb2 <- read_sav("2022-07-06_import-data/data/hsb2.sav")

# Rdata

load("2022-07-06_import-data/data/crime.RData", verbose = TRUE)

# save some objects as RData file
x <- 1
save(
  crime,
  hsb2demo,
  x,
  file = "2022-07-06_import-data/data/multiple_objs.RData"
)

# load the saved objects
load("2022-07-06_import-data/data/multiple_objs.RData", verbose = TRUE)
x
crime

# for fast and compressed data you can use this file format:
?read_parquet


# ---- writing ----

# csv

?write_csv
# ?write.csv

write_csv(
  hsb2demo,
  "2022-07-06_import-data/data/datadata/datadatadata/test_export.csv"
)


# tsv

?write_delim

?write_sas

?write.dta

?write_parquet
