##### SIMPLE GGIR CALL FOR R-CLUB #####
# Written by Erin MacIntyre 
# erin.macintyre@mymail.unisa.edu.au 

#Useful links: 
#https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html - package vignettes
#https://cran.r-project.org/web/packages/GGIR/GGIR.pdf - reference manual (defines ALL variables)
#https://groups.google.com/g/rpackageggir - google group, tons of answers to other peoples questions. You can also ask your own question and Vincent is brillant at responding.
#https://github.com/wadpac/GGIR - GGIR github - download latest beta versions and can delve deep into the code to understand it better 
#https://github.com/ShimmerEngineering/Verisense-Toolbox/tree/master/Verisense_step_algorithm - verisense step count github

#Why GGIR?
#openess/transparancy
#customisable
#reproducable
#more than just MVPA - step counts, cut-point free metrics 

#Why not?
#processing time!!! 
#code can be tricky - but not for you after today :) 

library(GGIR)

# the below file references are relative to the `coda_club` home directory
# (and should automatically work if you opened the `coda_club.Rproj` file)
g.shell.GGIR(
  mode = c(1, 2, 3, 4, 5),
  datadir = "2022-11-16_ggir/actigraphs/emac_1.gt3x",
  outputdir = "2022-11-16_ggir/results",
  desiredtz = "Australia/Adelaide",
  do.report = c(2, 4, 5),
  #=====================
  # Part 1 - loads data and important file features. This is the longest part! 
  # However, once done you can then run part 2-5 only
  #=====================
  f0 = 1, 
  f1 = 1, #defining directories, number of files 
  idloc = 2, #VERY IMPORTANT for actigraph files. GGIR can't put file ID from metadata of gtx files, so this is how you create an ID 
  windowsizes = c(5, 900, 3600), #defining window sizes, kept the same as original code 
  dynrange = 8, #provide dynamic range for accelerometer data to overwrite hardcoded 6 g for GENEA and 8 g for other brands
  do.enmo = TRUE, #ENMO is the main summary measure of acceleration. The value presented is the average ENMO over all the available data normalised per 24 hour cycles, with invalid data imputed by the average at similar timepoints on different days of the week.
  acc.metric = "ENMO", 
  #=====================
  # Part 2 - Data quality analyses and low-level description of signal features 
  # per day and per file. 
  #=====================
  strategy = 1, #VERY IMPORTANT. This directs GGIR how to analyse your data, depends on your accelerometer protocol 
  hrs.del.start = 0,          
  hrs.del.end = 0, #along with strategy, tells GGIR how to process data 
  maxdur = 3, #max number of days expected                 
  includedaycrit = 16, #min hours in a day for it to be valid 
  qwindow = c(0, 24), #To specify windows over which all variables are calculated, 0,24 means that variables will be calculated over 24hr day
  mvpathreshold = c(100),
  bout.metric = 6, #how bout is defined - 6 is most up to date, ref manual for details  
  excludefirstlast = FALSE, #include or exclude first and last day?
  #=====================
  # Part 3 + 4 - together deal with inactive periods and sleep. 3 IDs these 
  # times, then 4 labels then as sleep vs. inactivity, best to use diary to 
  # improve accuracy
  #=====================
  outliers.only = FALSE, #only displays outliers in report
  do.visual = TRUE, 
  loglocation = "logs/basic_diary.csv",
  nnights = 2, #log has 2 nights 
  sleeplogidnum = FALSE, #this and below is just describing the format of the log
  colid = 1, 
  coln1 = 2,
  #=====================
  # Part 5 - Derives sleep and physical activity characteristics by re-using 
  # information derived in part 2, 3 and 4. Total time in intensity categories, 
  # the number of bouts, time spent in bouts and average acceleration (overall 
  # activity) is calculated.
  #=====================
  threshold.lig = c(44.8), threshold.mod = c(100.6),  threshold.vig = c(428.8), #Hildebrand cut-points
  boutcriter = 0.8,        boutcriter.in = 0.9,       boutcriter.lig = 0.8,
  boutcriter.mvpa = 0.8,   boutdur.in = c(1, 10, 30), boutdur.lig = c(1, 10),
  boutdur.mvpa = c(1),
  includedaycrit.part5 = 16, 
  #=====================
  # Visual report
  #=====================
  timewindow = c("MM"), #summary stats calculated from midnight to midnight
  visualreport = TRUE
)






