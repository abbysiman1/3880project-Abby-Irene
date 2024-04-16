source("http://www.uvm.edu/~rsingle/stat3880/data/scripts-3880.R")
setwd("/Users/abigailsimanjuntak")
data <- read.csv("2015satisfactiondataset.csv")
colnames(data) <- tolower(colnames(data))

colSums(is.na(data))
# 393 missing values in arrival.delay.in.minutes

#dropping incomplete cases
data <- na.omit(data)

