## R-Programming #031 - Course Programming Assignment 3 - Hospital Data Quality
## T. Debus                                               Aug. 28th 2015
##

## Initializing & Generic Data Loading

outcome <- read.csv("~/Documents/workspace/RStudio/Training/RProg-031/PA-3/hospital-data/outcome-of-care-measures.csv")



## Part 1 - Heart Attack mortality rates

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

