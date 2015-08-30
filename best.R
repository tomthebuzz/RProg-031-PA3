## R-Programming #031 - Course Programming Assignment 3 - Hospital Data Quality
## T. Debus                                               Aug. 28th 2015
##

## Initializing & Generic Data Loading

raw_data <- read.csv("~/Documents/workspace/RStudio/Training/RProg-031/PA-3/hospital-data/outcome-of-care-measures.csv")


## Part 2 - Find the best hosiptal in a state (function)


best <- function(state, outcome) {

## Function called best takes two arguments: the 2-character abbreviated name of a state and 
## an outcome name ("heart attack", "heart failure", "pneumonia")
  
## Return hospital name in that state with lowest 30-day death rate & exclude NA outcomes

# check if states and outcomes are valid

states <- raw_data[ , 7]
val_outcome <- c("heart attack", "heart failure", "pneumonia")

if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  }
  else if ((outcome %in% val_outcome) == FALSE) {
    stop(print("invalid outcome"))
  }
  
# get the relevant data for the desired state
  sel_data <- subset(raw_data, State == state)
  
# get the relevant outcome column from the data file
  if (outcome == "heart attack") {
    oc_column <- 11
  }
  else if (outcome == "heart failure") {
    oc_column <- 17
  }
  else if (outcome == "pneumonia") {
    oc_column <- 23
  }
  else {
    stop(print("Outcome Selection Error"))
  }

  
# remove NA's in the desired outcome column

  requ_col <- as.numeric(sel_data[,oc_column])
  excl <- is.na(requ_col)
  dec_data <- sel_data[!excl, ]
  
# find hospitals with minimum outcome value

  considered <- as.numeric(dec_data[, oc_column])
  desired_rows <- which(considered == min(considered))
  best_hosp <- dec_data[desired_rows, 2]
  
# if multiple hospitals share the minimum outcome, then return
# the first hospital name from the alphabetically sorted list

    if (length(best_hosp) > 1) {
      best_hosp_sort <- sort(best_hosp)
      as.vector(best_hosp_sort[1])
    }
  else {
    as.vector(best_hosp)
  }
}

