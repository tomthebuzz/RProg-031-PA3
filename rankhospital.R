## R-Programming #031 - Course Programming Assignment 3 - Hospital Data Quality
## T. Debus                                               Aug. 28th 2015
##

## Initializing & Generic Data Loading

raw_data <- read.csv("~/Documents/workspace/RStudio/Training/RProg-031/PA-3/hospital-data/outcome-of-care-measures.csv")


## Part 3 - Rank hosiptals by outcome within a state (function)


rankhospital <- function(state, outcome, num = "best") {
  
## Function called best takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num)
  
## Return hospital name that matches the rank given for a certain state and outcome

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

  
# ensure num is not greater than number of hospitals in state
  
  if (is.numeric(num) == TRUE) {
    if (length(sel_data[,2]) < num) {
      return(NA)
    }
  }  
  
# remove NA's in the desired outcome column

  requ_col <- as.numeric(sel_data[,oc_column])
  excl <- is.na(requ_col)
  dec_data <- sel_data[!excl, ]
  
  
# sort dataframe in ascending order of outcome values

  oc_col_name <- names(dec_data)[oc_column]
  hosp_col_name <- names(dec_data)[2]
  index <- with(dec_data, order(dec_data[oc_col_name], dec_data[hosp_col_name]))
  
  ord_dec_data <- dec_data[index, ]

#if num calls for "best" / "worst" map to numerical value (1 / length)
  
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ord_dec_data[, oc_column])
    }
  }
  #return the hospital name with the outcome ranking of num
  c(as.vector(ord_dec_data[num, 2])) 
  
}

