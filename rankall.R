## R-Programming #031 - Course Programming Assignment 3 - Hospital Data Quality
## T. Debus                                               Aug. 28th 2015
##

## Initializing & Generic Data Loading

raw_data <- read.csv("~/Documents/workspace/RStudio/Training/RProg-031/PA-3/hospital-data/outcome-of-care-measures.csv", colClasses='character')

## Part 4 - Show hospitals for outcome across all states (function)

rankall <- function(outcome, num = "best") {
  
## Function called rankall takes two arguments: an outcome (outcome) and the intended ranking (num)
## Return hospital names across all states that matches the rank given for an outcome
  
  print(class(num))
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  possible <- c("heart attack", "heart failure", "pneumonia") 
  if(!(outcome %in% possible)){
    stop("invalid outcome")
  }
  else
    {
    diseaseCol <- 0
    if(outcome=="heart attack"){
      diseaseCol <- 11
    }
    else if(outcome == "heart failure"){
      diseaseCol <- 17
    }
    else diseaseCol <- 23
    
    hospitalNamesCol <- 2
    
    for(i in 1:nrow(raw_data)){  
      if(raw_data[i, diseaseCol]=="Not Available"){ 
        raw_data[i, diseaseCol] <- NA 
      }
    }  
    r <-  split(raw_data, raw_data$State)  
    hNames <- vector()
    sNames <- vector()
    for(i in 1:length(r)){ 
      nas <- sum(is.na(r[[i]][,diseaseCol]))
      if(is.numeric(num) && num>(length(r[[i]][,2])-nas)){
        hNames <- c(hNames, NA) 
        sNames <- c(sNames, r[[i]][1, 7])
        next
      }
      r[[i]][,diseaseCol] <- as.numeric(r[[i]][,diseaseCol])
      
      r[[i]][,] <- r[[i]][order(r[[i]][, diseaseCol], r[[i]][, hospitalNamesCol]), ] 
      
      if(num=="best")hNames <- c(hNames, r[[i]][1, 2])
      else if(num=="worst")hNames <- c(hNames, r[[i]][length(r[[i]][,2])-nas, 2])
      else hNames <- c(hNames, r[[i]][num, 2])
      
      sNames <- c(sNames, r[[i]][1, 7])
    }
    dataframe <- data.frame(hospital = hNames, state = sNames)
  }
  dataframe 
  
  }



