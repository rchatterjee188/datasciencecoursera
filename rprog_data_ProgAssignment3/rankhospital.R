# Calls package knitr
rankhospital <- function(state, outcome, num = "best") {
  library(knitr)
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state, outcome, num are valid
  if(! state %in% outcome_data$State) stop("invalid state")
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  if(!(num == "best" | num == "worst" | is.numeric(num))) stop("invalid num")
  
  ## Return hospital name in that state with given rank 30-day death rate
  state_ind <- which(outcome_data$State == state)
  
  suppressWarnings({
    mort_data <- if (identical(outcome, "heart attack")){
      as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[state_ind])
    } else if (identical(outcome, "heart failure")){
      as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[state_ind])
    } else { # pneumonia
      as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[state_ind])
    }
  })
  names(mort_data) <- outcome_data$Hospital.Name[state_ind]
  mort_data <- mort_data[!is.na(mort_data)]
  
  if (num == "best") num = 1
  else if (num == "worst") num = length(mort_data)
  else if (num > length(mort_data)) return(NA)
  
  names(mort_data[order(mort_data,names(mort_data))[num]])
}