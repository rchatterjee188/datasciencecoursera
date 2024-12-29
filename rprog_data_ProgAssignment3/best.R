# Calls package knitr
best <- function(state, outcome) {
  library(knitr)
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! state %in% outcome_data$State) stop("invalid state")
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
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
  
  sort(names(mort_data[which(mort_data == min(mort_data[!is.na(mort_data)]))]))[1]
}