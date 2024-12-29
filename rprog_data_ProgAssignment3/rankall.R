# Calls package knitr
rankall <- function(outcome, num = "best") {
  library(knitr)
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  if(!(num == "best" | num == "worst" | is.numeric(num))) stop("invalid num")
  
  ## For each state, find the hospital of the given rank
  mort_label <- if (identical(outcome, "heart attack")){
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (identical(outcome, "heart failure")){
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else { # pneumonia
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  suppressWarnings({
    state_outcome <- split(data.frame(mortality_rt = as.numeric(outcome_data[[mort_label]]),
                              Hospital.Name = outcome_data$Hospital.Name), outcome_data$State)
  })
  
  state_hos <- data.frame(hospital = character(length(state_outcome)), state = names(state_outcome))
  for (state_name in state_hos$state){
    state_data <- state_outcome[[state_name]][!is.na(state_outcome[[state_name]]$mortality_rt),]
    
    if (num == "best") val = 1
    else if (num == "worst") val = nrow(state_data)
    else val = num
    rank <- order(state_data$mortality_rt,state_data$Hospital.Name)[val]
    
    state_hos[[which(state_hos$state == state_name),"hospital"]] = state_data[rank,"Hospital.Name"]
  }

  ## Return a data frame with the hospital names and the (abbreviated) state name
  state_hos

}
