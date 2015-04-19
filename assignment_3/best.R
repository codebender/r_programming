## best function returns the hospital name in that state with lowest 30-day
## death rate for the given outcome

best <- function(state, outcome) {
  ## Check that state and outcome are valid
  if (is.null(state)) {
    stop("No 'state' value defined.")
  }
  if (!state %in% state.abb) {
    stop("invalid state")
  }
  if (is.null(outcome)) {
    stop("No 'outcome' value defined.")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }

  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Return hospital name in that state with lowest 30-day death rate
  filtered <- outcome_data[outcome_data$State == state,]

  min_mortality_rate <- NULL
  if(outcome == "heart attack") {
    min_mortality_rate <- filtered[which.min(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
  }
  else if(outcome == "heart failure") {
    min_mortality_rate <- filtered[which.min(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
  }
  else {
    min_mortality_rate <- filtered[which.min(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  }

  min_mortality_rate <- min_mortality_rate[order(min_mortality_rate$Hospital.Name),]

  return(min_mortality_rate$Hospital.Name)
}
