rankhospital <- function(state, outcome, num = "best") {
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
  mortaility_sorted <- NULL
  if(outcome == "heart attack") {
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    mortaility_sorted <- filtered[with(filtered, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)),]
    mortaility_sorted <- mortaility_sorted[!is.na(mortaility_sorted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
  }
  else if(outcome == "heart failure") {
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    mortaility_sorted <- filtered[with(filtered, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)),]
    mortaility_sorted <- mortaility_sorted[!is.na(mortaility_sorted$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
  }
  else {
    filtered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filtered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    mortaility_sorted <- filtered[with(filtered, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)),]
    mortaility_sorted <- mortaility_sorted[!is.na(mortaility_sorted$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  }

  if(num == 'best') {
    num <- 1
  }
  else if(num == 'worst') {
    num <- nrow(mortaility_sorted)
  }
  ## Return hospital name in that state with the given rank 30-day death rate
  mortaility_sorted$Hospital.Name[num]
}
