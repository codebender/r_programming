rankall <- function(outcome, num = "best") {
  ## Check that outcome is valid
  if (is.null(outcome)) {
    stop("No 'outcome' value defined.")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }

  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  state_mort_name_sorted <- NULL
  if(outcome == 'heart attack') {
    outcome_data$mort_col <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }
  else if(outcome == "heart failure") {
    outcome_data$mort_col <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }
  else {
    outcome_data$mort_col <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }

  ## remove NAs
  outcome_data <- outcome_data[!is.na(outcome_data$mort_col),]

  ## sorts by State, Mortality, Hospital name
  state_mort_name_sorted <- outcome_data[with(outcome_data, order(State, mort_col, Hospital.Name)),]

  ## split sorted data by state
  states <- split(state_mort_name_sorted, state_mort_name_sorted$State)

  hospital_names <- NULL
  for(state in states) {
    if(num == 'best') {
      hospital_names <- c(hospital_names, state$Hospital.Name[1])
    }
    else if(num == 'worst') {
      hospital_names <- c(hospital_names, tail(state$Hospital.Name, 1))
    }
    else {
      hospital_names <- c(hospital_names, state$Hospital.Name[num])
    }

  }

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=hospital_names, state=names(states))
}
