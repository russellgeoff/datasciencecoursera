best <- function(state, outcome) {
  ## Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!(state %in% csv$State)){
    stop("invalid state")
  }
  
  if (! (outcome %in% pos_outcomes)){
    stop("invalid outcome")
  }
  
  csv_state <- csv[csv$State == state,]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (outcome == "heart attack"){
    index  <- match(x = min(as.numeric(csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE),
                    table = csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    bestHName <- csv_state$Hospital.Name[index]
    bestHName
  }
  else if (outcome == "heart failure"){
    index  <- match(x = min(as.numeric(csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE),
                    table = csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    bestHName <- csv_state$Hospital.Name[index]
    bestHName
  }
  else{
    # pneumonia
    index  <- match(x = min(as.numeric(csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE),
                    table = csv_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    bestHName <- csv_state$Hospital.Name[index]
    bestHName
  }
}