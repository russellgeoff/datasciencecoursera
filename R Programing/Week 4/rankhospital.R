rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
  pos_num <- c("best", "worst")
  csv_state <- csv[csv$State == state,] ## Get only data for the state in question
  
  ## Check that state and outcome are valid
  if (!(state %in% csv$State)){
    stop("invalid state")
  }
  
  if (! (outcome %in% pos_outcomes)){
    stop("invalid outcome")
  }
  
  if (!is.numeric(num) && !(num %in% pos_num)){
    stop("invalid num")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## Order csv_state by criteria then hosptial name
  
  if (outcome == "heart attack"){
    csv_state <- csv_state[with(csv_state, 
                                order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                                      Hospital.Name, na.last = NA)),]
  }
  else if (outcome == "heart failure"){
    csv_state <- csv_state[with(csv_state, 
                                order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                                      Hospital.Name, na.last = NA)),]
  }
  else{
    # pneumonia
    csv_state <- csv_state[with(csv_state, 
                                order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), 
                                      Hospital.Name, na.last = NA)),]
  }
  
  
  ## Catagorize the prefered ranking
  if (num == "best"){
    head(csv_state$Hospital.Name, n=1)
  }
  else if (num == "worst"){
    tail(csv_state$Hospital.Name, n=1)
  }
  else{
    if (num > length(csv_state)){
      return(NA)
    }
    else{
      csv_state$Hospital.Name[num]
    }
  }
}
