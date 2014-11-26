rankall <- function(outcome, num = "best") {
  ## Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
  pos_num <- c("best", "worst")
  
  ## Check that outcome is valid
  
  if (! (outcome %in% pos_outcomes)){
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  rankInState <- function(state, outcome, num = "best"){
    csv_state <- csv[csv$State == state,]
    
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
  
  states <- unique(csv$State)
  order_states <- states[order(states)] ## Sorts states in alphabetical order
  output <- sapply(order_states, FUN=rankInState, outcome = outcome, num =num)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=output, state=order_states)
}