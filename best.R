best <- function(state, outcome) {
  
  # Read outcome data.
  oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  # Check that state and outcome are valid.

  # State.
  states <- unique(oc$State)
  if ((state %in% states) == FALSE) {
    
    stop("invalid state")
  }
  
  # Outcome.
  if (outcome == "heart attack") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    sos <- oc[oc$State == state, c("Hospital.Name", col_name)]
    completes <- sos[complete.cases(sos[, 2]), ]
    min_rate <- min(completes[, 2])
    mins <- completes[completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min_rate, ]
  } else if (outcome == "heart failure") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    sos <- oc[oc$State == state, c("Hospital.Name", col_name)]
    completes <- sos[complete.cases(sos[, 2]), ]
    min_rate <- min(completes[, 2])
    mins <- completes[completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min_rate, ]
  } else if (outcome == "pneumonia") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    sos <- oc[oc$State == state, c("Hospital.Name", col_name)]
    completes <- sos[complete.cases(sos[, 2]), ]
    min_rate <- min(completes[, 2])
    mins <- completes[completes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_rate, ]
  } else {
    
    stop("invalid outcome")
  }

  # Return hospital name in desired state with lowest desired 30-day death rate.
  best_hospitals <- mins[order(mins$Hospital.Name), ]
  best_hospital <- best_hospitals[1, 1]
  return(best_hospital)
}
