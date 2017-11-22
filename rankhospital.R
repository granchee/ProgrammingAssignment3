rankhospital <- function(state, outcome, num = "best") {
  
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
    sorteds <- completes[order(completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, completes$Hospital.Name), ]
  } else if (outcome == "heart failure") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    sos <- oc[oc$State == state, c("Hospital.Name", col_name)]
    completes <- sos[complete.cases(sos[, 2]), ]
    sorteds <- completes[order(completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, completes$Hospital.Name), ]
  } else if (outcome == "pneumonia") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    sos <- oc[oc$State == state, c("Hospital.Name", col_name)]
    completes <- sos[complete.cases(sos[, 2]), ]
    sorteds <- completes[order(completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia, completes$Hospital.Name), ]
  } else {
    
    stop("invalid outcome")
  }
  
  # Return hospital name in desired state with desired 30-day death rate's desired rank.
  row_max <- nrow(sorteds)
  if (num == "best") {
    row_idx <- 1
  } else if (num == "worst") {
    row_idx <- row_max
  } else if ((num >= 1) && (num <= row_max)) {
    row_idx <- num
  } else {
    return(NA) # Bail out if num is bad.
  }
  return(sorteds[row_idx, 1])
}
