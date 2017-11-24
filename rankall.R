get_ranked_hospital <- function(state, sorteds, num) {
  
  observations <- sorteds[sorteds$State == state, ]
  row_max <- nrow(observations)
  
  if (num == "best") {
    row_idx <- 1
  } else if (num == "worst") {
    row_idx <- row_max
  } else if ((num >= 1) && (num <= row_max)) {
    row_idx <- num
  } else {
    return(NA) # If ranked hospital doesn't exist for state.
  }
  
  observations[row_idx, 1] # Return Hospital.Name if ranked hospital exists.
}

rankall <- function(outcome, num = "best") {
  
  # Read outcome data.
  oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # For every valid outcome do necessary; stop if invalid.
  if (outcome == "heart attack") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    slim <- oc[, c("Hospital.Name", "State", col_name)]
    completes <- slim[complete.cases(slim[, 3]), ]
    sorteds <- completes[order(completes$State, completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, completes$Hospital.Name), ]
  } else if (outcome == "heart failure") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    slim <- oc[, c("Hospital.Name", "State", col_name)]
    completes <- slim[complete.cases(slim[, 3]), ]
    sorteds <- completes[order(completes$State, completes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, completes$Hospital.Name), ]
  } else if (outcome == "pneumonia") {
    
    col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    col_idx <- which(colnames(oc) == col_name)
    oc[, col_idx] <- as.numeric(oc[, col_idx])
    slim <- oc[, c("Hospital.Name", "State", col_name)]
    completes <- slim[complete.cases(slim[, 3]), ]
    sorteds <- completes[order(completes$State, completes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, completes$Hospital.Name), ]
  } else {
    
    stop("invalid outcome")
  }
  
  # Return data frame with result (or NA) for each state.
  states <- sort(unique(oc$State))
  hospitals <- lapply(states, get_ranked_hospital, sorteds, num)
  hospitals_states <- cbind(hospitals, states)
  df_hospitals_states <- as.data.frame(hospitals_states)
  colnames(df_hospitals_states) <- c("hospital", "state")
  
  df_hospitals_states
}
