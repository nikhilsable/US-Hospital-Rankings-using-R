best <- function(state, outcome) {
  # Read csv for outcome data
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  bdata <- as.data.frame(cbind(outdata[, 2], outdata[, 7], outdata[, 11], outdata[, 17], outdata[, 23]), stringsAsFactors = FALSE)
  colnames(bdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  # Verify valid state data 
  if(!state %in% bdata[, "state"]){
    stop('invalid state')
  } 
  # Verify valid outcome data 
    else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
    else {
    srows <- which(bdata[, "state"] == state) # integer vector for all rows where state == the input state
    finset <- bdata[srows, ]   # a new final set of data from our bdata date frame and srows
    outval <- as.numeric(finset[, eval(outcome)]) #a numeric vector of all values for the outcome
    min_val <- min(outval, na.rm = TRUE) # calculating the min value for lowest 30 day mortality for outcome 
    result  <- finset[, "hospital"][which(outval == min_val)] #compute value for the corresponding hospital name
    send  <- result[order(result)] # order to handle ties
  }
  return(send)
}