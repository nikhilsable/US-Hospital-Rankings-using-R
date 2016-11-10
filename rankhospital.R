rankhospital <- function(state, outcome, num = "best"){
  # Read csv for outcome data
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  rdata <- as.data.frame(cbind(outdata[, 2], outdata[, 7], outdata[, 11], outdata[, 17], outdata[, 23]),stringsAsFactors = FALSE)
  colnames(rdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  # Verify valid state data 
  if (!state %in% rdata[, "state"]) {
    stop('invalid state')
  } 
  # Verify valid outcome data 
    else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
    else if (is.numeric(num)) {
    srows <- which(rdata[, "state"] == state)
    ranked <- rdata[srows, ]
    ranked[, eval(outcome)] <- as.numeric(ranked[, eval(outcome)]) #setting up data as numeric
    k <- ranked[, eval(outcome)]
    ranked <- ranked[order(k, ranked[, "hospital"]), ]
    answer <- ranked[, "hospital"][num]
  } else if (!is.numeric(num)){
    if (num == "best") {
      answer <- best(state, outcome)
    } else if (num == "worst") {
      srows <- which(rdata[, "state"] == state)
      ranked <- rdata[srows, ]    
      ranked[, eval(outcome)] <- as.numeric(ranked[, eval(outcome)])
      k <- ranked[, eval(outcome)]
      ranked <- ranked[order(k, ranked[, "hospital"], decreasing = TRUE), ] #order decreasing to handle ties
      answer <- ranked[, "hospital"][1]
    } 
  }
  return(answer)
}