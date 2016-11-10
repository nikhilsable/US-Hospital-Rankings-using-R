rankall <- function(outcome, num = "best"){
  # Read csv for outcome outdata
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  rdata   <- as.data.frame(cbind(outdata[, 2], outdata[, 7], outdata[, 11], outdata[, 17], outdata[, 23]), stringsAsFactors = FALSE)
  colnames(rdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  rdata[, eval(outcome)] <- as.numeric(rdata[, eval(outcome)])
  
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    by_state <- with(rdata, split(rdata, state))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(rdata, split(rdata, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(rdata, split(rdata, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], by_state[[i]][, "hospital"], decreasing = TRUE), ]
        ordered[[i]] <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } 
  }
  return(output)
}