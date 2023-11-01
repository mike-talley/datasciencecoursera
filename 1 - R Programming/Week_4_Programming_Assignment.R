## Set wd
setwd("~/Documents/R/Coursera/datasciencecoursera/1 - R Programming")

## Read and inspect outcome data
outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
  # 46col x 4706row

## Create a histogram of heart attack death rates
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

###################################################################################

## Part 2: Find the best hospital in the state
  
## Create a function named 'best' with two args (2char state name, and outcome name)
  
best <- function(state, outcome) {
  
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  statelist <- vector()
  
  if(outcome == "heart attack") {}
    else if(outcome == "heart failure") {}
    else if(outcome == "pneumonia") {}
    else stop("invalid outcome")
  
  ## Read outcome data
  data <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
      na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check for valid state and outcome values, and rename columns for ease of use
      data <- data[, c(2,7,outcomes[outcome])]
      completeresults <- data[complete.cases(data), ]
      names(completeresults) <- c("hospital", "state", "outcome")
      statelist <- completeresults[, 2]
      
  ## Arrange results by state, then mortality rate
      arrangedresults <- completeresults[order(
        completeresults$state, completeresults$outcome, completeresults$hospital), ]
      splitresults <- split(arrangedresults ,arrangedresults$state)[state]
  
  ## Return best hospital in the state with lowest 30d death rate
      besthospital <- sapply(splitresults, function(x) x$hospital[1])
  
  if(state %in% statelist) {}      
      else stop("invalid state")
      
  ## Optional line to return the mortality rate for the hospital
  ## rate <- sapply(splitresults, function(x) x$outcome[1])
  ## output <- c(besthospital, rate)
  
  as.character(besthospital)
}

###############################################################################

#Part 3: Rank hospitals and pick any one of them based on 30d mortality

rankhospital <- function(state, outcome, num = "best") {
  
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  statelist <- vector()
  
  if(outcome == "heart attack") {}
    else if(outcome == "heart failure") {}
    else if(outcome == "pneumonia") {}
    else stop("invalid outcome")
  
  ## Read outcome data
  data <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
      na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check for valid state and outcome values, and rename columns for ease of use
  data <- data[, c(2,7,outcomes[outcome])]
  completeresults <- data[complete.cases(data), ]
  names(completeresults) <- c("hospital", "state", "outcome")
  statelist <- completeresults[, 2]
  
  ## Arrange results by state, then mortality rate, then subset based on state argument
  completeresults <- completeresults[order(completeresults$outcome, completeresults$hospital), ]
  splitresults <- split(completeresults, completeresults$state)[state]
  
  ## Return hospital based on its rank for 30d death rate
  if(num == "best") {
    hospitalRank <- sapply(splitresults, head, 1)
    hospitalRank <- hospitalRank[1, 1]
  }
  else if(num == "worst") {
    hospitalRank <- sapply(splitresults, tail, 1)
    hospitalRank <- hospitalRank[1, 1]
  }
  else {
    hospitalRank <- sapply(splitresults, function(x) x[num, 1])
  }
  
  if(state %in% statelist) {}      
    else stop("invalid state")
  
  as.character(hospitalRank)
}

#######################################################################

## Part 4: Rank top hospital in each state based on outcome

rankall <- function(outcome, num = "best") {
  
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  df <- data.frame()
  
  if(outcome == "heart attack") {}
  else if(outcome == "heart failure") {}
  else if(outcome == "pneumonia") {}
  else stop("invalid outcome")
  
  ## Read outcome data
  data <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                   na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check for valid state and outcome values, and rename columns for ease of use
  data <- data[, c(2,7,outcomes[outcome])]
  names(data) <- c("hospital", "state", "outcome")
  data <- data[order(data$outcome, data$hospital), ]
  
  splitresults <- split(data, data$state)
  
  if(num == "best") {
    hospital <- sapply(splitresults, function(x) x[1, 1])
  }
  else if(num == "worst") {
    completeresults <- data[complete.cases(data), ]
    splitcomplete <- split(completeresults, completeresults$state)
    hospital <- sapply(splitcomplete, function(x) x[nrow(x), 1])
  }
  else {
    hospital <- sapply(splitresults, function(x) x[num, 1])
  }
  
  state <- names(hospital)
  output <- data.frame(hospital, state)
  output
}
