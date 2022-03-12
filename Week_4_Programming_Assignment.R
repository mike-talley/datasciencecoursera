## Set wd
setwd("~/Documents/R/Coursera/datasciencecoursera")

## Read and inspect outcome data
outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
  # 46col x 4706row

## Create a histogram of heart attack death rates
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## Find the best hospital in the state
  
  ## Create a function named 'best' with two args (2char state name, and outcome name)
  
best <- function(state, outcome) {
  
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Read outcome data
  data <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                   na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check for valid state and outcome values
  data <- data[, c(2,7,outcomes[outcome])]
  completeresults <- data[complete.cases(data), ]
  
  ## Arrange results by state, then mortality rate
  arrangedresults <- arrange(completeresults, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  splitresults <- split(arrangedresults ,arrangedresults$State)[state]
  
  ## Return best hospital in the state with lowest 30d death rate
  besthospital <- sapply(splitresults, function(x) x$Hospital.Name[1])
  rate <- sapply(splitresults, function(x) x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[1])
  
  output <- c(besthospital, rate)
  output
}
