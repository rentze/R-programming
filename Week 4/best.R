## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.


## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
## and “f” are tied for best, then hospital “b” should be returned).




best <- function(state, outcome) {
  
 
  
  ## Read outcome data
  ## need to read columns related to 30-day mortality for the specified outcome (“heart attack”, “heart failure”, or “pneumonia”)
  
  ## [2] "Hospital.Name" 
  ## [7] "State" 
  
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"   

  outcomeVector <- c('heart attack'=11,'heart failure'=17,'pneumonia'=23)
  outcome <- outcomeVector[outcome]
  
  ## read data
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv('hospital-data.csv', colClasses='character')
 
  
  ## Check that state is valid
  ## a NA in value causes that level to be removed from the levels and the elements formerly with that level to be replaced by NA.
  ## Test if shorter vectors are in longer vectors
  if( !( state %in% levels(factor(data$State)))) 
  {
    stop("invalid state")
  }
  
  ## Check that outcome is valid
  if (! (outcome  %in% outcomeVector))
  {
    stop("invalid outcome")
  }

  
  ## Return hospital name in that state with lowest 30-day death

  ## retrieve data for the input state
  
  stateData <- data[data$State == state,]
  

}
