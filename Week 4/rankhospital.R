rankhospital <- function(state,outcome,num ="best"){
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
  
  ## Check whether num is a number or a valid string
  
  if (class(num) == "character"){
    if (!(num=="best" || num=="worst"))
      stop("invalid num value")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  
  ## transform data for the input outcome
  data[,outcome] <- as.numeric(as.character(data[,outcome]))
  data<-na.omit(data)
  ##write.table(data,file='result.txt',sep='\t')
  
  ## retrieve data for the input state
  filteredPerState <- subset(data, State == state)
  
  ##write.table(filteredPerState,file='result2.txt',sep='\t')
  
  filteredPerState <- filteredPerState[order(filteredPerState[,outcome],filteredPerState[,'Hospital.Name'],na.last=TRUE),'Hospital.Name']
  ##write.table(na.omit(filteredPerState),file='result3.txt',sep='\t')
  
  if (num == 'best')
    return(filteredPerState[1])
  else if (num == 'worst')
    return(filteredPerState[length(filteredPerState)])
  else if (length(filteredPerState) < num)
    return(NA)
  else
    return (filteredPerState[num])
  
}