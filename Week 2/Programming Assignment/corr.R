corr <- function(directory, threshold = 0) {
  
  ## Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation
  ## between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater 
  ## than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. 
  ## If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
  ## For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. 
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files( path = directory )
  
  cr <- c() 
  
  for(i in 1:length(files)){
    data <- read.csv( paste(directory, "/", files[i], sep="") ) ## read the data
    
    completeCases <- data[complete.cases(data),] ## extract the complete cases for each data value
    if ( nrow(completeCases) > threshold ) { ## if the number of complete cases is greater than the threshold, calculate
      cr <- c(cr, cor(completeCases$sulfate, completeCases$nitrate) ) 
    }
  }
  
  return( cr )
  
}