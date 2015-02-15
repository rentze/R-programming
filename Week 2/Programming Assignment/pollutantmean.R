pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## Write a function named 'pollutantmean' that calculates the mean of a pollutant 
  ## (sulfate or nitrate) across a specified list of monitors. 
  ## The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
  ##  Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified 
  ## in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA
  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## algorithm
  ## 1) initialize a data vector to hold numeric values read from the csv fiels
  data = numeric()
  
  
  ## id can be replaced with  
      ## id <- list.files( path = directory )
      ## length(id)
  
  ## 2) loop through the number of files
  for (i in id)
  {
    ## 3) read each csv file 
    ## read csv argument is the absolute path of the file, thus concatenate directory with file number and csv extension
    value= read.csv(paste(directory,"/", formatC(i, width = 3, flag = "0"),".csv", sep = ""))
    ## 4) extract data
    data = c(data,value[[pollutant]]) 
  }
  ## 5) return the mean value ignoring missing values
  return (mean(data,na.rm = TRUE))
}