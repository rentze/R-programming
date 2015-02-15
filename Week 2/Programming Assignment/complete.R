complete <- function(directory, id = 1:332) {
  
  ## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
  ## The function should return a data frame where the first column is the name of the file and the second column is the number of 
  ## complete cases.   A prototype of this function follows
  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## algorithm
  ## 1) initialize a data vector to hold numeric values read from the csv fiels
  nobs = numeric()
  
  ## 2) loop through the number of files
  for (i in id) 
  {
    ## 3) read each csv file 
    ## read csv argument is the absolute path of the file, thus concatenate directory with file number and csv extension
    value= read.csv(paste(directory,"/", formatC(i, width = 3, flag = "0"),".csv", sep = ""))
    ## 4) extract data
    nobs = c(nobs,sum(complete.cases(value)))
  }
  ## 5) return the mean value ignoring missing values
  return (data.frame(id,nobs))
}