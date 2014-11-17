complete <- function(directory, id = 1:332) {
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
  
  all_data <- data.frame()
  
  id = formatC(id, width = 3, format = 'd', flag = '0')
  
  for (i in id){
    csv_file <- read.csv(file=paste(directory,'/', i,'.csv', sep = ''))
    rows <- nrow(na.omit(csv_file))
    all_data <- rbind(all_data,data.frame(id=i, nobs = rows))
  }
  all_data
}