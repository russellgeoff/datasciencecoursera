pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  all_data <- data.frame()
  
  id <- formatC(id, width = 3, format = 'd', flag = '0')
  
  for (i in id){
    csv_file <- read.csv(file=paste(directory,'/', i,'.csv', sep = ''))
    csv_data <- na.omit(csv_file[pollutant])
    all_data <- rbind(all_data, csv_data)
  }
  mean(all_data[[pollutant]])
  

}