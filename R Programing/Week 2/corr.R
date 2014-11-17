corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cor_total = vector()
  
  comp <- complete(directory = directory)
  comp_thresh <- comp[comp$'nobs'>threshold,]
  for (id in comp_thresh[["id"]]){
    csv_file <- read.csv(file=paste(directory,'/', id,'.csv', sep = ''))
    cor_temp = cor(x = csv_file[['nitrate']], y=csv_file[['sulfate']],use = "pairwise.complete.obs")
    cor_total = c(cor_total, cor_temp)
  }
  cor_total
}