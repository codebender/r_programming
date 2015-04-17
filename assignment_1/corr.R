corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations

  files <- list.files(directory, full.names=TRUE)
  complete_observations <- complete(directory, 1:332)
  above_threshold <- complete_observations[complete_observations$nobs > threshold,]
  correlations=numeric()
  for(i in above_threshold$id) {
    file_data <- read.csv(files[i])
    complete_observations <-  file_data[complete.cases(file_data),]
    correlations <- append(correlations,
                           cor(complete_observations$sulfate,
                               complete_observations$nitrate))
  }
  correlations
}
