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

  files <- list.files(directory, full.names=TRUE)
  data <- data.frame()

  for (i in id) {
    file_data <- read.csv(files[i])
    observations <- sum(complete.cases(file_data))
    data <- rbind(data, data.frame(id = i, nobs = observations))
  }
  data
}
