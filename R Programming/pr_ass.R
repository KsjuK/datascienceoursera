add2 <- fuction(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

columnmean <- function(y) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i])
  }
  means
}

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
  
  allFiles <- list.files(path = directory, full.names = TRUE)
  selectedData <- data.frame()
  for (i in id) {
    selectedData <- rbind(selectedData, read.csv(allFiles[i]))
  }
  if (pollutant == 'sulfate') {
    mean(selectedData$sulfate, na.rm = TRUE)
  } 
  else if (pollutant == 'nitrate') {
    mean(selectedData$nitrate, na.rm = TRUE)
  }
  
}

