pollutantmean <- function (directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)
  
  sum <- 0
  count <- 0
  
  for(i in id) {
    monitordata <- read.csv(files_full[i])
    polldata <- monitordata[[pollutant]]
    validpolldata <- polldata[!is.na(polldata)]
    
    sum <- sum + sum(validpolldata)
    count <- count + length(validpolldata)
  }
  
  sum / count
}