complete <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)
  complete <- data.frame(id, nobs = vector("numeric", length = length(id)))
  
  for (i in id) {
    datainfile <- read.csv(files_full[i])
    complete[[which(id == i),"nobs"]] = sum(complete.cases(datainfile))
  }
  complete
}