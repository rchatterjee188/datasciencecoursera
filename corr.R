corr <- function(directory, threshold = 0) {
  source("complete.R")
  allnobs <- complete(directory)
  valid <- allnobs$id[allnobs$nobs >= threshold]
  files_full <- list.files(directory, full.names=TRUE)

  correadings <- data.frame(valid, comp = vector("numeric", length = length(valid)))
  for (i in valid) {
    tab <- read.csv(files_full[i])
    correadings[[which(valid == i),"comp"]] <- cor(tab$sulfate, tab$nitrate, use = "na.or.complete")
  }
  correadings$comp
}