# missing data
# MCAR
MCAR <- function(missRate, full) {
  Simdat <- full
  temp <- runif(nrow(full))
  Simdat$X[temp<=missRate] <- NA
  temp <- runif(nrow(full))
  Simdat$outcome[temp<=missRate] <- NA
  #temp <- runif(nrow(full))
  #Simdat$outcome2[temp<=missRate] <- NA
  return(Simdat)
}
