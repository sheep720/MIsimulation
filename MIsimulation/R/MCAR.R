#' @title Generate MCAR dataset
#'
#' @param missRate the missing rate
#' @param full the orignial dataset
#'
#' @return the simulated MCAR dataset
#' @importFrom stats runif
#'
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
