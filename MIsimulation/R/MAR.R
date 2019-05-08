# MAR
MAR <- function(missRate, full){
  Simdat <- full
  if (missRate <= 0.5) {
    z <- log(5)*Simdat$treatment
    pr <- 1/(1+exp(-z))
    tempx <- ifelse(runif(nrow(full))<pr,1,0)
  }else{
    z <- log(5) + log(5)*Simdat$treatment
    pr <- 1/(1+exp(-z))
    tempx <- ifelse(runif(nrow(full))<pr,1,0)
  }
  newMissRate <- missRate*nrow(full)/sum(tempx)
  tempy <- runif(nrow(full))
  Simdat$X[tempy<newMissRate&tempx==1] <- NA

  if (missRate <= 0.5) {
    z <- log(5)*Simdat$treatment
    pr <- 1/(1+exp(-z))
    tempx <- ifelse(runif(nrow(full))<pr,1,0)
  }else{
    z <- log(5) + log(5)*Simdat$treatment
    pr <- 1/(1+exp(-z))
    tempx <- ifelse(runif(nrow(full))<pr,1,0)
  }
  newMissRate <- missRate*nrow(full)/sum(tempx)
  tempy <- runif(nrow(full))
  Simdat$outcome[tempy<newMissRate&tempx==1] <- NA

  # if (missRate <= 0.5) {
  #   z <- log(5)*Simdat$treatment
  #   pr <- 1/(1+exp(-z))
  #   tempx <- ifelse(runif(nrow(full))<pr,1,0)
  # }else{
  #   z <- log(5) + log(5)*Simdat$treatment
  #   pr <- 1/(1+exp(-z))
  #   tempx <- ifelse(runif(nrow(full))<pr,1,0)
  # }
  # newMissRate <- missRate*nrow(full)/sum(tempx)
  # tempy <- runif(nrow(full))
  # Simdat$outcome2[tempy<newMissRate&tempx==1] <- NA
  return(Simdat)
}
