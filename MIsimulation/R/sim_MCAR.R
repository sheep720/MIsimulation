sim_MCAR <- function(n, NSIM, missRate, trueValue, cores = 1){
  S <- NSIM
  MCAR_MethodA_Sum <- MCAR_MethodA(n, NSIM, missRate, trueValue, cores)
  MCAR_MethodB_Sum <- MCAR_MethodB(n, NSIM, missRate, trueValue, cores)
  MCAR_MethodC_Sum <- MCAR_MethodC(n, NSIM, missRate, trueValue, cores)
  MCAR_MethodD_Sum <- MCAR_MethodD(n, NSIM, missRate, trueValue, cores)
  dat <- data.frame(MCAR_MethodA_Sum$betaA, MCAR_MethodB_Sum$betaB, MCAR_MethodC_Sum$betaC, MCAR_MethodD_Sum$betaD)
  datasetFull <- cbind(MCAR_MethodA_Sum,MCAR_MethodB_Sum,MCAR_MethodC_Sum,MCAR_MethodD_Sum)
  MCmean <- apply(dat,2,mean)
  MCbias <- MCmean-trueValue
  MCrelbias <- MCbias/trueValue
  MCstddev <- sqrt(apply(dat,2,var))
  MCMSE <- apply((dat-trueValue)^2,2,mean)
  #   MCMSE <- MCbias^2 + MCstddev^2   # alternative lazy calculation
  MCRE <- MCMSE[1]/MCMSE
  # meanBias
  meanBias <- c(mean(datasetFull$meanBiasA),mean(datasetFull$meanBiasB),mean(datasetFull$meanBiasC),mean(datasetFull$meanBiasD))
  # meanError
  meanError <- c(mean(datasetFull$meanErrorA),mean(datasetFull$meanErrorB),mean(datasetFull$meanErrorC),mean(datasetFull$meanErrorD))
  # coverage
  coverage <- c(mean(datasetFull$coverA),mean(datasetFull$coverB),mean(datasetFull$coverC),mean(datasetFull$coverD))
  # stdError
  stdError <- c(mean(datasetFull$sdA),mean(datasetFull$sdB),mean(datasetFull$sdC),mean(datasetFull$sdD))
  sumdat <- rbind(rep(trueValue,4),S,MCmean,MCbias,MCrelbias,MCstddev,MCMSE,
                  MCRE,meanBias,meanError,coverage,stdError)
  names <- c("true value","# sims","MC mean","MC bias","MC relative bias",
             "MC standard deviation","MC MSE","MC relative efficiency","meanBias","meanError","coverage","stdError")
  ests <- c("MethodA","MethodB","MethodC","MethodD")

  dimnames(sumdat) <- list(names,ests)
  round(sumdat,5)
}
