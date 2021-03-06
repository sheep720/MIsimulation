#' @title Comparing the Performance of Method A-D (MAR)
#' @description This function will call the function \code{MAR_MethodA}, \code{MAR_MethodB}, \code{MAR_MethodC} and \code{MAR_MethodD}
#' to generate the beta coeffients for each iteration and return the summarized simulation results including mean bias, mean error,
#' relative bias, coverage, mean square error, standard deviation to compare the methods
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation runs
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return The summary simulation results
#' @export
#'
#' @examples
#' \dontrun{
#' sim_MAR(n = 1000, NSIM = 1000, missRate = 0.2, trueValue = log(2), cores = 1)
#' }
#' @importFrom stats var na.omit
sim_MAR <- function(n, NSIM, missRate, trueValue, cores = 1){
  MAR_MethodA_Sum <- MAR_MethodA(n, NSIM, missRate, trueValue, cores)
  MAR_MethodB_Sum <- MAR_MethodB(n, NSIM, missRate, trueValue, cores)
  MAR_MethodC_Sum <- MAR_MethodC(n, NSIM, missRate, trueValue, cores)
  MAR_MethodD_Sum <- MAR_MethodD(n, NSIM, missRate, trueValue, cores)
  #dat <- data.frame(MAR_MethodA_Sum$betaA, MAR_MethodB_Sum$betaB, MAR_MethodC_Sum$betaC, MAR_MethodD_Sum$betaD)
  datasetFull <- cbind(MAR_MethodA_Sum,MAR_MethodB_Sum,MAR_MethodC_Sum,MAR_MethodD_Sum)
  #MCmean <- c(mean(datasetFull$betaA, na.rm=TRUE), apply(dat,2,mean)[-1])
  #MCbias <- MCmean-trueValue
  #MCrelbias <- MCbias/trueValue
  # #fail
  fail <- c(sum(is.na(datasetFull$betaA)),sum(is.na(datasetFull$betaB)),sum(is.na(datasetFull$betaC)),sum(is.na(datasetFull$betaD)))
  # meanBeta
  meanBeta <- c(mean(datasetFull$betaA, na.rm=TRUE),mean(datasetFull$betaB, na.rm=TRUE),mean(datasetFull$betaC, na.rm=TRUE),mean(datasetFull$betaD, na.rm=TRUE))
  # meanBias
  meanBias <- c(mean(datasetFull$meanBiasA, na.rm=TRUE),mean(datasetFull$meanBiasB, na.rm=TRUE),mean(datasetFull$meanBiasC, na.rm=TRUE),mean(datasetFull$meanBiasD, na.rm=TRUE))
  # meanError
  meanError <- c(mean(datasetFull$meanErrorA, na.rm=TRUE),mean(datasetFull$meanErrorB, na.rm=TRUE),mean(datasetFull$meanErrorC, na.rm=TRUE),mean(datasetFull$meanErrorD, na.rm=TRUE))
  # relativeBias
  relBias <- meanBias/trueValue
  # coverage
  coverage <- c(mean(datasetFull$coverA, na.rm=TRUE),mean(datasetFull$coverB, na.rm=TRUE),mean(datasetFull$coverC, na.rm=TRUE),mean(datasetFull$coverD, na.rm=TRUE))
  # stdError
  stdError <- c(mean(datasetFull$sdA, na.rm=TRUE),mean(datasetFull$sdB, na.rm=TRUE),mean(datasetFull$sdC, na.rm=TRUE),mean(datasetFull$sdD, na.rm=TRUE))
  MCstddev <- c(var(na.omit(datasetFull$betaA)), var(na.omit(datasetFull$betaB)),var(na.omit(datasetFull$betaC)),var(na.omit(datasetFull$betaD)))
  #MCMSE <- apply((dat-trueValue)^2,2,mean)
  MCMSE <- meanBias^2 + MCstddev^2   # alternative lazy calculation
  MCRE <- MCMSE[1]/MCMSE

  sumdat <- rbind(rep(trueValue,4),NSIM,fail, meanBeta, meanBias,meanError, relBias,coverage,stdError,MCstddev,MCMSE,MCRE)
  names <- c("trueValue","# sims","# invalid","meanBeta","meanBias","meanError",
             "relativeBias","coverage","stdError", "MC standard deviation",'MC MSE',"MC relative efficiency")
  ests <- c("MethodA","MethodB","MethodC","MethodD")

  dimnames(sumdat) <- list(names,ests)
  round(sumdat,5)
}
