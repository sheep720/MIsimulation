#' sim_MAR
#'
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
#' sim_MAR <- function(n = 1000, NSIM = 1000, missRate = 0.2, trueValue = log(2), cores = 1)
#' }
#' @importFrom stats var
sim_MAR <- function(n, NSIM, missRate, trueValue, cores = 1){
  S <- NSIM
  MAR_MethodA_Sum <- MAR_MethodA(n, NSIM, missRate, trueValue, cores)
  MAR_MethodB_Sum <- MAR_MethodB(n, NSIM, missRate, trueValue, cores)
  MAR_MethodC_Sum <- MAR_MethodC(n, NSIM, missRate, trueValue, cores)
  MAR_MethodD_Sum <- MAR_MethodD(n, NSIM, missRate, trueValue, cores)
  dat <- data.frame(MAR_MethodA_Sum$betaA, MAR_MethodB_Sum$betaB, MAR_MethodC_Sum$betaC, MAR_MethodD_Sum$betaD)
  datasetFull <- cbind(MAR_MethodA_Sum,MAR_MethodB_Sum,MAR_MethodC_Sum,MAR_MethodD_Sum)
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
