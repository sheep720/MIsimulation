#' MAR_MethodD
#'
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation runs
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return the beta, betaSE, meanBias, meanError, cover for methodD
#' @export
#'
#' @importFrom parallel makeCluster parLapply mclapply stopCluster
#' @importFrom stats qnorm
MAR_MethodD <- function(n, NSIM, missRate, trueValue, cores = 1){
  MAR_MethodD_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MAR(missRate, full)
    result <- methodD(data)
    beta <- data.frame(result[2,1])
    betaSE <- data.frame(result[2,2])
    return(cbind(beta,betaSE))
  }
  if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    simulationNtimes <- parLapply(cl, 1:NSIM, simulation)
    stopCluster(cl)
  } else {
    simulationNtimes <- mclapply(1:NSIM, simulation, mc.cores = cores)
  }
  MAR_MethodD_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MAR_MethodD_Sum <- as.data.frame(MAR_MethodD_Sum)
  colnames(MAR_MethodD_Sum) <- c('betaD','sdD')
  MAR_MethodD_Sum$meanBiasD <- MAR_MethodD_Sum$betaD - trueValue
  MAR_MethodD_Sum$meanErrorD <- abs(MAR_MethodD_Sum$betaD - trueValue)
  MAR_MethodD_Sum$coverD <- ifelse((MAR_MethodD_Sum$betaD-MAR_MethodD_Sum$sdD*qnorm(0.975))<trueValue &
                                     (MAR_MethodD_Sum$betaD+MAR_MethodD_Sum$sdD*qnorm(0.975))>trueValue,1,0)
  return(MAR_MethodD_Sum)
}
