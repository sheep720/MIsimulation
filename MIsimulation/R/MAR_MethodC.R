#' MAR_MethodC
#'
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation runs
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return the beta, betaSE, meanBias, meanError, cover for methodC
#' @export
#'
#' @importFrom parallel makeCluster parLapply mclapply stopCluster
#' @importFrom stats qnorm
MAR_MethodC <- function(n, NSIM, missRate, trueValue, cores = 1){
  MAR_MethodC_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MAR(missRate, full)
    result <- methodC(data)
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
  MAR_MethodC_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MAR_MethodC_Sum <- as.data.frame(MAR_MethodC_Sum)
  colnames(MAR_MethodC_Sum) <- c('betaC','sdC')
  MAR_MethodC_Sum$meanBiasC <- MAR_MethodC_Sum$betaC - trueValue
  MAR_MethodC_Sum$meanErrorC <- abs(MAR_MethodC_Sum$betaC - trueValue)
  MAR_MethodC_Sum$coverC <- ifelse((MAR_MethodC_Sum$betaC-MAR_MethodC_Sum$sdC*qnorm(0.975))<trueValue &
                                     (MAR_MethodC_Sum$betaC+MAR_MethodC_Sum$sdC*qnorm(0.975))>trueValue,1,0)
  return(MAR_MethodC_Sum)
}
