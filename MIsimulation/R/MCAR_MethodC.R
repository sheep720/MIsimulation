#' MCAR_MethodC
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
MCAR_MethodC <- function(n, NSIM, missRate, trueValue, cores = 1){
  MCAR_MethodC_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MCAR(missRate, full)
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
  MCAR_MethodC_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MCAR_MethodC_Sum <- as.data.frame(MCAR_MethodC_Sum)
  colnames(MCAR_MethodC_Sum) <- c('betaC','sdC')
  MCAR_MethodC_Sum$meanBiasC <- MCAR_MethodC_Sum$betaC - trueValue
  MCAR_MethodC_Sum$meanErrorC <- abs(MCAR_MethodC_Sum$betaC - trueValue)
  MCAR_MethodC_Sum$coverC <- ifelse((MCAR_MethodC_Sum$betaC-MCAR_MethodC_Sum$sdC*qnorm(0.975))<trueValue &
                                      (MCAR_MethodC_Sum$betaC+MCAR_MethodC_Sum$sdC*qnorm(0.975))>trueValue,1,0)
  return(MCAR_MethodC_Sum)
}
