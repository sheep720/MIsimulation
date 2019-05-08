# MCAR
#' MCAR_MethodA
#'
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation runs
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return the beta, betaSE, meanBias, meanError, cover for methodA
#' @export
#'
#' @importFrom parallel makeCluster parLapply mclapply stopCluster
#' @importFrom stats qnorm
MCAR_MethodA <- function(n, NSIM, missRate, trueValue, cores = 1){
  MCAR_MethodA_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MCAR(missRate, full)
    result <- methodA(data)
    beta <- result[2,1]
    betaSE <- result[2,2]
    return(cbind(beta,betaSE))
  }
  if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    simulationNtimes <- parLapply(cl, 1:NSIM, simulation)
    stopCluster(cl)
  } else {
    simulationNtimes <- mclapply(1:NSIM, simulation, mc.cores = cores)
  }
  MCAR_MethodA_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MCAR_MethodA_Sum <- as.data.frame(MCAR_MethodA_Sum)
  colnames(MCAR_MethodA_Sum) <- c('betaA','sdA')
  MCAR_MethodA_Sum$meanBiasA <- MCAR_MethodA_Sum$betaA - trueValue
  MCAR_MethodA_Sum$meanErrorA <- abs(MCAR_MethodA_Sum$betaA - trueValue)
  MCAR_MethodA_Sum$coverA <- ifelse((MCAR_MethodA_Sum$betaA-MCAR_MethodA_Sum$sdA*qnorm(0.975))<trueValue &
                                      (MCAR_MethodA_Sum$betaA+MCAR_MethodA_Sum$sdA*qnorm(0.975))>trueValue,1,0)
  return(MCAR_MethodA_Sum)
}
