#' @title MCAR_MethodB
#' @description This function will call the \code{generateData} and \code{MCAR} function to generate the dataset under MCAR with certain missing rate,use method B for analysis, repeate for 1000 iterations and return the beta, betaSE, meanBias, meanError, cover for each iteratio
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation runs
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return the beta, betaSE, meanBias, meanError, cover for methodB
#' @export
#'
#' @importFrom parallel makeCluster parLapply mclapply stopCluster
#' @importFrom stats qnorm
MCAR_MethodB <- function(n, NSIM, missRate, trueValue, cores = 1){
  MCAR_MethodB_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MCAR(missRate, full)
    result <- methodB(data)
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
  MCAR_MethodB_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MCAR_MethodB_Sum <- as.data.frame(MCAR_MethodB_Sum)
  colnames(MCAR_MethodB_Sum) <- c('betaB','sdB')
  MCAR_MethodB_Sum$meanBiasB <- MCAR_MethodB_Sum$betaB - trueValue
  MCAR_MethodB_Sum$meanErrorB <- abs(MCAR_MethodB_Sum$betaB - trueValue)
  MCAR_MethodB_Sum$coverB <- ifelse((MCAR_MethodB_Sum$betaB-MCAR_MethodB_Sum$sdB*qnorm(0.975))<trueValue &
                                      (MCAR_MethodB_Sum$betaB+MCAR_MethodB_Sum$sdB*qnorm(0.975))>trueValue,1,0)
  return(MCAR_MethodB_Sum)
}
