#' @title MAR_MethodB
#' @description This function will call the \code{generateData} and \code{MAR} function to generate the dataset under MAR with certain missing rate,use method B for analysis, repeate for 1000 iterations and return the beta, betaSE, meanBias, meanError, cover for each iteration
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
MAR_MethodB <- function(n, NSIM, missRate, trueValue, cores = 1){
  MAR_MethodB_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MAR(missRate, full)
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
  MAR_MethodB_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MAR_MethodB_Sum <- as.data.frame(MAR_MethodB_Sum)
  colnames(MAR_MethodB_Sum) <- c('betaB','sdB')
  MAR_MethodB_Sum$meanBiasB <- MAR_MethodB_Sum$betaB - trueValue
  MAR_MethodB_Sum$meanErrorB <- abs(MAR_MethodB_Sum$betaB - trueValue)
  MAR_MethodB_Sum$coverB <- ifelse((MAR_MethodB_Sum$betaB-MAR_MethodB_Sum$sdB*qnorm(0.975))<trueValue &
                                     (MAR_MethodB_Sum$betaB+MAR_MethodB_Sum$sdB*qnorm(0.975))>trueValue,1,0)
  return(MAR_MethodB_Sum)
}
