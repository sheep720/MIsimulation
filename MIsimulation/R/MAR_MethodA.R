#' @title MAR_MethodA
#' @description This function will call the \code{generateData} and \code{MAR} function to generate the dataset under MAR with certain missing rate,use method A for analysis, repeate for 1000 iterations and return the beta, betaSE, meanBias, meanError, cover for each iteration
#'
#' @param n sample size in each simulated dataset
#' @param NSIM the number of simulation iterations
#' @param missRate the missing rate
#' @param trueValue the true value of the parameter
#' @param cores the number of cores for parallelization, defalut = 1
#'
#' @return the beta, betaSE, meanBias, meanError, cover for methodA
#' @export
#'
#' @importFrom parallel makeCluster parLapply mclapply stopCluster
#' @importFrom stats qnorm
MAR_MethodA <- function(n, NSIM, missRate, trueValue, cores = 1){
  MAR_MethodA_Sum <- NULL
  simulation <- function(NSIM){
    full <- generateData(n)[,-1]
    data <- MAR(missRate, full)
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
  MAR_MethodA_Sum <- matrix(unlist(simulationNtimes), ncol = 2, byrow = TRUE)
  MAR_MethodA_Sum <- as.data.frame(MAR_MethodA_Sum)
  colnames(MAR_MethodA_Sum) <- c('betaA','sdA')
  MAR_MethodA_Sum$meanBiasA <- MAR_MethodA_Sum$betaA - trueValue
  MAR_MethodA_Sum$meanErrorA <- abs(MAR_MethodA_Sum$betaA - trueValue)
  MAR_MethodA_Sum$coverA <- ifelse((MAR_MethodA_Sum$betaA-MAR_MethodA_Sum$sdA*qnorm(0.975))<trueValue &
                                     (MAR_MethodA_Sum$betaA+MAR_MethodA_Sum$sdA*qnorm(0.975))>trueValue,1,0)
  return(MAR_MethodA_Sum)
}
