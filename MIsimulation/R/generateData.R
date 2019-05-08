#' @title Generate the original data for simulation
#'
#' @param n the sample size in each simulated dataset
#'
#' @importFrom MASS mvrnorm
#' @return the simulated dataset according to the settings in Kontopantelis and Evangelos (2017)
#' @export
#'
generateData <- function(n){
  # a binary exposure variable (treatment)
  # a continuous covariate X confounding the relationship between exposure and outcomes
  Sigma <- matrix(c(1,0.5,0.5,1),2,2)
  mu <- c(0,0)
  Simdat <- mvrnorm(n, mu = mu, Sigma = Sigma ) # from MASS package
  colnames(Simdat) <- c("eTemp","X")
  Simdat <- data.frame(Simdat)
  Simdat$treatment <- 0
  Simdat$treatment[Simdat$eTemp > 0] <- 1

  # a primary binary outcome Y
  z <- -2.19722 + log(2)*Simdat$treatment + log(1.5)*Simdat$X + log(1.2)*Simdat$treatment*Simdat$X
  pr <- 1/(1+exp(-z))         # pass through an inv-logit function
  Simdat$outcome <- ifelse(runif(n)<pr,1,0)

  # a secondary binary outcome Y’
  #z2 <- 0 + log(5)*Simdat$outcome
  #pr2 <- 1/(1+exp(-z))
  #Simdat$outcome2 <- ifelse(runif(n)<pr2,1,0)
  return(Simdat)
}
