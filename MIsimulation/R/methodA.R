# analysis methods
# method A: complete case analysis
#' @title methodA: complete case analysis
#'
#' @param dataset the dataset for analysis
#'
#' @return the summary of the model fit
#'
#' @importFrom stats glm complete.cases
methodA <- function(dataset){
  temp <- dataset[complete.cases(dataset),]
  fit <- glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,data=temp,family="binomial")
  result <- data.frame(summary(fit)$coefficients)
  return(result)
}
