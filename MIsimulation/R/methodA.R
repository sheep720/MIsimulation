# analysis methods
# method A: complete case analysis
#' @title methodA
#'
#' @description Method A used in Kontopantelis and Evangelos (2017) for anaylsis incomplete data: complete case analysis
#' @param dataset the dataset for analysis
#'
#' @return the summary of the model fit
#' @export
#'
#' @importFrom stats glm complete.cases
#' @importFrom methods is
methodA <- function(dataset){
  temp <- dataset[complete.cases(dataset),]
  # fit <- glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,data=temp,family="binomial")
  # result <- data.frame(summary(fit)$coefficients)
  # return(result)
  fit <- tryCatch(glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,data=temp,family="binomial"), error=function(e) e, warning=function(w) w)
  if(is(fit,"warning")) {
    result <- data.frame(matrix(rep(NA,4),2,2))
  }
  else if (exp(data.frame(summary(fit)$coefficients)[2,1])>1000|exp(data.frame(summary(fit)$coefficients)[2,1])<0.01){
    result <- data.frame(matrix(rep(NA,4),2,2))
  }
  else {
    result <- data.frame(summary(fit)$coefficients)
  }
  return(result)
}
