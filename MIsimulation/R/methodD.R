# analysis methods
# method D: outcome imputed and included in mi model
#' @title method D
#'
#' @description Method D used in Kontopantelis and Evangelos (2017) for anaylsis incomplete data: outcome imputed and included in mi model
#'
#' @param dataset the dataset for analysis
#'
#' @return the summary of the model fit
#' @export
#'
#' @importFrom mice mice pool
#' @importFrom stats glm
methodD <- function(dataset){
  temp <- dataset
  imp <- mice(temp)
  #fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  #result <- summary(pool(fit))
  fit <- tryCatch(with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") ), error=function(e) e, warning=function(w) w)
  if(is(fit,"warning")) {
    result <- data.frame(matrix(rep(NA,4),2,2))
  }
  else if (exp(summary(pool(fit))[2,1])>1000|exp(summary(pool(fit))[2,1])<0.01){
    result <- data.frame(matrix(rep(NA,4),2,2))
  }
  else {
    result <- summary(pool(fit))
  }
  return(result)
}
