# analysis methods
# method C: no outcome imputation, outcome imputed in mi model
#' @title method C: no outcome imputation, outcome imputed in mi model
#'
#' @param dataset the dataset for analysis
#'
#' @return the summary of the model fit
#' @export
#'
#' @importFrom mice mice pool
#' @importFrom stats glm
methodC <- function(dataset){
  temp <- dataset[!is.na(dataset$outcome),]
  imp <- mice(temp)
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
