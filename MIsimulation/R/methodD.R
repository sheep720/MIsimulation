# analysis methods
# method D: outcome imputed and included in mi model
#' @title method D: outcome imputed and included in mi model
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
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
