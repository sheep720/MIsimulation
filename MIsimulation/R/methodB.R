# analysis methods
# method B: no outcome imputation, not included in mi model
#' @title method B: no outcome imputation, not included in mi model
#'
#' @param dataset the dataset for analysis
#'
#' @return the summary of the model fit
#' @export
#'
#' @importFrom mice mice pool
#' @importFrom stats glm
methodB <- function(dataset){
  temp <- dataset[!is.na(dataset$outcome),]
  imp <- mice(temp, print = FALSE)
  pred <- imp$predictorMatrix
  pred[, "outcome"] <- 0
  imp <- mice(temp, pred = pred)
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
