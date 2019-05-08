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
  #imp <- mice(temp, print = FALSE)
  #pred <- imp$predictorMatrix
  #pred[, "outcome"] <- 0
  pred <- matrix(c(0,1,1,1,1,0,1,1,1,1,0,1,0,0,0,0),4,4)
  rownames(pred) <- c('eTemp', 'X', 'treatment', 'outcome')
  colnames(pred) <- c('eTemp', 'X', 'treatment', 'outcome')
  imp <- mice(temp, predictorMatrix = pred)
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
