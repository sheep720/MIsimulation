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
  # imp <- mice(temp, print = FALSE)
  # pred <- imp$predictorMatrix
  # pred[, "outcome"] <- 0
  pred <- matrix(c(0,1,1,1,0,1,0,0,0),3,3)
  rownames(pred) <- c('X', 'treatment', 'outcome')
  colnames(pred) <- c( 'X', 'treatment', 'outcome')
  imp <- mice(temp, predictorMatrix = pred)
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
