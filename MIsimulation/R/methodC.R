# analysis methods
# method C: no outcome imputation, outcome imputed in mi model
methodC <- function(dataset){
  temp <- dataset[!is.na(dataset$outcome),]
  imp <- mice(temp)
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
