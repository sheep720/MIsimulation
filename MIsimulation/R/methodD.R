# analysis methods
# method D: outcome imputed and included in mi model
methodD <- function(dataset){
  temp <- dataset
  imp <- mice(temp)
  fit <- with(imp, glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,family="binomial") )
  result <- summary(pool(fit))
  return(result)
}
