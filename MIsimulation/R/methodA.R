# analysis methods
# method A: complete case analysis
methodA <- function(dataset){
  temp <- dataset[complete.cases(dataset),]
  fit <- glm( outcome~as.factor(treatment) + X + as.factor(treatment)*X ,data=temp,family="binomial")
  result <- data.frame(summary(fit)$coefficients)
  return(result)
}
