rm(list=ls(all=T))

set.seed(1234)

x1 <- runif(10,0,1)

vector <- sin(2*pi*x1) + rnorm(10,0,0.1)

plot(vector,type='l')

x2 = x1^2; x3 = x1^3; x4 = x1^4; x5 = x1^5; x6 = x1^6; x7 = x1^7

const = 1

input <- as.matrix(cbind(const,x1,x2,x3,x4,x5,x6,x7))

new_input <- as.matrix(cbind(x1,x3,x6,x7))

optim_func <- function(input,output){
  cost <- function(beta){
    rmse <- sum((output - input %*% beta)^2)
    return(rmse)
  }
  weights <- optim(par = rep(0,4),fn = cost,method='BFGS', 
                   control = list(maxit=10000000))
  return(weights)
}

weight <- optim_func(new_input,vector)

model = lm(vector ~ input + 0)

summary(model)  ### Ultra observation, in case the coefficients are not significant optim creates an issue

model = lm(vector ~ new_input + 0)

