rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)

setwd("C:/Users/Charlie Lu/Desktop/")
samples <- fread('./R file/regression_example.csv')
plot(samples)

sampleSize = 1000

x = as.numeric(as.character(samples$x))
y = as.numeric(as.character(samples$y))

likelihood = function(param){
  beta = param[1]
  sd = param[2]
  
  pred = beta*x
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) 
  sumll = sum(singlelikelihoods)
  return(sumll)
}

# Prior distribution on the log scale
prior = function(param){
  beta = param[1]
  sd = param[2]
  aprior = dunif(beta, min=-.2, max=.2, log = T)
  sdprior = dunif(sd, min=-.1, max=.1, log = T)
  return(aprior+sdprior)
}

# Posterior distribution on the log scale
posterior = function(param){
  return (likelihood(param) + prior(param))
}

proposalfunction = function(param){
  return(rnorm(2,mean = param, sd= c(2,2)))
}

run_metropolis_MCMC = function(startvalue, iterations){
  chain = array(dim = c(iterations+1,2))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

set.seed(440)

startvalue = c(4.5,7)
chain = run_metropolis_MCMC(startvalue, 100000)
