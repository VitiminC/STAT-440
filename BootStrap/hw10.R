rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)
library(boot)
library(MASS)
setwd("C:/Users/Charlie Lu/Desktop/")
data <- fread('./R file/student_scores.csv')

plot(data$GPA,data$EntranceExam)
cor(data$GPA,data$EntranceExam)

sample_set <- cbind(data$GPA,data$EntranceExam)


bootstrap = function(samples, B, estimator) {
  #' generic bootstrap function 
  #' @param samples vector of samples
  #' @param B number of bootstrap estimates
  #' @param estimator estimating function 
  
  # generate matrix of bootstrap resamples
  n = dim(samples)[1]
  # resamples = matrix(
  #   sample(samples, size=n*B, replace=TRUE),
  #   nrow=B
  # )
  bootstrap_ests = c()
  for(i in 1:B){
    index = sample(1:n, n , replace = TRUE)
    sample_new = samples[index, ]
    bootstrap_ests[i] = estimator(sample_new)
  }

  #d <- split(temp,rep(1:B,each=n/2))
  #print(d)
  #print(samples[resamples,])
  # apply to each row
  # bootstrap_ests = apply(resamples, 1, estimator)
  # print(bootstrap_ests)
  # return bootstrap mean and standard error estimates
  c(
    mean(bootstrap_ests),
    sqrt(var(bootstrap_ests))
  )
}

est_function = function(a){
  x = a[,1]
  y = a[,2]
  return(cor(x,y))
}
set.seed(440)
bootstrap_corr = bootstrap(sample_set, 10000, est_function)
bootstrap_corr
c(lower=bootstrap_corr[1] - 2 * bootstrap_corr[2],
  est=bootstrap_corr[1],
  upper=bootstrap_corr[1] + 2 * bootstrap_corr[2])

#Boot function
set.seed(440)
est_function1 = function(a,index){
  x = a[index,1]
  y = a[index,2]
  return(cor(x,y))
}
set.seed(440)
non <- boot(sample_set, est_function1, R = 10000)
non
c(0.7763745 - 2*0.132994,0.7763745+ 2*0.132994)
c(lower=bootstrap_corr[1] - 2 * bootstrap_corr[2],
  est=bootstrap_corr[1],
  upper=bootstrap_corr[1] + 2 * bootstrap_corr[2])

#Parametric
mu <- c(mean(data$EntranceExam),mean(data$GPA))
mu
sigma <- cov(data.frame(sample_set))
sigma

set.seed(440)
binorm <- mvrnorm(10000,mu,sigma)
mv_mu <- c(mean(binorm[,1]),mean(binorm[,2]))

mv_mu
mv_cov <- cov(data.frame(binorm))
mv_cov       

set.seed(440)
bootstrap_covs = replicate(
  1000, 
  # calculate sample covariance using...
  cov(
    # one randomly generated MVN sample per row
    mvrnorm(dim(sample_set)[1], mv_mu, mv_cov)
  )[1, 2]
)

set.seed(440)
# aggregate our estimates using bootstrap rules
bootstrap_cov_est = mean(bootstrap_covs)
bootstrap_cov_se = sqrt(var(bootstrap_covs))
bootstrap_cov_est
bootstrap_cov_se

c(lower=bootstrap_cov_est - 2 * bootstrap_cov_se,
  est=bootstrap_cov_est,
  upper=bootstrap_cov_est + 2 * bootstrap_cov_se)

c(lower=(bootstrap_cov_est - 2 * bootstrap_cov_se) / sqrt(prod(diag(sigma))),
  est=bootstrap_cov_est / sqrt(prod(diag(sigma))),
  upper=(bootstrap_cov_est + 2 * bootstrap_cov_se) / sqrt(prod(diag(sigma))))
