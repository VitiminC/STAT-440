rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)


setwd("C:/Users/Charlie Lu/Desktop/")
data <- fread('./R file/student_scores.csv')

plot(data$GPA,data$EntranceExam)
cor(data$GPA,data$EntranceExam)

sample_set <- cbind(data$GPA,data$EntranceExam)


jackknife_vec = function(samples, est_func) {
  #' 
  #' Function for performing jackknife estimation for
  #' vector-valued functions
  #' 
  #' @param samples vector of samples
  #' @param est_func scalar-valued function 
  
  n = length(samples[,1])
  jackknife_samps = sapply(
    # for each index in the sample...
    1:n,
    # ...calculate the statistic at all but the current index
    function(j) { est_func(samples[-j,]) }
  )
  
  # calculate the jackknife estimate
  theta_est = mean(jackknife_samps) 
  
  # calculate the jackknife variance estimate
  var_est = (
    (n-1) / n * sum((jackknife_samps - theta_est)**2)
  )
  
  # calculate the jackknife bias estimate
  bias_est = (
    (n-1) * (theta_est - est_func(samples))
  )
  
  # return all three outputs
  list(
    theta_est,
    bias_est,
    var_est
  )
}
est_function = function(a){
  x = a[,1]
  y = a[,2]
  return(cor(x,y))
}
jackknife_vec(sample_set,est_function)























