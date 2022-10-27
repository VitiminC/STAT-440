rm(list = ls())
library(ggplot2)
set.seed(440)

setwd("C:/Users/Charlie Lu/Desktop")
data <- read.csv(file = 'R file/arrival_times.csv')



make_sample_df <- function(run, tmax, lambda)
{
  ## set the starting time 
  x <- 0
  
  ## while the cumulation of time is within the time T, 
  ## we keep generating new count
  while(sum(x) < tmax) x <- c(x, rexp(1, lambda))
  
  ## output is a dataframe with cumulated time points,
  ## number of counts and how many samples we want to generate
  data.frame(t = cumsum(x), N = seq_along(x), run = rep(run, length(x)))
}

n <- 10000
lam_vec <- rep(NA,n)
for (i in 1:n){
  samp = make_sample_df(1,8,5)
  lam_hat = sum(samp$t<=8)/8
  lam_vec[i] = lam_hat
  
}
hist(lam_vec, breaks = 25)
lam_vec

lam_hat1 <- 80/7.924
extreme1 <- sum(lam_vec>lam_hat1)
p_value <- (extreme1)/n
p_value



#t.test(lam_vec,mu=5,alternative="greater")
