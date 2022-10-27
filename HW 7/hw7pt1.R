rm(list = ls())
library(ggplot2)
set.seed(440)

setwd("C:/Users/Charlie Lu/Desktop")
data <- read.csv(file = 'R file/arrival_times.csv')

x <- seq(1, 80, 1) 
zdat <- data.frame(x=x,y=data)

hist(zdat$arrival_time, breaks = 10)

zdat$arrival_time
sorted <- sort(zdat$arrival_time)
time <- data.frame(t=(sorted),N=seq_along(sorted))
time
lam_hat1 <- 80/7.924
lam_hat1

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

samp <- make_sample_df(1,8,5)

plot(samp$t,samp$N)

samp
lam_hat2 <- 43/8.147733908
lam_hat2
