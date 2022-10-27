rm(list = ls())
set.seed(440)

n <- 100
mu <- (((150*n)/400)+(180/1600))/((n/400)+(1/1600))
sigma <- 1/((n/400)+(1/1600))

c(mu - 1.96*(sqrt(sigma)),mu + 1.96*sqrt(sigma))

