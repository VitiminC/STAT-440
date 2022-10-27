rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)
library(gridExtra)

# define x range for plotting
xs = seq(.001, .999, .001)

set.seed(440)
xs = seq(.001, .999, .001)
#prior 1
plot(xs,dbeta(xs,7,3),type="l",col="red",ylim=c(0,4),xlim=c(0,1))
lines(xs,dbeta(xs,4,2),col="green")
lines(xs,dbinom(3,4,xs),col = "blue")
legend(x = "topleft",          # Position
       legend = c("Posterior", "Prior", "Likelihood"),  # Legend texts
       col = c( 2, 3, 4),           # Line colors
       lwd = 2)                 # Line width

#prior 2
set.seed(440)
plot(xs,dbeta(xs,19,7),type="l",col="red",ylim=c(0,6),xlim=c(0,1))
lines(xs,dbeta(xs,16,6),col="green")
lines(xs,dbinom(3,4,xs),col = "blue")
legend(x = "topleft",          # Position
       legend = c("Posterior", "Prior", "Likelihood"),  # Legend texts
       col = c( 2, 3, 4),           # Line colors
       lwd = 2)                 # Line width


#part 1
set.seed(440)
plot(xs,dbeta(xs,5,5),type="l",col="red",ylim=c(0,6),xlim=c(0,1))
lines(xs,dbeta(xs,4,2),col="green")
lines(xs,dbinom(1,4,xs),col = "blue")
legend(x = "topleft",          # Position
       legend = c("Posterior", "Prior", "Likelihood"),  # Legend texts
       col = c( 2, 3, 4),           # Line colors
       lwd = 2)                 # Line width



#part 2
set.seed(440)
plot(xs,dbeta(xs,5,5),type="l",col="red",ylim=c(0,6),xlim=c(0,1))
lines(xs,dbeta(xs,4,2),col="green")
lines(xs,dbinom(1,4,xs),col = "blue")
legend(x = "topleft",          # Position
       legend = c("Posterior", "Prior", "Likelihood"),  # Legend texts
       col = c( 2, 3, 4),           # Line colors
       lwd = 2)                 # Line width


#part 3
set.seed(440)
plot(xs,dbeta(xs,29,77),type="l",col="red",ylim=c(0,10),xlim=c(0,1))
lines(xs,dbeta(xs,4,2),col="green")
lines(xs,dbinom(25,100,xs),col = "blue")
legend(x = "topleft",          # Position
       legend = c("Posterior", "Prior", "Likelihood"),  # Legend texts
       col = c( 2, 3, 4),           # Line colors
       lwd = 2)                 # Line width





