rm(list = ls())
library(expm)

P <- t(matrix(c(0.180,0.274,0.426,0.120,
                0.171,0.367,0.274,0.188,
                0.161,0.339,0.375,0.125,
                0.079,0.355,0.384,0.182),nrow=4, ncol=4))
P
P%^%5
P%^%50
P%^%500

#markov chain
set.seed(440)
# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 100000 ) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    # draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

num.chains     <- 1
num.iterations <- 100000

chain.states  <- matrix(NA, ncol=num.chains, nrow=num.iterations)
set.seed(440)
# simulate chains
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.mc.sim(P)
}

matplot(chain.states, type='l', lty=1, col=1:5, xlim=c(0, 50), ylim=c(0,5), ylab='state', xlab='position')
abline(h=1, lty=3)
abline(h=4, lty=4)

#proportions
temp <- unlist(as.list(chain.states))
A <- length(which(temp==1))/(length(temp))
C <- length(which(temp==2))/(length(temp))
G <- length(which(temp==3))/(length(temp))
t <- length(which(temp==4))/(length(temp))
proportions <- c(A,C,G,t)
proportions
count = 0
#sequence counting
for(i in 1:length(temp)){
  if (temp[i] == 2 & temp[i+1] == 3){
    count = count + 1
  }
}
prop <- count/(length(temp)-1)
prop


#One step

#initial prob
P
proportion <- 0.274*length(which(temp==2))/length(temp)
proportion

#F
c(C,G)
ans <- C*G
ans





