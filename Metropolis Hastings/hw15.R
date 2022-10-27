rm(list = ls())
set.seed(440)


alpha<-2.7
beta<-6.3

mc_N<-10000
X<-numeric(mc_N)
X[1]<-0


f<-function(x){dbeta(x,alpha,beta)}
Q<-function(x1,x2){dunif(x1,0,1)}

accept_fun<-function(x_c,x_p){
  accept<-f(x_p)*Q(x_c,x_p)/(f(x_c)*Q(x_p,x_c))
  return(min(accept,1))
}

set.seed(440)
for(i in 2:mc_N){
  x_prop<-runif(1,min=0,max=1)
  accept<-accept_fun(X[i-1],x_prop)
  dec<-rbinom(1,1,accept)
  if(dec==1){
    X[i] = x_prop
  }else{
    X[i] = X[i-1]
  }
}

hist(X,probability = TRUE, main="Histogram of values of x visited by MH algorithm")
curve(dbeta(x,alpha,beta),add=TRUE,col="red")

#mean and var
mean(X)
var(X)

#expected mean and var
alpha/(alpha+beta)
(alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1))
