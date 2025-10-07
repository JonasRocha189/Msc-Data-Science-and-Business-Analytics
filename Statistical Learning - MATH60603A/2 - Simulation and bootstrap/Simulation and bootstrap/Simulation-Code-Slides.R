# The code herein was used to produce the plots and numbers in Theme 5.
# Seeds are often reset to make the examples reproducible

# Congruential generator

newx=function(x,a=1664525,c=1013904223,m=2^32){
  (a*x+c) %% m
}
  
# The seed is chosen arbitrarily, it is not a random number
x=newx(20606)

for(i in 1:100){
  x[i+1]=newx(x[i])
}

plot(x,0:100,pch=20,xlab="x_n",ylab="Sequence")
title("Numbers obtained by a congruential RNG")

for(i in 101:1000){
  x[i+1]=newx(x[i])
}

hist(x/2**32,main="Histogram of 1,000 uniform random numbers",col="gray")


# Example of Transformations

x1=seq(-4,4,.1)
par(mfrow=c(1,2))
plot(x1,dnorm(x1),type="l",lwd=2,xlab="x",ylab="Density",main="Density of X~N(0,1)",sub="Normal distribution",col="blue")
x2=seq(0,8,.1)
plot(x2,dlnorm(x2),type="l",lwd=2,xlab="y",ylab="Density",main="Density of Y=exp(X)",sub="Lognormal distribution",col="red")

# This second set of two plots was finally not included
x3=(0:20)/20
plot(cbind(x3,1),type="l",lwd=2,xlab="u",ylab="Density",main="Density of U",col="blue")
x4=seq(0,4,.1)
plot(x4,dexp(x4),type="l",lwd=2,xlab="y",ylab="Density",main="Density of W=-log(U)",col="red")

# Rejection algorithms

set.seed(2354325)
n=150
X=2*runif(n)-1
Y=2*runif(n)-1
col=rep("dark green",n)
col[X^2+Y^2>1]="red"
plot(X,Y,pch=20,xlim=c(-1,1),ylim=c(-1,1),col=col,asp=1)
theta=seq(0,2*pi,length=100)
lines(cos(theta),sin(theta),col="gray",lwd=2)

# Examples of simulations


# Lotto 6/49
set.seed(13246)
sample(1:49,6)		# Generates a winning number
draw=function(i){ sum(sample(1:49,6)<=6) }	# Generates one draw
# We can arbitrarily suppose the ticket had numbers 1 to 6
res=sapply(1:100000,draw)
table(as.factor(res))/100000

# Roulette
set.seed(13246)
onerep=function(i,money=20){
  n=0
  while(money>0 & money <40) { 
    n=n+1
    if((runif(1)*38)>1) {money=money-1} 
    else {money=money+35}
  }
  return(c(money,n))
}

out=sapply(1:1000,onerep)

# Probability of losing
mean(out[1,]==0)

# Expected amount left
mean(out[1,])

# Distribution of gains
hist(out[1,out[1,]>0],main="Winnig amounts",col="Gray",xlab="Amount ($)")

# Distribution of number of bets
hist(out[2,],main="Number of bets",col="Gray",xlab="Number of bets")
mean(out[2,])

# Number of bets when winning or not
hist(out[2,out[1,]==0],main="Number of bets when losing",col="Gray",xlab="Number of bets")
hist(out[2,out[1,]>0],main="Number of bets when winning",col="Gray",xlab="Number of bets")

# Regression

library(MASS)
set.seed(23905)
n=200; p=5
Sigma=matrix(0.2,p,p)
diag(Sigma)=1
X=mvrnorm(n,rep(0,p),Sigma)
X=cbind(1,X)
beta=c(2,1,-5,3,2,0)

Y=X%*%beta+rnorm(n,4)
summary(lm(Y~X[,-1]))
        
p=exp(X%*%beta)/(1+exp(X%*%beta))
u=runif(n)
Y2=as.numeric(u<p)
summary(glm(Y2~X[,-1]),family="binomial")  
  
# variance of the median.

x=matrix(rnorm(50*1000),ncol=1000)
meds=apply(x,2,median)
var(meds)

# Bootstrap
data(iris)
dat=iris %>%
  filter(Species=="setosa") %>%
  select(Petal.Length)

set.seed(45213)
x=matrix(sample(dat[[1]],50*1000,replace=TRUE),ncol=1000)
meds=apply(x,2,median)
var(meds)






