# 
#  Chapitre 3: Estimation de maximum de vraisemblance
#  Chapter 3: Maximum likelihood estimation



# gÃ©nÃ©rer observations de loi gamma
# generate gamma distributed observations

set.seed(12)
n<-1000 # nombre d'observations / number of observations
a<-2
b<-5
data<-rgamma(n,shape=a,scale=b)
summary(data)


# MLEs:
library(MASS)
fitdistr(data, "gamma", start=list(shape=2, scale=5))
fitdistr(data, "gamma", start=list(shape=1.5, scale=4.5))
fitdistr(data, "gamma", start=list(shape=1, scale=2))


# approche 1 / approach 1
# crÃ©er fonction de log-vraisemblance
# create log-likelihood function
llgamma<-function(params,x){
  alpha<-params[1]
  beta<-params[2]
  N<-length(x)
  return(-N*log(gamma(alpha)) - (alpha*N)*log(beta) + 
           (alpha-1)*sum(log(x)) - (sum(x)/beta))
}
# test
llgamma(c(a,b),data)
# optimisation
optim(par=c(2,5), fn=llgamma, method="L-BFGS-B",lower=c(0,0),upper=c(Inf,Inf),x=data, control=list(fnscale=-1))
optim(par=c(1,2), fn=llgamma, method="L-BFGS-B",lower=c(0,0),upper=c(Inf,Inf),x=data, control=list(fnscale=-1))



# approche 2 / approach 2
# crÃ©er fonction de score / create score function
score.alpha<-function(alpha,x){
  log(alpha)-digamma(alpha)-log(mean(x))+mean(log(x))
}
# test
score.alpha(a,data)
# solve
uniroot(f=score.alpha,interval=c(10^-10,10^10),x=data)
alpha.hat<-uniroot(f=score.alpha,interval=c(10^-10,10^10),x=data)$root
beta.hat<-mean(data)/alpha.hat
alpha.hat
beta.hat