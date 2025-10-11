# Ch1: intro 
# Exemple sur les données appariées 
# Example on paired data 

intro<-read.csv("intro.csv")
head(intro)

# Statistiques descriptives des variables T et C 
# Descriptives statistics for the variables T et C 
summary(intro)

###################################
# test-t bilateral (apparie)
# two-sided t-test (paired)
attach(intro)
# approche 1: a la main
# approach 1: by hand
n<-nrow(intro)
dbar<-mean(t-c)
s<-sd(t-c)
# statistique de test
# test statistic
test<-dbar/(s/sqrt(n))
test
# p-value
2*pt(test,df=n-1,lower.tail=FALSE)
# approche 2: fonction t.test
# approach 2: t.test function
t.test(t-c, alternative = "two.sided", mu = 0)


###################################
# intervalle de confiance
# confidence interval

# approche 1: a la main
# approach 1: by hand
c(dbar-s/sqrt(n)*qt(.975,df=n-1,lower.tail=TRUE),dbar+s/sqrt(n)*qt(.975,df=n-1,lower.tail=TRUE))
# approche 2: fonction t.test
# approach 2: t.test function
t.test(t-c, alternative = "two.sided", mu = 0)


