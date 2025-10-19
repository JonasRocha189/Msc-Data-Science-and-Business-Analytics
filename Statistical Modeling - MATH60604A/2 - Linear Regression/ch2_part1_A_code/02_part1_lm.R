
# Chap 2 Partie 1:  Régression linéaire simple  
# Chap 2 Part 1:  Simple linear regression  
  
  
# Ex: Fixation-intention d'achat 
# Ex: Fixation-intention to buy 

intention<-read.csv("intention.csv")
head(intention)


# Statistiques descriptives et histogrammes 
# Descriptive statistics and histograms 
summary(intention)
summary<-sapply(intention,function(x) c(mean(x),sd(x),min(x),max(x),length(x)))
row.names(summary)<-c("mean","sd","min","max","n")
summary
apply(intention[,c(3,5:7)],2,table)

# Histogrammes
# Histograms
par(mfrow=c(2,2))
hist(intention$intent,xlab="intention",main="Histogram")
hist(intention$age,xlab="age",main="Histogram")
hist(intention$fix,xlab="fixation",main="Histogram")
hist(intention$emo,xlab="emotion",main="Histogram")


# Diagramme de dispersion et corrélation 
# Scatterplots and correlation 

par(mfrow=c(1,2))
# intention, fixation
plot(intent~fix,xlab="fixation",ylab="intention",data=intention,lwd=1.5)
# pour inclure le meilleur modele lineaire 
# to include best linear model
abline(lm(intent~fix,data=intention),lwd=1.5)
# intention, emotion
plot(intent~emo,xlab="emotion",ylab="intention",data=intention,lwd=1.5)
abline(lm(intent~emo,data=intention),lwd=1.5)



# correlation
attach(intention)
# correlation
cor(cbind(intent,fix,emo))
# test
cor.test(intent,fix)
cor.test(intent,emo)
cor.test(fix,emo)
# alternative
#install.packages("Hmisc")
library("Hmisc")
rcorr(cbind(intent,fix,emo))


# Régression linéaire simple 
# Simple linear regression
lmod<-lm(intent~fix)
lmod
summary(lmod)
# intervalles de confiance/confidence intervals
confint(lmod,level=0.95)
confint(lmod,level=0.99)


# graphiques avec ggplot
# graphs with ggplot
library(ggplot2)
# modele lineare + intervalle de confiance 
# linear trend + confidence interval
ggplot(intention, aes(x=fix, y=intent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE)




# Prediction 

# Création du fichier contenant les variables explicatives pour 
# les 10 premières lignes du fichier original pour cette illustration 
# Prediction 
# Create new data file containing the explanatory 
# variables for the 10 first lines of the original data file 

new<-data.frame(fix=c(1:10))
new
# We can obtain the predicted values (which is the same as the estimated mean) for the values of fixation in newdata
predict(lmod,newdata=new)
# intervalles de confiance pour moyenne estimee
# confidence interval for estimated mean
predict.lm(lmod,newdata=new,interval=c("confidence"),
           level=0.95)
# predictions + intervalle de prediction
# predicitons + prediction intervals 
predict.lm(lmod,newdata=new,interval=c("prediction"),
           level=0.95)
##
# pour afficher les resultats dans un teableau
# displaying things together in one table
library("dplyr")
pred1<-data.frame(predict.lm(lmod,newdata=new,interval=c("confidence"),
                             level=0.95))
pred2<-data.frame(predict.lm(lmod,newdata=new,interval=c("prediction"),
                             level=0.95))
predictions<-left_join(pred1,pred2,by=c("fit"))
names(predictions)<-c("prediction","lwr.ci","upr.ci","lwr.pi","upr.pi")
predictions





# visualisation:
# graphiques avec ggplot
# graphs with ggplot
library(ggplot2)
pred.inter<- predict(lmod, interval="prediction")
intention_new<-cbind(intention,pred.inter)
# modele lineare, intervalles de confiance + prediction
# linear trend, confidence + prediction intervals
ggplot(intention_new, aes(x=fix, y=intent)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE)+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")


par(mfrow=c(1,1))

# Analyse des résidus  
# Residual analysis

# residus ordinaires
# ordinary residuals
resid(lmod)
lmod$residuals
plot(lmod$residuals~lmod$fitted.values)
hist(lmod$residuals)

# standardisé
# standardized
rstandard(lmod) 
hist(rstandard(lmod))
plot(rstandard(lmod)~lmod$fitted.values)


# studentized 
rstudent(lmod)
hist(rstudent(lmod),xlab="Residuals",main="Histogram")
plot(rstandard(lmod)~lmod$fitted.values)


# graphiques avec ggplot
# graphs using ggplot
library("ggplot2")
res.dat<-data.frame(cbind(intent,fix,lmod$fitted,lmod$residuals,rstandard(lmod),rstudent(lmod)))
names(res.dat)<-c("intent","fix","fitted","resid","rstand","rstud")
head(res.dat)

# histogram rstud
ggplot(data = res.dat, mapping = aes(x = rstud)) +
  geom_density() + 
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.5) +
  xlab("residuals") 

# qqplot rstud
ggplot(data = res.dat, mapping = aes(sample = rstud)) +
  stat_qq(distribution = qt, dparams = lmod$df.residua) +
  stat_qq_line(distribution = qt, dparams = lmod$df.residual) +  
  labs(x = "theoretical quantiles", 
       y = "empirical quantiles") +
  ggtitle("QQ-Plot Studentized Residuals")

# qqplot: alternative
library("EnvStats")
qqPlot(rstudent(lmod), 
       distribution = "t", param.list=list(df = lmod$df.residual),
       add.line=TRUE)

# resid vs. fitted + smooth
ggplot(data = res.dat, 
       aes(x = fitted, y = rstud)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("fitted values")

# resid vs. fix + smooth
ggplot(data = res.dat, 
       aes(x = fix, y = rstud)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("fixation")



# Incorporation d'une variable explicative binaire 
# Incorporating a binary explanatory variable 

lmod1<-lm(intent~sex)
summary(lmod1)
lmod2<-lm(intent~as.factor(sex))
summary(lmod2)

levels(as.factor(sex))
sex<-relevel(as.factor(sex),2)
lmod3<-lm(intent~sex)
summary(lmod3)




