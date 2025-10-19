# 
# Chapitre 2 Partie 2:  Régression linéaire multiple 
# Chapter 2 Part 2: Multiple linear regression 


intention<-read.csv("intention.csv")
head(intention)

########################################################
# 1: Le modele / the model
#    Ex: intention d'achat / intention to buy 
mod<-lm(intent~fix+emo+age+sex+stat,data=intention)
summary(mod)


########################################################
# 2: variable explicative categorielle
#    categorical explanatory variable 

# creation de variables indicatrice
# creating indicator variables
intention$educ1<-as.numeric(intention$educ==1)
intention$educ2<-as.numeric(intention$educ==2)
intention$educ3<-as.numeric(intention$educ==3)
# verification
attach(intention)
table(educ,educ1)
table(educ,educ2)
table(educ,educ3)
# model 1
mod2.1<-lm(intent~educ1+educ2,data=intention)
summary(mod2.1)

# model 2
mod2.2<-lm(intent~educ1+educ3,data=intention)
summary(mod2.2)

# model 3
mod2.3<-lm(intent~educ2+educ3,data=intention)
summary(mod2.3)

# model NA
mod2.4<-lm(intent~educ1+educ2+educ3,data=intention)
summary(mod2.4)
# 
# as.factor()
mod2.5<-lm(intent~as.factor(educ),data=intention)
summary(mod2.5)
intention$educ<-relevel(as.factor(intention$educ),ref=2)
mod2.6<-lm(intent~educ,data=intention)
summary(mod2.6)
# toutes comparaisons 
# all comparisons
install.packages("emmeans")
library(emmeans)
comp<-emmeans(mod2.6,~educ)
comp
contrast(comp,method="pairwise",adjust="none")


########################################################
# 3: test global

mod<-lm(intent~sex+age+as.factor(rev)+as.factor(educ)+stat+fix+emo,data=intention)
summary(mod)
anova(mod) # (sequential SS)
# SSR
sum(anova(mod)[1:7,2])
# p 
sum(anova(mod)[1:7,1])
# SSE
anova(mod)[8,2]
# n-p-1
anova(mod)[8,1]
# ... #
# p-value
pf(9.988936,9,110,lower.tail=FALSE)
##
null<-lm(intent~1,data=intention)
summary(null)
# SST
anova(null)
## global f-test
anova(mod,null)

# Test F: 
# effets des variables individuellement
# individual variable effects
library(car)
Anova(mod,type=2) 
Anova(mod,type=3)
# attention / careful !
?Anova



########################################################             
# 4: Prediction

datapred<-intention[1:10,]
pred<-predict.lm(mod,newdata=datapred)
cbind(datapred,pred)


########################################################
# 5: Analyse des residus
#    residual analysis 
resid<-rstudent(mod)
fitted<-mod$fitted.values
res.dat<-cbind(intention,fitted,resid)
#               
# histogram 
library(ggplot2)
ggplot(data = res.dat, mapping = aes(x = resid)) +
  geom_density() + 
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5) +
  xlab("residuals") 
#
# qqplot 
ggplot(data = res.dat, mapping = aes(sample = resid)) +
  stat_qq(distribution = qt, dparams = mod$df.residua) +
  stat_qq_line(distribution = qt, dparams = mod$df.residual) +  
  labs(x = "theoretical quantiles", 
       y = "empirical quantiles") +
  ggtitle("QQ-Plot Studentized Residuals")
#
# resid vs. fix + smooth
ggplot(data = res.dat, 
       aes(x = fix, y = resid)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("fixation")
#
# resid vs. emo + smooth
ggplot(data = res.dat, 
       aes(x = emo, y = resid)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("emotion")
#
# resid vs. age + smooth
ggplot(data = res.dat, 
       aes(x = age, y = resid)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("age")
#
# resid vs. sex
ggplot(res.dat, aes(x=as.factor(sex), y=resid)) + 
  geom_boxplot() +
  labs(title="Residuals",x="sex", y = "residuals")
tapply(res.dat$resid,as.factor(res.dat$sex),function(x) c(mean(x),var(x)) )
#
# resid vs. rev
ggplot(res.dat, aes(x=as.factor(rev), y=resid)) + 
  geom_boxplot() +
  labs(title="Residuals",x="rev", y = "residuals")
tapply(res.dat$resid,as.factor(res.dat$rev),function(x) c(mean(x),var(x)) )
#
# resid vs. educ
ggplot(res.dat, aes(x=as.factor(educ), y=resid)) + 
  geom_boxplot() +
  labs(title="Residuals",x="educ", y = "residuals")
tapply(res.dat$resid,as.factor(res.dat$educ),function(x) c(mean(x),var(x)) )
#
# resid vs. stat
ggplot(res.dat, aes(x=as.factor(stat), y=resid)) + 
  geom_boxplot() +
  labs(title="Residuals",x="stat", y = "residuals")
tapply(res.dat$resid,as.factor(res.dat$stat),function(x) c(mean(x),var(x)) )
# resid vs. fitted + smooth
ggplot(data = res.dat, 
       aes(x = fitted, y = resid)) + 
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("residuals") + 
  xlab("fitted values")

########################################################
# 6: R2
mod<-lm(intent~sex+age+as.factor(rev)+as.factor(educ)+stat+fix+emo,data=intention)
summary(mod)
# by hand / manuellement
# break-down:
anova(mod) # (sequential SS)
# SSR
SSR<-sum(anova(mod)[1:7,2])
# SSE
SSE<-anova(mod)[8,2]
# SST
SST<-anova(null)[,2]
# R2
SSR/SST
# sigma-hat
np1<-anova(mod)[8,1]
sqrt(SSE/np1)


########################################################
# 7: effets non-lineaire
#    non-linear effects 

reglin2<-read.csv("reglin2.csv")
head(reglin2)
# quadratic
nlmod1<-lm(y~x+I(x^2),data=reglin2)
summary(nlmod1)
# plot
ggplot(reglin2, aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(aes(y = y),method = "lm", 
              formula = y ~ x + I(x^2), size = 1)
# alternative
reglin2$x2<-reglin2$x^2
head(reglin2)
nlmod2<-lm(y~x+x2,data=reglin2)
summary(nlmod2)
# cubic
nlmod3<-lm(y~x+I(x^2)+I(x^3),data=reglin2)
summary(nlmod3)


########################################################
# 8: interactions
# Ex: interaction entre variable continue et binaire
#     interaction between binary and continuous varaible 

reglin3<-read.csv("reglin3.csv")
head(reglin3)
#
# modele sans effet de sexe
# model without sex effect
mod.int<-lm(intent~fix,data=reglin3)
summary(mod.int)
ggplot(data = reglin3, 
       aes(x = fix, y = intent)) + 
  geom_point(aes(col = as.factor(sex))) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              formula = "y ~ x", 
                col = "black", 
              fullrange = TRUE)
#
# modele sans interactions
# model without interaction
mod.int1<-lm(intent~as.factor(sex)+fix,data=reglin3)
summary(mod.int1)
pred.int1<-mod.int1$fitted.values
ggplot(reglin3, aes(x=fix, y=intent,col=as.factor(sex)))+
  geom_point() +
  geom_line(aes(y = pred.int1), size = 1)
#
# modele avec interactions
# model with interaction
mod.int2<-lm(intent~as.factor(sex)*fix,data=reglin3)
summary(mod.int2)
pred.int2<-mod.int2$fitted.values
ggplot(reglin3, aes(x=fix, y=intent,col=as.factor(sex)))+
  geom_point() +
  geom_line(aes(y = pred.int2), size = 1)
# alternative
ggplot(data = reglin3, 
       aes(x = fix, y = intent, col = as.factor(sex))) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              formula = "y ~ x",
              show.legend = FALSE,
              fullrange = TRUE) 
#
# modele avec interactions + autres variables 
# model with interactions + other variables
reglin6<-read.csv("reglin6.csv")
head(reglin6)
mod.int3<-lm(intent~as.factor(sex)*fix+emo+age+as.factor(rev)+as.factor(educ)+stat,data=reglin6)
summary(mod.int3)




########################################################
# 9: multicolinearite / multicollinearity
#
# illustration
ex<-read.table("colinear.txt",header = TRUE)
head(ex)
attach(ex)
summary(age-age2)
cor(age,age3)
cor(age,age4)
install.packages("Hmisc")
library(Hmisc)
rcorr(cbind(age,age2,age3,age4))
#
summary(lm(height~age))
summary(lm(height~age+age2))
#
summary(lm(height~age))
summary(lm(height~age3))
summary(lm(height~age+age3))
summary(lm(height~age4))
summary(lm(height~age+age4))
###
#
reglin8<-read.csv("Data/reglin8.csv")
head(reglin8)
library("Hmisc")
rcorr(as.matrix(reglin8))
#
# VIF
library(car)
mod.col1<-lm(Y~.,data=reglin8)
summary(mod.col1)
vif(mod.col1)
#
# une facon d'incorporer les variables collineaires: 
#    Combinaison des trois variables X1, X2 et X3 
#    en une nouvelle variable (la moyenne des 3) 
# one way to incoporate the collinear variables: 
#   Combination of the three variables X1, X2, X3 
#   and create a new variable that is their average 
attach(reglin8)
temp<-reglin8
temp$avg123<-(X1+X2+X3)/3
head(temp)
# 
mod.col2<-lm(Y~avg123+X4+X5,data=temp)
summary(mod.col2)
vif(mod.col2)

