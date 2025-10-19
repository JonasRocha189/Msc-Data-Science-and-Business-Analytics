# 
# Chapitre 2 Partie 3: test-t et ANOVA 
# Chapter 2 Part 3: t-test and ANOVA 


########################################################
########################################################
########################################################
# t test (classic / classique)

# Exemple: Carte de crC)dit vs comptant 
# Example: credit vs cash 

comp<-read.csv("Data/comp.csv")
head(comp)
summary(comp)
table(comp$grp)



# Test t bidirectionnel: H0: u1 = u2 vs. H1: u1 et u2 sont diffC)rente 
# Bilateral t-test: H0: u1 = u2 vs. H1: u1 and u2 differ 

# t-test 
t.test(off~grp,alternative="two.sided",var.equal=TRUE,conf.level=0.95,data=comp)

# En utilisant un modele de regression 
# Using linear regression 
mod1<-lm(off~grp,data=comp)
summary(mod1)
# alternative: as.factor(grp)
summary(lm(off~as.factor(grp),data=comp))
# c.i.:
confint(mod1)


########################################################
# Assumptions

# residuals / residus
# histogram
library(ggplot2)
ggplot(comp, aes(x = rstudent(mod1))) +
  geom_density() + 
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.5) 
# qqplot
ggplot(comp, mapping = aes(sample = rstudent(mod1))) +
  stat_qq(distribution = qt, dparams = mod1$df.residual) +
  stat_qq_line(distribution = qt, dparams = mod1$df.residual) +  
  labs(x = "theoretical quantiles", 
       y = "empirical quantiles") 
# Y
# histogram
library(ggplot2)
ggplot(comp, aes(x = off)) +
  geom_density() + 
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.5) +
  facet_grid(grp ~ .)
# qqplot
ggplot(data = comp, aes(sample = off)) + 
  geom_qq( ) +
  geom_qq_line( ) +
  facet_grid(grp ~ .)

# test: egalite des variances / equality of variances
var.test(off~grp,data=comp,alternative="two.sided")


########################################################
# Welch test

# Welch test
t.test(off~grp,alternative="two.sided",var.equal=FALSE,conf.level=0.95,data=comp)

# linear regression: generalized least squares
# allows for non-constant variance 
# permet pour une variance non-constante
library(nlme)
?gls
mod2 <- gls(off ~ grp, data = comp, 
          weights=varIdent(form = ~ 1 | grp))
summary(mod2)
library(emmeans)
m2<-emmeans(mod2, specs="grp")
m2
m2.cont<-contrast(m2,method="pairwise")
m2.cont
confint(m2.cont)


########################################################
########################################################
########################################################
# ANOVA (one-way / un facteur)
# 
# Exemple: Service dans les banques indiennes 
# Example: service in Indian banks

comp3<-read.csv("Data/comp3.csv")
head(comp3)
summary(comp3)
table(comp3$ban)


# Statistiques descriptives des diffC)rentes banques 
# Descriptive statistics for the different banks 

stats<-data.frame(as.matrix(aggregate(score~ban, data=comp3, 
                                      function(x)cbind(mean(x),sd(x),min(x),max(x),length(x)))))
names(stats)<-c("ban","mean","sd","min","max","n")
stats

library(dplyr)
group_by(comp3, ban) %>%
  summarise(
    mean = mean(score),
    sd = sd(score),
    min = min(score),
    max = max(score),
    count = n(),
  )

# visualization
boxplot(score~as.factor(ban),data=comp3)



# ANOVA 
oneway.test(score ~ ban, data = comp3, var.equal = TRUE)
# linear regression
mod3<-lm(score~as.factor(ban),data=comp3)
summary(mod3)
library(car)
Anova(mod3)

# normality / normalite?
# histograms
library(ggplot2)
ggplot(comp3, aes(x = score)) +
  geom_density() + 
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.5) +
  facet_grid(ban ~ .)
# qqplot
ggplot(data = comp3, aes(sample = score)) + 
  geom_qq( ) +
  geom_qq_line( ) +
  facet_grid(ban ~ .)

# test: equality of variance / egalite des variances
bartlett.test(score~as.factor(ban),data=comp3)
library(car)
leveneTest(score~as.factor(ban),data=comp3)
leveneTest(score~as.factor(ban),data=comp3,center=mean)

# Welch's test
oneway.test(score ~ ban, data = comp3, var.equal = FALSE)



# Pairwise comparisons / comparaisons paires
# linear regression / regression lineaire
library(nlme)
library(emmeans)
# constant variance constante
contrast(emmeans(mod3, ~ban),method="pairwise",adjust="none")
# different variance by bank
# differente variance par banque
mod4 <- gls(score ~ as.factor(ban), data = comp3, 
            weights=varIdent(form = ~ 1 | as.factor(ban)))
summary(mod4)
contrast(emmeans(mod4, ~ban),method="pairwise",adjust="none") 



########################################################
########################################################
########################################################
# ANOVA (two-way) 

#  Exemple: UX 
#  Example: UX 
comp4<-read.csv("Data/comp4.csv")
head(comp4)

# modeles a effets principaux
# main effects model
mod5<-lm(eval~as.factor(sta)+as.factor(del),data=comp4)
summary(mod5)
#
library(car)
Anova(mod5)
anova(mod5,lm(eval~as.factor(del),data=comp4))
anova(mod5,lm(eval~as.factor(sta),data=comp4))



# modele avec interaction
# interaction model
mod6<-lm(eval~as.factor(sta)*as.factor(del),data=comp4)
summary(mod6)
#
library(car)
Anova(mod6)
anova(mod6,mod5)


# explorer plus en profondeur anova/Anova
# further exploring anova/Anova
Anova(mod6,error=mod5)
Anova(mod6,error=mod6)
Anova(mod6)
anova(mod6,lm(eval~as.factor(del),data=comp4))
anova(mod5,lm(eval~as.factor(del),data=comp4))
anova(mod5,lm(eval~as.factor(del),data=comp4),error=mod6)
anova(mod6,lm(eval~as.factor(sta),data=comp4))
anova(mod5,lm(eval~as.factor(sta),data=comp4))
anova(mod5,lm(eval~as.factor(sta),data=comp4),error=mod6)

# autre variation: type III
# other variant: type III
Anova(mod6,type=3)
# manually / manuellement
data<-comp4
data$sta2<-as.numeric(comp4$sta==2)
data$del2<-as.numeric(comp4$del==2)
data$del3<-as.numeric(comp4$del==3)
data$int22<-as.numeric(comp4$sta==2 & comp4$del==2)
data$int23<-as.numeric(comp4$sta==2 & comp4$del==3)
head(data)
# 
mod7<-lm(eval~sta2+del2+del3+int22+int23,data=data)
summary(mod7)
summary(mod6)
#
Anova(mod6,type=2)
Anova(mod6,type=3)
anova(mod7,lm(eval~del2+del3+int22+int23,data=data))
anova(mod7,lm(eval~sta2+int22+int23,data=data))


# comparisons
library(emmeans)
emmeans(mod6, ~del*sta)
contrast(emmeans(mod6, ~del*sta),method="pairwise",adjust="none")


# tapply(data2$eval, as.factor(data2$grp), mean)


# testing equality of variance
# test egalite des variances
data2<-comp4
data2$grp<-1*as.numeric(comp4$del==1 & comp4$sta==1) + 
  2*as.numeric(comp4$del==2 & comp4$sta==1)+
  3*as.numeric(comp4$del==3 & comp4$sta==1)+
  4*as.numeric(comp4$del==1 & comp4$sta==2)+
  5*as.numeric(comp4$del==2 & comp4$sta==2)+
  6*as.numeric(comp4$del==3 & comp4$sta==2)
head(data2)

# test: equality of variance / egalites des variances
bartlett.test(eval~as.factor(grp),data=data2)
library(car)
leveneTest(eval~as.factor(grp),data=data2)
leveneTest(eval~as.factor(grp),data=data2,center=mean)


# variance par/by group
library(nlme)
mod8 <- gls(eval ~ as.factor(del)*as.factor(sta), data = data2, 
            weights=varIdent(form = ~ 1 | grp))
summary(mod8)








