
# Practice of the second part of Linear Regression


### 0 - Data

intention <- read.csv("ch2_part2_A_code/intention.csv")
head(intention)



### 1 - Model
mod <- lm(intent~fix+emo+age+sex+stat, data = intention)
summary(mod)



### 2 - Categorical Values

# Creating indicator variables
intention$educ1 <-  as.numeric(intention$educ == 1)
intention$educ2 <-  as.numeric(intention$educ == 2)
intention$educ3 <-  as.numeric(intention$educ == 3)
head(intention)

# verification
attach(intention)

table(educ, educ1)
table(educ, educ2)
table(educ, educ3)

summary(educ1 +educ2 + educ3)

mod2.1 <- lm(intent ~ educ1 + educ2, data = intention)
summary(mod2.1)

mod2.2 <- lm(intent ~ educ1 + educ3, data = intention)
summary(mod2.2)

mod2.3 <- lm(intent ~ educ2 + educ3, data = intention)
summary(mod2.3)

# no need to use all educ varibles
mod2.4 <- lm(intent ~ educ1 + educ2 + educ3, data = intention)
summary(mod2.4)

# as.factor
mod2.5 <- lm(intent ~ as.factor(educ), data = intention)
summary(mod2.5)

# Change the reference level
intention$educ <- relevel(as.factor(intention$educ), ref=2)
mod2.6 <- lm(intent ~ educ, data = intention)
summary(mod2.6)

# all comparisons
install.packages("emmenas")
library(emmeans)

comp <- emmeans(mod2.6, ~educ)
comp

contrast(comp, method="pairwise", adjuste="none")



### 3 - Test global effects
mod <- lm(intent ~ sex + age + as.factor(rev) + as.factor(educ) + stat + fix + emo, data=intention)
summary(mod)

# Sequention SS
anova(mod)


# SSR (here, i.e. SSE(reduced) - SSE(full) where reduced model is an intercept only model )
sum(anova(mod)[1:7,2])


# p
sum(anova(mod)[1:7,1])


# SSE(full)
anova(mod)[8,2]


# n-p-1
anova(mod)[8,1]

# p-value
pf(9.988936,9,110,lower.tail=FALSE)


# model without X1,...,Xp
null<-lm(intent~1,data=intention)
summary(null)


# SST
anova(null)


## global f-test
anova(mod,null)


# Test F:
# individual variable effects
library(car)

Anova(mod)
?Anova

# another way to proceed for categorical variables to compare the model with and without education
mod.no.edu<-lm(intent~fix+emo+sex+age+as.factor(rev)+stat,data=intention)
anova(mod,mod.no.edu)




### 4 - Prediction

datapred <- intention[1:10,]
datapred

pred <- predict.lm(mod, newdata=datapred)
cbind(datapred, pred)



### 5 - Analysis of Residuals

resid <- rstudent(mod)
fitted <- mod$fitted.values
res.dat <- cbind(intention, fitted, resid)
head(res.dat)

# Histogram
library(ggplot2)
ggplot(data=res.dat, mapping=aes(x=resid)) + 
  geom_density() + 
  geom_histogram(aes(y= ..density..), bins=10, alpha=0.5) +
  xlab("residuals")


# qqplot
ggplot(data = res.dat, mapping = aes(sample = resid)) +
  stat_qq(distribution = qt, dparams = mod$df.residua) +
  stat_qq_line(distribution = qt, dparams = mod$df.residual) +
  labs(x = "theoretical quantiles",
       y = "empirical quantiles") +
  ggtitle("QQ-Plot Studentized Residuals")


# resid vs. fix + smooth
ggplot(data = res.dat, aes(x = fix, y = resid)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("fixation")


# resid vs. emo + smooth
ggplot(data = res.dat, aes(x = emo, y = resid)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("emotion")


# resid vs. age + smooth
ggplot(data = res.dat,
       aes(x = age, y = resid)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("age")


# resid vs. age + smooth
ggplot(data = res.dat, aes(x = age, y = resid)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("age")

# resid vs. sex
ggplot(res.dat, aes(x=as.factor(sex), y=resid)) +
  geom_boxplot() +
  labs(title="Residuals",x="sex", y = "residuals")

tapply(res.dat$resid,as.factor(res.dat$sex),function(x) c(mean(x),var(x)) )

# resid vs. rev
ggplot(res.dat, aes(x=as.factor(rev), y=resid)) +
  geom_boxplot() +
  labs(title="Residuals",x="rev", y = "residuals")

tapply(res.dat$resid,as.factor(res.dat$rev),function(x) c(mean(x),var(x)) )

# resid vs. educ
ggplot(res.dat, aes(x=as.factor(educ), y=resid)) +
  geom_boxplot() +
  labs(title="Residuals",x="educ", y = "residuals")

tapply(res.dat$resid,as.factor(res.dat$educ),function(x) c(mean(x),var(x)) )

# resid vs. stat
ggplot(res.dat, aes(x=as.factor(stat), y=resid)) +
  geom_boxplot() +
  labs(title="Residuals",x="stat", y = "residuals")

tapply(res.dat$resid,as.factor(res.dat$stat),function(x) c(mean(x),var(x)) )

# resid vs. fitted + smooth
ggplot(data = res.dat, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("fitted values")




### 6 - R²

mod<-lm(intent~sex+age+as.factor(rev)+as.factor(educ)+stat+fix+emo,data=intention)
summary(mod)

null<-lm(intent~1,data=intention)
summary(null)

# by hand 
# break-down:
anova(mod) # (sequential SS)

# SSR
SSR<-sum(anova(mod)[1:7,2])
SSR


# SSE
SSE<-anova(mod)[8,2]
SSE

# SST
anova(null)
SST<-anova(null)[,2]
SST

# R2
SSR/SST


# sigma-hat
np1<-anova(mod)[8,1]
sqrt(SSE/np1)

summary(mod)


### 7 - Non-linear effects 
reglin2 <- read.csv("ch2_part2_A_code/reglin2.csv")
head(reglin2)

# quadratic term for X
nlmod1<-lm(y ~ x+I(x^2),data=reglin2)
summary(nlmod1)


# plot
ggplot(reglin2, aes(x=x, y=y)) +
  geom_point() +
  stat_smooth(aes(y = y),method = "lm", formula = y ~ x + I(x^2), size = 1)


# alternative
reglin2$x2<-reglin2$x^2
head(reglin2)

nlmod2<-lm(y~x+x2,data=reglin2)
summary(nlmod2)

# cubic
nlmod3<-lm(y~x+I(x^2)+I(x^3),data=reglin2)
summary(nlmod3)

 

### 8 - Interactions

reglin3 <- read.csv("ch2_part2_A_code/reglin3.csv")
head(reglin3)

# model without sex effect
mod.int <- lm(intent ~ fix, data=reglin3)
summary(mod.int)

# visualization
ggplot(data = reglin3, aes(x = fix, y = intent)) +
  geom_point(aes(col = as.factor(sex))) +
  geom_smooth(method="lm", se=FALSE, formula="y ~ x", col="black", fullrange=TRUE)

# model without interaction
mod.int1<-lm(intent~as.factor(sex)+fix,data=reglin3)
summary(mod.int1)

# fitted values
pred.int1<-mod.int1$fitted.values

# visualization
ggplot(reglin3, aes(x=fix, y=intent,col=as.factor(sex)))+
  geom_point() +
  geom_line(aes(y = pred.int1), size = 1)

# model with interactions
mod.int2 <- lm(intent ~ as.factor(sex) * fix, data=reglin3) 
summary(mod.int2)


# fitted value
pred.int2<-mod.int2$fitted.values

# visualization
ggplot(reglin3, aes(x=fix, y=intent,col=as.factor(sex)))+
  geom_point() +
  geom_line(aes(y = pred.int2), size = 1)

# alternative
ggplot(data = reglin3, aes(x = fix, y = intent, col = as.factor(sex))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, formula="y ~ x", show.legend=FALSE, fullrange=TRUE)



# model with interactions + other variables
reglin6<-read.csv("ch2_part2_A_code//reglin6.csv")
head(reglin6)

mod.int3<-lm(intent~as.factor(sex)*fix+emo+age+as.factor(rev)+as.factor(educ)+stat,data=reglin6)
summary(mod.int3)
  





### 9 - Multicollinearity

# illustration
ex<-read.table("ch2_part2_A_code//colinear.txt",header = TRUE)
head(ex)

attach(ex)

summary(age-age2)

cor(age, age3)

cor(age, age4)

library(Hmisc)

rcorr(cbind(age, age2, age3, age4))

# age2
summary(lm(height~age,data=ex))

summary(lm(height~age+age2,data=ex))


# age3
summary(lm(height~age,data=ex))

summary(lm(height~age3,data=ex))

summary(lm(height~age+age3,data=ex))

# age4
summary(lm(height~age,data=ex))

summary(lm(height~age4,data=ex))

summary(lm(height~age+age4,data=ex))



reglin8<-read.csv("ch2_part2_A_code//reglin8.csv")
head(reglin8)

library("Hmisc")
rcorr(as.matrix(reglin8))


# VIF
library(car)
mod.col1<-lm(Y~.,data=reglin8)
summary(mod.col1)

vif(mod.col1)

# one way to incoporate the collinear variables:
# Combination of the three variables X1, X2, X3
# and create a new variable that is their average

attach(reglin8)
temp<-reglin8
temp$avg123<-(X1+X2+X3)/3
head(temp)

mod.col2<-lm(Y~avg123+X4+X5,data=temp)
summary(mod.col2)

vif(mod.col2)

