# Practice of Simple Linear Regression

# Loading Data
intention <- read.csv("intention.csv")
head(intention)


### 1 - EXPLORING THE DATA

# Summary
summary(intention)

# Alternatively
summary2 <- sapply(intention, function(x) c(mean(x), sd(x), min(x), max(x), length(x) ) )
row.names(summary2) <- c("mean", "sd", "min", "max", "n")
summary2

apply(intention[,c(3,5:7)],2,table)


# Histograms
par(mfrow=c(2,2))
hist(intention$intent, xlab = "intention", main = "Histogram")
hist(intention$age, xlab = "age", main = "Histogram")
hist(intention$fix, xlab = "fixation", main = "Histogram")
hist(intention$emo, xlab = "emotion", main = "Histogram")

# Scatterplot and Correlations
par(mfrow=c(1,2))
#
# intention, fixation
plot(intent~fix, xlab="fixation", ylab="intention", data=intention, lwd=1.5)
# include best linear model
abline(lm(intent~fix, data=intention), lwd=1.5)
#
# intention, emotion
plot(intent~emo, xlab="emotion", ylab="intention", data=intention, lwd=1.5)
abline(lm(intent~emo, data=intention), lwd=1.5)



### 2 - CORRELATION

attach(intention)

# Correlation
cor(cbind(intent,fix,emo))

# Test
cor.test(intent, fix)
cor.test(intent, emo)

# Test - alternative
library("Hmisc")
rcorr(cbind(intent, fix, emo))


### 3 - ESTIMATION: SIMPLE LINEAR REGRESSION

# Simple Linear Regression
lmod <- lm(intent~fix)
lmod
### Slope (B1) = 1.14
### Intercept (B0) = 6.45

summary(lmod)
### Hypothesis: H0 = 0 , H1 != 0
### P-value < 0.05: So we can reject H0 
### We can conclude fixation has significant (linear) effect on intention

# Confidence Intervals
confint(lmod, level=0.95)
confint(lmod, level=0.99)
# A 95% C.I. for βˆ1 is (0.7014641, 1.586703). Since the C.I. does not contain 0, we can conclude that β1 is significantly different from 0 



### 4 - PREDICTION

# create a new dataframe
# variables from 1 - 10
new <- data.frame(fix = c(1:10))
new

predict(lmod, newdata=new)

# estimate + confidence interval for estimated mean
predict.lm(lmod, newdata=new, interval=c("confidence"), level=0.95)

# predictions + prediction intervals
predict.lm(lmod, newdata=new, interval=c("prediction"), level=0.95)

# displaying things together
library("dplyr")
pred1 <- data.frame(predict.lm(lmod, newdata=new, interval=c("confidence"), level=0.95))
pred2 <- data.frame(predict.lm(lmod, newdata=new, interval=c("prediction"), level=0.95))

predictions <- left_join(pred1, pred2, by=c("fit"))
names(predictions) <- c("prediction", "lwr.ci", "upr.ci", "lwr.pi", "upr.pi")
predictions

# graphs
library(ggplot2)

pred.inter <- predict(lmod, interval="prediction")
intention_new <- cbind(intention, pred.inter)

# linear trend, confidence + predictions intervals
ggplot(intention_new, aes(x=fix, y=intent)) + 
  geom_point() +
  geom_smooth(method = lm, color = "red", se = TRUE) + 
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(y=upr), color = "red", linetype = "dashed") 



### 5 - RESIDUALS

# ordinary residuals
resid(lmod)
# standardized residuals
rstandard(lmod)
# jackknife
rstudent(lmod)

# plots using ggplot
# preparing a dataframe with residuals
library("ggplot2")
res.dat <- data.frame(cbind(intent, fix, lmod$fitted, lmod$residuals, rstandard(lmod), rstudent(lmod)))
names(res.dat) <- c("intent", "fix", "fitted", "resid", "rstand", "rstud")
head(res.dat)

# resid vs fix + smooth
ggplot(data = res.dat, aes(x = fix, y = rstud)) +
  geom_point() +
  geom_smooth() + 
  theme(legend.position = "bottom") + 
  ylab("residuals") +
  xlab("fixation")

# resid vs. fitted + smooth
ggplot(data = res.dat,
       aes(x = fitted, y = rstud)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom") +
  ylab("residuals") +
  xlab("fitted values")

# histogram rstud
ggplot(data = res.dat, mapping = aes(x = rstud)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.5) +
  xlab("residuals")

# qqplot rstud
ggplot(data = res.dat, mapping = aes(sample = rstud)) +
  stat_qq(distribution = qt, dparams = lmod$df.residual) +
  stat_qq_line(distribution = qt, dparams = lmod$df.residual) +
  labs(x = "theoretical quantiles",
       y = "empirical quantiles") +
  ggtitle("QQ-Plot Studentized Residuals")


## These plots (histogram + qq-plot) suggest that the assumption of normality is reasonably met here.


# plots using basic R functions:
hist(rstudent(lmod),xlab="Residuals",main="Histogram")

# qqplot: alternative
library("EnvStats")
qqPlot(rstudent(lmod),
       distribution = "t", param.list=list(df = lmod$df.residual),
       add.line=TRUE)


# scatterplots
plot(rstandard(lmod)~lmod$fitted.values,
     xlab="fitted values",ylab="residuals")
abline(h=0)

plot(rstandard(lmod)~fix,
     xlab="fitted values",ylab="residuals")
abline(h=0)




### 6 - R²

summary(lmod)
# Here, we obtain R2 = 0.182, indicating that fix explains 18.4% of the variability in intent.



### 7 - Binary predictor

# Checking the sex as binary
lmod1 <- lm(intent~sex) 
summary(lmod1)

# Checking sex as factor
lmod2 <- lm(intent~as.factor(sex))
summary(lmod2)

# Here we obtain βˆ1 = 1.37, so we can say that the mean intention to buy score is 1.37 units higher for women
# than for men. In other words, on average, women are more interested in buying the product than men.
# Moreover, this difference is significant (p-value 0.01)

# Change the sex reference, from 1 (female) to 0 (male)
levels(as.factor(sex))
sex<-relevel(as.factor(sex), 2)
lmod3 <- lm(intent~sex)
summary(lmod3)

# In this case, the parameter estimates are different, but the model itself is indeed still equivalent. Both models yield:
#  Eb(intention|sex = 0) = 7.55
# Eb(intention|sex = 1) = 8.92
# With this second model, βˆ1 has the same magnitude but different sign as now it represents the difference in
# the mean intention to by for males vs. females.