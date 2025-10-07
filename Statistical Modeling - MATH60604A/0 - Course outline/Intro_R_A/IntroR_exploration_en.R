knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)

mydata=read.csv("Data/kc_house_data.csv")
dim(mydata)
head(mydata)

attach(mydata)

summary(mydata$price)

# Price variable 
par(mfrow=c(1,2))
hist(mydata$price)
boxplot(mydata$price,horizontal=F,main="Price",xlab="Price")

# Variable price
par(mfrow=c(1,2))
hist(log(mydata$price))
boxplot(log(mydata$price),horizontal=F,main="log-Price",xlab="Price")

sum(price>2000000)
sum(price>3000000)
sum(price>4000000)

table(bedrooms)

# Bedrooms
par(mfrow=c(2,2))
plot(bedrooms,price,xlab="Nbr of bedrooms",ylab="Price")
plot(bedrooms,log(price),xlab="Nbr of bedrooms",ylab="log-Price")
plot(bedrooms[bedrooms<9],price[bedrooms<9],xlab="Nbr of bedrooms",ylab="Price")
plot(bedrooms[bedrooms<9],log(price[bedrooms<9]),xlab="Nbr of bedrooms",ylab="log-Price")


library(ggplot2)
ggplot(mydata, aes(x=price))

ggplot(mydata, aes(x=price)) + geom_histogram() 

mydata$logprice=log(mydata$price)
head(mydata$logprice)
plot1=ggplot(mydata, aes(x=logprice)) + geom_histogram(binwidth=2) + ggtitle("Bin width 2")
plot2=ggplot(mydata, aes(x=logprice)) + geom_histogram(binwidth=1) + ggtitle("Bin width 1")
plot3=ggplot(mydata, aes(x=logprice)) + geom_histogram(binwidth=0.5) + ggtitle("Bin width 0.5")
plot4=ggplot(mydata, aes(x=logprice)) + geom_histogram(binwidth=0.1) + ggtitle("Bin width 0.1")

gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

plot1=ggplot(mydata, aes(y=logprice)) + geom_boxplot()
plot2=ggplot(mydata, aes(y=logprice)) + geom_boxplot(fill="gray", color="black")+ theme_classic()

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

mydata$floors_fact=as.factor(mydata$floors)
plot1=ggplot(mydata, aes(x=floors_fact,y=logprice)) + geom_boxplot()
plot2=ggplot(mydata, aes(x=floors_fact,y=logprice,color=floors_fact))  + geom_boxplot()+ labs(title="Log-Price by number of floors",x="Nbr of floors", y = "Log-Price")

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

plot1=ggplot(mydata,aes(x=bedrooms,y=logprice))+ geom_point(alpha = .3) + ggtitle("Log-Price vs bedrooms")

plot2=ggplot(mydata,aes(x=bedrooms,y=logprice))+ geom_point(alpha = .3) + ggtitle("Log-Price vs bedrooms") +  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed")

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

library(ggridges)
mydata$fcondition=factor(mydata$condition,labels=c("poor", "acceptable", "average", "good", "very good"))
ggplot(mydata, aes(x = price, y = fcondition)) + ggridges::geom_density_ridges() + scale_x_continuous(labels= scales::dollar)+ labs(title="Price by condition",x="Price", y = "Condition")
