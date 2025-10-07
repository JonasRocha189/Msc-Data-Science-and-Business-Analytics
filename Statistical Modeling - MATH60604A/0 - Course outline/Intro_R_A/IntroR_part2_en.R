knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)

# Read dataset 
mydata=read.table("Data/electricbill.txt", header=TRUE)
mydata2=data.frame(YEAR=mydata$YEAR, MONTH=mydata$MONTH, 
                   BILL=mydata$BILL, CONSUMPTION=mydata$CONSUMPTION)
attach(mydata2)

length(BILL)
sum(BILL)
sum(BILL^2)
mean(BILL)
var(BILL)
sd(BILL)
sqrt(var(BILL))
summary(BILL)
summary(BILL^2)
quantile(BILL,0.25)
table(YEAR)

summary(mydata2)
nrow(mydata2)
ncol(mydata2)
dim(mydata2)

# Load library
library(MASS)
# generate a random sample from the multivariate normal distribution 
x=mvrnorm(100,mu=1,Sigma=1)

condition=(YEAR==1991)
condition

condition=(YEAR>1995) # year greater than 1995
condition=(YEAR>=1995) # year greater than or equal to 1995
condition=(YEAR!=1992) # year different from 1992

# Extract the elements of the variable YEAR for which the condition holds true 
condition=(YEAR>1995)
YEAR[condition]

# An equivalent (and more direct way) to do this: 
YEAR[YEAR>1995]

# Extract elements from the BILL variable corresponding to the year 1991 
BILL[YEAR==1991]

# Extract observations from the mydata2 dataset corresponding to the year 1991
mydata2[YEAR==1991,]

# Extract observations from the mydata2 dataset with BILL>200 
mydata2[BILL>200,]

# Create a new dataframe containing only observations for the months of january and february 
mydata4=mydata[(MONTH=='Jan' |MONTH=='Feb') ,] 
dim(mydata4)
head(mydata4)

# Create a new dataframe containing only observations with 100<BILL<200 
mydata4=mydata[(BILL>100  & BILL<200) ,] 
dim(mydata4)
head(mydata4)

# Sequence from 1 to 10
z=1:10
z

# Sequence form 5 to 10
z=5:10
z

# Sequence from 1 to 10 with jumps of 2
w=seq(from=1,to=10,by=2)
w

# Sequence from 0 to 10 (of length 11)
w=seq(0,10,length=11)
w

# Repetition of the number 1, repeated 10 times
w=rep(1,10)
w

# Repetition of the sequence 1-2-3, repeated 5 times
w=rep(1:3,5)
w

# Repetition of another sequence, repeated 2 times
w=rep(seq(from=1,to=10,by=2),2)
w

# Extract observations 1,3, and 6 from BILL 
BILL[c(1,3,6)]

# Extract  the first 10 observations from BILL
BILL[1:10]

# Extract the first 10 lines from mydata
mydata2[1:10,]

# Extract every second row from mydata2 for columns (1,2,4)
mydata2[seq(1,nrow(mydata),by=2),c(1,2,4)]

plot(mydata$BILL,mydata$CONSUMPTION) 

plot(mydata$BILL,mydata$CONSUMPTION, main="Scatter plot of bill vs consumption",
     xlab="Bill", ylab="Consumption")

plot(mydata$BILL,mydata$CONSUMPTION, main="Scatter plot of bill vs consumption",
     xlab="Bill", ylab="Consumption",pch=21, bg="red")

plot(mydata$BILL,mydata$CONSUMPTION, main="Scatter plot of bill vs consumption",
        xlab="Bill", ylab="Consumption")
regress=lm(mydata$CONSUMPTION~mydata$BILL)
abline(regress)
abline(h=6000, col="green")
abline(v=100, col="red")

plot(mydata$NUM,mydata$BILL,type="l",xlab="Time",ylab="Bill")

hist(mydata$BILL, main = "Bill", xlab = "Range", ylab="Frequency")

# single boxplot
boxplot(mydata$BILL,main="Bill")

# boxplots side by side
boxplot(mydata$BILL~mydata$MONTH,main="Bill")

par(mfrow=c(1,2))
plot(mydata$BILL,mydata$CONSUMPTION,xlab="Bill", ylab="Consumption")
plot(mydata$TEMP,mydata$CONSUMPTION,xlab="Temp", ylab="Consumption")
par(mfrow=c(1,1))

jpeg("myplot.jpg")
plot(mydata$BILL,mydata$CONSUMPTION)
dev.off()

pdf("myplots.pdf")
# First page of the pdf file 
plot(mydata$BILL,mydata$CONSUMPTION)
# Second page of the pdf file 
plot(mydata$TEMP,mydata$CONSUMPTION)
# Close the file
dev.off()
