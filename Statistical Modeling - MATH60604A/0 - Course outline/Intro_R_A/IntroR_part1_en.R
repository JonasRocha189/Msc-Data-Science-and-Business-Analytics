knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)

# this is a comment 
a = 1 # this is a comment, but a=1 is an R command 

# Careful: you must change the path to the appropriate one on your own computer! 
setwd("~/Cours HEC/Intro R")

2+3
pi
2+3*pi
log(2+3*pi)
log(2,base=10)
log(2,base=2)
exp(2.435785)

a=3 # Create a scalar variable called a, taking the value 3 
a # Display the value of the variable a
a<-3 # same code, using the <- operator 
a
b=5 # Create a scalar variable called b, taking the value 5 
b # Display the value of b 
b-a # Display the value of b-a 
d=b/a # Store the value of b/a in a new variable called d 
d # Display the value of d

x=c(2,3,1,5,4,6,5,7,6,8) # create a vector x with 10 numerical components 
x # display the value of x 
x[3] # display the 3rd element of x 
a=x[3] # store the 3rd element of x in an object called a 
a # display the value of a

salary=c(36,54,42,81) # salary of 4 employees 
increase=c(0.02,0.02,0.03,0.05) # salary increase for each employee 
salary_final=salary*(1+increase) # salary with increase
salary_final

z=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, byrow=T) # defines matrix by row 
z
z.transpose=matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, byrow=F) # defines matrix by column 
z.transpose

z[2,3] # element from row 2 column 3 
z[,1] # 1st column
z[1,] # 1st row

z=matrix(0, nrow=5, ncol=2) # initially defines matrix with all 0's 
# create 2 vectors s
amount.dollars=c(55,70,100,20,15)
amount.euros=0.66*amount.dollars
# fill each of the columns of the matrix z respectively with the 2 vectors created 
z[,1]=amount.dollars
z[,2]=amount.euros
z 

## read.table(file, header = FALSE, sep = "",dec = ".", row.names, col.names,na.strings = "NA",skip = 0)

## read.table("Data/electricbill.txt", header=TRUE)

# Save the contents of the read.table command in an object called "mydata" (you can call the data whatever you like)
mydata=read.table("Data/electricbill.txt", header=TRUE)

## mydata

str(mydata)
head(mydata)

nrow(mydata)
ncol(mydata)

# Display element [2,3] (i.e., line 2 column 3) of the dataframe 
mydata[2,3]
# Display the first row
mydata[1,]
# Display the first 6 entries of the 3rd column
head(mydata[,3])

# 2nd column, i.e. YEAR
head(mydata$YEAR)

# Save the 5th column of mydata to an object called toto
toto=mydata[,5]
# Display the contents of toto (first 6 elements only)
head(toto)

attach(mydata)
head(YEAR)

# Created a new variable YEAR2 which counts the YEAR from 1 to 10
YEAR2=YEAR-1990
head(YEAR2)

# Convert temperature from Fahrenheit to Celsius
TEMP.CELSIUS=(TEMP-32)/1.8
head(TEMP.CELSIUS)

# Create a new dataframe called mydata2 which contains only the columns
# YEAR, BILL and CONSUMPTION 
mydata2=data.frame(YEAR=mydata$YEAR, BILL=mydata$BILL, CONSUMPTION=mydata$CONSUMPTION)
head(mydata2)

# Modify the existing mydata dataframe, adding the columns
# TEMP.CELSIUS previously created
mydata=data.frame(mydata,TEMP.CELSIUS=TEMP.CELSIUS)
head(mydata)

sex=c("male", "female", "female", "male", "male", "female", "female")
sex
smoking=c("yes", "no", "no", "yes", "no", "yes", "no", "no", "yes", "no")
smoking

sex.factor=as.factor(sex)
sex.factor

ls()

rm(a)
ls()

## # Instal the packages
## install.packages("knitr")
## install.packages("rmarkdown")
## install.packages("markdown")
## 
## # Load the libraries in R
## library(knitr)
## library(rmarkdown)
## library(markdown)
