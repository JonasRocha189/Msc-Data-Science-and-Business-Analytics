knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)

library(magrittr)
mydata=read.table("Data/salesmen.txt",header=T)

# Equivalent codes, without and with piping
nrow(mydata)
mydata %>% nrow

# Another example:
x <- c(1,2,5,6)
round(exp(diff(log(x))), 1)

x %>% log() %>%
    diff() %>%
    exp() %>%
    round(1)

library(dplyr)

# Create a dataframe containing the columns EmpID and Month from mydata
data1=select(mydata,EmpID,Month)
head(data1)

# Create a dataframe removing the columns nSales and TotSales from mydata
data2=select(mydata, -nSales, -TotSales)
head(data2)

# Create a dataframe including all columns between EmpID and nSales from mydata
data3 = select(mydata, EmpID:nSales)
head(data3)

data4=select(mydata, contains("Sales"))
head(data4)

data5=select(mydata, starts_with("Emp"))
head(data5)

# Extract observations for which EmpID > 1005
data6 = filter(mydata, EmpID > 1005)
head(data6)

# Extract observations for which nSales <= 10
data7 = filter(mydata, EmpID > 1005, nSales <= 10)
head(data7)

# Extract observations for which EmpID > 1005 and nSales <= 10
data8 = filter(mydata, EmpID > 1005 & nSales <= 10) 
head(data8)

# Extract observations for which EmpID > 1005 or nSales <= 10
data9 = filter(mydata, EmpID == 1000 | EmpID == 1005)
head(data9)

# Extract observations for which EmpID =1003 or 1005
data10 = filter(mydata, EmpID %in% c(1003, 1005))
head(data10)

subset(mydata,EmpID==1000&Month<=5)
subset(mydata,EmpID==1000&Month<=5, select=c(Month, nSales, TotSales))

data1=matrix(1:25,ncol=5,byrow=T)
data1

data2=matrix(1:15,ncol=5,byrow=T)
data2

# Combine by row 
rbind(data1,data2)

# Combine by column 
data3=matrix(1:15,nrow=5,byrow=T)
cbind(data1,data3)

part1 = read.table("Data/sales_part1.txt")
colnames(part1)
unique(part1$EmpID)

part2 = read.table("Data/sales_part2.txt")
colnames(part2)
unique(part2$EmpID)

part3 = read.table("Data/sales_part3.txt")
colnames(part3)
unique(part3$EmpID)

# Combine the data in part1 and part 2 by row
sales_combine=bind_rows(part1, part2)
head(sales_combine)
tail(sales_combine)

sales_full=full_join(x=sales_combine, y=part3, by=c("EmpID", "Month"))
head(sales_full)
tail(sales_full)

sales_left=left_join(x=part1, y=part3, by=c("EmpID", "Month"))
head(sales_left)
tail(sales_left)

sales_right=right_join(x=part1, y=part3, by=c("EmpID", "Month"))
head(sales_right)
tail(sales_right)

emp_comb=rbind(part1, data.frame(part2, Team="B"))
sales_merge2 = merge(x=emp_comb, y=part3, by=c("EmpID", "Month"), all=TRUE)
head(sales_merge2)
tail(sales_merge2)

head(aggregate(mydata$nSales~mydata$EmpID, FUN=sum, na.rm = TRUE))

head(aggregate(cbind(mydata$nSales, mydata$TotSales)~mydata$EmpID,FUN=sum, na.rm = TRUE))
