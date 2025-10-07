knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)

add.2.numbers = function(a,b)
{
   sum = a+b
   return(sum)
}
add.2.numbers(3,4)

# Alternative way to call on the function
a=3
b=4
add.2.numbers(a,b)

myfunction = function(a,b)
{
   sum = a+b
   diff=a-b
   return(list("mysum"=sum,"mydiff"=diff))
}
myfunction(3,4)

# Display successive numbers
for (i in 1:10) 
{
   print(i^2) 
}

# Another example
# Display a series of elements of a given set 
for (w in c("red", "blue", "green")) 
{
   print(w) 
}


# Iteratively fill the object x 
x=NULL 
for(i in 1:10)
{
  x=c(x,i)
}
x  

sales=function(x,emp)
{ 
# x : dataset 
# emp : ID of the employee we want to analyse 
# output : graph 
   if(sum(x$EmpID==emp)==0) stop("This employee does not exist.") 
   par(mfrow=c(1,2)) 
   plot(1:12,x$nSales[x$EmpID==emp],type='l',xlab="Month", 
        ylab="Number of transactions") 
   title(paste("Number of transactions of employee",emp,"by month")) 
   plot(1:12,(x$TotSales/x$nSales)[x$EmpID==emp],type='l',
        xlab="Month", ylab="Average sale") 
   title("Average amount of a sale per month") 
}

# Create a vector x which contains an NA value 
x=c(1,NA,5.6,-1,0) 

# Return indicators of missing values (form TRUE/FALSE) 
is.na(x) 

# What happens if we try to take the log of a NA value?
y=log(x) 
y

# Return indicators for missing values (NA), not a number (NaN), finite number, in the form TRUE/FALSE
is.na(y) 
is.nan(y) 
is.finite(y)


mean(x)
# specifying na.rm=TRUE will ignore missing values
mean(x,na.rm=TRUE)

sum(is.na(x)) # Number of missing values 
x[!is.na(x)] # display non-missing values of the vector x

nb.na=function(x,p=.1)
{ 
  # x : a vector 
  # p : acceptable proportion of missing values 
  # output : number of missing values 
  
  if(mean(is.na(x))>=p) {warning("There are too many missing values.")} 
  return(sum(is.na(x)))
}


mydata=read.table("Data/salesmen.txt",header=T)

# Re-format data in array form 
# Dimensions: 12 (month) * 150 (emp) * 4 (variables)
mydata_array=array(as.matrix(mydata),c(12,150,4)) 

# Data for 1st employee, 1st month 
mydata_array[1,1,]

# Data for 1st employee
mydata_array[,1,]

# Data for 1st month (showing only first six lines)
head(mydata_array[1,,])

# Number of sales for all employees in january (showing only first 6 lines) 
head(mydata_array[1,,c(1,3)])

# Average monthly sale amounts for 3rd employee 
mydata_array[,3,4]/mydata_array[,3,3] 

mydata=read.table("Data/salesmen.txt",header=T)

# Creating a list with 5 elements:
# year, branch, ID, total number of monthly sales, total monthly sales amount 
mydata_list=list(year=2012, succ="Montreal Centre", ID=unique(mydata$EmpID),
                 nb=t(matrix(mydata$nSales,nrow=12)), 
                 amount=t(matrix(mydata$TotSales,nrow=12)) )
names(mydata_list) 

# Extract the monhtly number of sales for all employees (displaying only first 6 lines)  
head(mydata_list$nb) 

# Alternative way to obtain same thing ...
head(mydata_list[[4]])

# Extract the monthly number of sales for the first employee only 
mydata_list[[4]][1,]

# Rows = month
# Columns = employees
nb_sale=mydata_array[,,3] 
dim(nb_sale)

# Apply the mean function to all rows (i.e., all months) of the matrix
# this will yield the monthly average number of sales 
apply(nb_sale,1,mean) 

# Same thing, this time specifying the na.rm=T option within the mean function 
apply(nb_sale,1,mean,na.rm=T)

# Apply the mean function to all columns (i.e., all employees) of the matrix
# this will yield the average sales for each employee
# (here, showing only first few lines) 
head(apply(nb_sale,2,mean,na.rm=T))

# Minimum and maximum value by month 
apply(nb_sale,1,range,na.rm=T)

# Minimum and maximum value by employee, for the first 10 employees 
apply(nb_sale[,1:10],2,range,na.rm=T)

fn=function(i)
{
     sum((1:10)^i)
} 
sapply(0:5,fn)


tapply(mydata$nSales,as.factor(mydata$EmpID),sum)
