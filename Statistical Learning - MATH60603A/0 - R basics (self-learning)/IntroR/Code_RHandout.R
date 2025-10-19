### Code from the R handout

## Introduction to R

# Creating objects
# NOTE: Type x after each line to see what the object has become
x=2	
x=c(1,4,5.6,9)
x=1:10
x=rep(1:3,c(2,5,3))
x=rep(c("A","B","C"),each=5)


pi
pi=4
pi
rm(pi) # erases the obejct
pi

# Operations on vectors
n=c(98,10,3,1) 				# number of employees per category
salary=c(36,42,54,81)*1000		# salary of employees per category
salary/12					# monthly salary per category
n*salary					# payroll per category
inc=c(.02,.02,.03,.05)			# salary increase per category
incsalary=salary*(1+inc)			# new salaries

sum(n*incsalary)				# payroll after increase
allsal=rep(incsalary,n)			# list of the salary of each employee
mean(allsal)					# mean salary
sd(allsal)					# standard deviation of the salary
quantile(allsal,c(.1,.9))		# quantiles 10% and 90% of salaries
summary(allsal)

# Types of variables
x=rep(c("M","W"),c(4,5))
y=as.factor(x)
is.factor(y)
summary(x)
summary(y)

mean(x=="M")

# Accessing parts of a vector
x=2001:2019
x[4]
x[c(3,4)]		# you may provide a vector of elements to retain,
x[-7]			# a vector of elements to remove
x[x>2006]		# or a vector of logical values.

# Matrices
x=matrix(1:12,3,4)
x
x[2,2]
x[-2,]
x[x>5]

t(1:3) %*% 1:3
1:3 %*% t(1:3)

# Loops and conditional statements
for(i in 1:10){
  if(i>5){
    cat("Number",i,"is greater than 5\n")
  }
  else {
    cat("Number",i,"is less than or equal to 5\n")
  }
}

# Graphics
x=rnorm(30)		# Randomly generated data
hist(x)
boxplot(x)

group=sample(1:2,30,replace=TRUE)	# random choice of a group for each
boxplot(x~group)

year=2001:2019
sales=rnorm(19,mean=10000+10*year,sd=100)   # randomly generated numbers
plot(year,sales,type='l')
title("Evolution of sales")

# Reading a data file

# NOTE: Write complete path, or change the working directory 
x=read.table("Data/employees.txt")	
x
names(x)				# Name of variables in x
unique(x[,1])			# Removes repeated values
boxplot(x[,3]~x[,1])
title("Monthly number of sales per employee")

boxplot(x$TotSales~x$Month,col="gray",lty=1,pch=20,xlab="Month")
title("Total sales of each employee per month")

emps=unique(x$EmpID)
par(mfrow=c(2,3))		# The par function controls some graphics properties
for(i in 1:6){
  plot(1:12,x$TotSales[x$EmpID==emps[i]],type='l',xlab="Month",
       ylab="Total Sales",ylim=range(x$TotSales))
  title(paste("Montthly sales of employee",emps[i]))
}

# Structure of statistical analyses
x=read.table("Data/Big5.txt")
head(x)
reg=lm(Amount ~ Extraversion, data=x)
print(reg)		# Summary of the fit
summary(reg)		# More details on the fit
plot(reg)		# Plots of residuals
predict(reg)		# Predictions for the dataset
names(reg)		# To see the name of the elements of the list

mreg=lm(Amount ~ Neuroticism + Extraversion + Openness + Agreeableness + Conscientiousness, data=x)
summary(mreg)
step(mreg)		# Stepwise method

# Exercises
# Solutions to exercices are provided in the slides.
# To learn coding, you need to create some programs yourself.
# Understand the syntax is a first step, but you also need to be able to develop solutions from scratch.


# Using the same employees.txt file, answer the following questions.

emp = read.table("Data/employees.txt")
emp

# 1. What is the total number of sales transactions for this company during the year (for all employees)? What is the total value of those sales?

emp.sales <- sum($TotSales) 
emp.sales





## Introduction to R: lists, arrays, functions and graphics

# Missing values
x=c(1,NA,5.6,-1,0)
is.na(x)
y=log(x)
y
is.na(y)
is.nan(y)
is.finite(y)

mean(x)
mean(x,na.rm=TRUE)

sum(is.na(x))		# Number of missing values
x[!is.na(x)]			# Vectors of non missing values

# Empty object (NULL)
rm(x)
c(1:5,x)

x=NULL
for(i in 1:10){
  x=c(x,i)
}
x

# Logical operators

x=2
x==2
x>0
y=1:10
y!=5
y[y!=5]

x=1:10
(x<7) & (x>3)
x[(x<7) & (x>3)]

# Lists

w=read.table("employees.txt")
d=list(year=2012, 
       succ="Montreal Centre",
       ID=1000:1007,
       nb=t(matrix(w$nSales,nrow=12)),
       amount=t(matrix(w$TotSales,nrow=12))
)
names(d)
d$nb
d[[4]]
d[[4]][1,]

x=1:20
y=4+2*x+rnorm(20)	# Creates a dataset
a=lm(y~x)		# Fits a linear regression
summary(a)
names(a)
a$residuals
a$coeff

# data.frame
sex=as.factor(rep(c("M","W"),each=48))
w=data.frame(w,sex)	# Add the sex variable to an existing data.frame
summary(w)
w$EmpID=as.character(w$EmpID)
summary(w)

w[1,]
w[w$EmpID=="1000",]

w$EmpID=as.factor(w$EmpID)
summary(w)
summary(w$EmpID)

# Functions
nb.na=function(x,p=.1){
  # x : a vector
  # p : proportion of missing values that is acceptable
  # output : number of missing values 
  if(mean(is.na(x))>=p) warning("There are too many missing values.")
  sum(is.na(x))
}

nb.na(c(1,2,3,4,NA))
nb.na(c(1,2,3,4,NA),p=.25)

sales=function(x,emp){
  # x   : dataset
  # emp : employee ID whose data we want to analyze
  # output : graphics for employee emp
  
  if(sum(x$EmpID==emp)==0) stop("This employee does not exist.")
  par(mfrow=c(1,2))
  plot(1:12,x$nSales[x$EmpID==emp],type='l',xlab="Month",
       ylab="Number of transactions")
  title(paste("Number of transactions of employee",emp,"by month"))
  plot(1:12,( x$TotSales/x$nSales)[x$EmpID==emp],type='l',xlab="Month",
       ylab="Average sale")
  title("Average amount of a sale per month")
}

sales(w,1001)
sales(w,1006)
sales(w,1)

# Recycling
cbind(1:10,1:3)	# Sticks vectors side by side into a matrix
matrix(1:4,4,5)	# Creates a 4 x 5 matrix from a single vector

sum(w$nSales*c(1,1,1,0,0,0,0,0,0,0,0,0))	# Nb sales Jan to Mar

# Arrays
w=read.table("employees.txt")
a=array(as.matrix(w),c(12,8,4))
a[1,1,]

a[1,,c(1,3)]		# Number of sales in January for all employees
a[,3,4]/a[,3,3]	# Average monthly sale for the 3rd employee

# Implicit “apply” loops
nbv=a[,,3]			# Number of sales : lines = month, col = employees
apply(nbv,1,mean)		# Average per month
apply(nbv,2,mean)		# Average per employee
apply(nbv,1,range)		# Min and max values per month
apply(nbv,2,range)		# Min et max values per employee

                            # Average for number and total values of sales
apply(a[,,3:4],c(1,3),mean)	# Per month
apply(a[,,3:4],c(2,3),mean)	# Per employee

fn=function(i){sum((1:10)^i)}
sapply(0:5,fn)

tapply(w$nSales,as.factor(w$EmpID),sum)

# Parallel computing

fn=function(i){ sum(log(sqrt((1:10000)*i))) }
system.time(sapply(1:10000,fn))		# Using one core
library(parallel)
cl=makeCluster(4)
system.time(parSapply(cl,1:10000,fn))	# Using 4 cores

# More graphics

pie(apply(nbv,2,mean),labels=unique(w$EmpID))
title("Proportion of sales by each employee")

barplot(nbv[,1],names.arg=month.abb[1:12])
title("Number of sales per month for employee 1000")

# Using a “package”

install.packages("depth")	# Run just the first time
library(depth)			# Run once if you need the package in a session

x=read.table("salesmen.txt")
jan=x[x[,2]==1,4]/x[x[,2]==1,3]
feb=x[x[,2]==2,4]/x[x[,2]==2,3]
ID=unique(x$EmpID)
data=cbind(jan,feb)[ID<1100,]
data=data[complete.cases(data),] 
isodepth(data) 			# Plot average amount of a sale Jan vs Feb
isodepth(data,twodim=FALSE)
med(data)

# Exercises
# Solutions to exercices are provided in the slides.
# To learn coding, you need to create some programs yourself.
# Understand the syntax is a first step, but you also need to be able to develop solutions from scratch.







## Introduction to R: Additional topics
# Split-apply-combine – Concept

install.packages("dplyr")
library("dplyr")
employees=read.table("employees.txt")
nrow(employees)  #traditionnal way of using nrow
employees %>% nrow   #using the pipe

nrow(filter(employees, EmpID == 1000))
employees %>%
  filter(EmpID == 1000) %>%
  nrow()

# The dplyr library
# Make sure to observe the result of each of these operations by typing the name of the variable
# to display it, or looking at the Environment tab in RStudio.

empl_mo = select(employees, EmpID, Month)
empl_mo2 = select(employees, -nSales, -TotSales)  # Equivalent in this case

empl_mo_ns = select(employees, EmpID:nSales)

empl_sales = select(employees, contains("Sales"))
empl_id = select(employees, starts_with("Emp"))

empl_fil_emp = filter(employees, EmpID > 1005)
empl_fil_emp_sale = filter(employees, EmpID > 1005, nSales <= 10)
empl_fil_emp_sale = filter(employees, EmpID > 1005 & nSales <= 10) #commas are &
empl_fil_emp_or = filter(employees, EmpID == 1000 | EmpID == 1005)
empl_fil_ord = filter(employees, EmpID %in% c(1003, 1005))

# Combining data with dplyr 
emp_part1 = read.table("sales_part1.txt")
emp_part2 = read.table("sales_part2.txt")
emp_part3 = read.table("sales_part3.txt")
sales_comb=bind_rows(emp_part1, emp_part2) 
sales_full=full_join(x=sales_comb, y=emp_part3, by=c("EmpID", "Month"))
sales_left=left_join(x=emp_part1, y=emp_part3, by=c("EmpID", "Month"))
sales_right=right_join(x=emp_part1, y=emp_part3, by=c("EmpID", "Month"))

# Piping with dplyr

empl_transform = employees %>%
  select(EmpID, TotSales, nSales) %>%
  filter(EmpID <= 1005) %>%
  group_by(EmpID) %>%
  summarise(TotSales = sum(TotSales), nSales = sum(nSales)) %>%
  mutate(Ratio = TotSales / nSales) %>%
  arrange(desc(Ratio))

# Other tools for data manipulation

emp_comb=rbind(emp_part1, data.frame(emp_part2, Team="B"))

# No common key, data are sorted, each line correspond to the same individual
emp_comb = emp_comb[order(emp_comb$EmpID, emp_comb$Month),]
emp_part3 = emp_part3[order(emp_part3$EmpID, emp_part3$Month),]
sales_merge1 = cbind(emp_comb, emp_part3)
sales_merge1 = cbind(emp_comb, emp_part3$TotSales)  # Avoids repeating columns

sales_merge2 = merge(x=emp_comb, y=emp_part3, by=c("EmpID", "Month"), all=TRUE)

# Left join
sales_leftjoin=merge(x=emp_part1, y=emp_part3, by=c("EmpID", "Month"), all.x=TRUE)
# Right join
sales_rightjoin=merge(x=emp_part1, y=emp_part3, by=c("EmpID", "Month"), all.y=TRUE)
# Cross join
tm = data.frame(Team=(c("A","B","C","D")))
employees_tm = merge(employees, tm, by=NULL)

# Aggregation
aggregate(employees$nSales~employees$EmpID, FUN=sum, na.rm = TRUE)
aggregate(cbind(employees$nSales, employees$TotSales)~employees$EmpID, FUN=sum, na.rm = TRUE)

aggregate(employees$nSales~employees$EmpID+employees$Month, FUN=sum, na.rm = TRUE)

# Subsetting
subset(employees,EmpID==1000&Month<=5)
subset(employees,EmpID==1000&Month<=5, select=c(Month, nSales, TotSales))

