## Solutions to Part 1

x=read.table("employees.txt")

# Question 1
sum(x$nSales)
sum(x$TotSales)

# Question 2
which.max(x$nSales)		# Entry number for the record
x[which.max(x$nSales),]		# Value of that entry

# Question 3
tots=rep(0,8)		# Creating a vector of zeroes
emps=unique(x$EmpID)	# Creating a list of employees
for(i in 1:8){
  tots[i]=sum(x$TotSales[x$EmpID==emps[i]])
}
cbind(emps,tots)	# Place data in two columns and show it

# Question 4
AveSales=x$TotSales/x$nSales	# Computing average sale
boxplot(AveSales~x$Month)	# Variation by month
boxplot(AveSales~x$EmpID)	# Variation by employee

# Question 5
stot=rep(0,8)		# Creating a vector of zeroes
ntot=rep(0,8)
emps=unique(x$EmpID)	# Creating a list of the employees
for(i in 1:8){
  stot[i]=sum(x$TotSales[x$EmpID==emps[i]])
  ntot[i]=sum(x$nSales[x$EmpID==emps[i]])
}
stot/ntot

# THIS WOULD BE WRONG:
mean(AveSales[x$EmpID==1000])		# Error!!!

# Question 6
emps=unique(x$EmpID)
par(mfrow=c(2,4))		# Multiple plits in the same window
for(i in 1000:1007){
  plot(1:12,AveSales[x$EmpID==i],type='l',xlab="Mois",
       ylab="Average Sale")
  title(paste("Monthly Average sale for employee",i))
}

# Question 7
x$EmpID=as.factor(x$EmpID)
model=lm(TotSales~Month+EmpID,x)
summary(model)


## Part II

# Question 1
x=read.table("salesmen.txt")
apply(is.na(x),2,sum)

# Question 2
x[x$EmpID%/%100==25,]
x[x$EmpID>=2500 & x$EmpID<2600,]

# Question 3
branch=x$EmpID%/%100
works=!is.na(x$nSales)
etp=tapply(works,branch,sum)/12

# Question 4
extract=function(i){t(matrix(x$TotSales[branch==i],nrow=12))}
listbybranch=lapply(1:25,extract)

tmp=list()
for(i in 1:25){ tmp[[i]]=extract(i) }

# Question 5
totbybranch=sapply(listbybranch,sum,na.rm=TRUE)
pie(totbybranch)
title("Annual total sales by branch")


# Question 6
avebybranch=totbybranch/etp
barplot(avebybranch)
title("Annual sales per FTE for all branches")

# Question 7
salesevolution=function(i,data){
  keep=data$EmpID%/%100==i
  par(mfrow=c(1,2))
  
  tabSales=matrix(data$nSales[keep],nrow=12)
  nSmonthly=apply(tabSales,1,sum,na.rm=TRUE)
  plot(1:12,nSmonthly,type='l',xlab="Month",
       ylab="Number of Sales")
  title("Progression of the monthly number of sales ")
  title(sub=paste("Branch",i))
  
  tabTot=matrix(data$TotSales[keep],nrow=12)
  totmonthly=apply(tabTot,1,sum,na.rm=TRUE)
  plot(1:12,totmonthly,type='l',xlab="Month",
       ylab="Total sales")
  title("Progression of the monthly total sales")
  title(sub=paste("Branch",i))
}
salesevolution(1,x)


## Part III
# Consider the same exercises, but use piping
# Only questions where this is relevant appear next

x=read.table("employees.txt")

# Question 3, Part I
x %>%
  group_by(EmpID) %>%
  summarise(TotSale=sum(TotSales))

# Question 5, Part I
x %>%
  group_by(EmpID) %>%
  summarise(TotSales=sum(TotSales),nSales=sum(nSales)) %>% 
  mutate(AvgSale=TotSales/nSales)


x=read.table("salesmen.txt")

# Question 2, Part II
x %>% mutate(branch=(EmpID%/%100)) %>%
    filter(branch==25)

# Question 3, Part II
x %>% mutate(branch=(EmpID%/%100)) %>%
  group_by(as.factor(branch)) %>%
  summarise(fte=sum(!is.na(nSales))/12)

# Question 4 to 6, Part II
totbybranch = x %>% mutate(branch=(EmpID%/%100)) %>%
  group_by(as.factor(branch)) %>%
  summarise(fte=sum(!is.na(nSales))/12, TotSales=sum(TotSales,na.rm=TRUE)) %>%
  mutate(Avg=TotSales/fte)
totbybranch
pie(totbybranch$Avg)
title("Annual total sales by branch")
