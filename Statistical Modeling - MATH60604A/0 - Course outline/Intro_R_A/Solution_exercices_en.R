setwd("~/Cours HEC/Intro R")

###############
# PART 1
##############

# Final exercise

mydata=read.table("Data/babyboom.txt",header=T)
head(mydata)
nrow(mydata)
ncol(mydata)
mydata[,1:2]
mydata[5,]

mydata2=data.frame(Time=mydata$Time,Sex=mydata$Sex,Weight=mydata$Weight)
mydata2$Weight_livre=mydata2$Weight*0.00220462
head(mydata2)


###############
# PART 2
##############

# Create a new dataframe called mydata.winter which contains only observations for the months 
# of Dec-Jan-Feb for which BILL>100. 
# For this new dataset, calculate the mean, minimum and maximum values for the BILL variable. 
# Using the cor function, calculate the correlation between the variales BILL and CONSUMPTION.


mydata=read.table("Data/electricbill.txt", header=TRUE)
attach(mydata)
mydata.winter=mydata[(MONTH=="Dec" | MONTH=="Jan" | MONTH=="Fev")& BILL>100,] #careful with brackets here...
mean(mydata.winter$BILL)
min(mydata.winter$BILL)
max(mydata.winter$BILL)
cor(mydata.winter$BILL,mydata.winter$CONSUMPTION)

# Extract columns (1,2,4) from mydata2 for every second row. 
mydata=read.table("Data/electricbill.txt", header=TRUE)
mydata2=data.frame(YEAR=mydata$YEAR, MONTH=mydata$MONTH,BILL=mydata$BILL, CONSUMPTION=mydata$CONSUMPTION)
attach(mydata2)
id=seq(1,nrow(mydata2),by=2)
result=mydata2[id,c(1,2,4)]
nrow(mydata2)
nrow(result)


# Exercise: 
# 1) Create a scatter plot of BILL vs. YEAR for the month of January only. 
# 2) Create a scatter plot of BILL vs. YEAR for the month of June only. 
# 3) Create a scatter plot of BILL vs. YEAR for the month of August only. 
# 4) Create a scatter plot of BILL vs. YEAR for the month of October only. 
# 5) Display the 4 figures on the same page (2 rows 2 columns) and save it in pdf format.

mydata=read.table("Data/electricbill.txt", header=TRUE)
attach(mydata)
pdf("Myplot.pdf")
par(mfrow=c(2,2))
plot(YEAR[MONTH=="Jan"],BILL[MONTH=="Jan"],xlab="YEAR",ylab="BILL",main="Jan")
plot(YEAR[MONTH=="Jun"],BILL[MONTH=="Jun"],xlab="YEAR",ylab="BILL",main="Jun")
plot(YEAR[MONTH=="Aug"],BILL[MONTH=="Aug"],xlab="YEAR",ylab="BILL",main="Aug")
plot(YEAR[MONTH=="Oct"],BILL[MONTH=="Oct"],xlab="YEAR",ylab="BILL",main="Oct")
dev.off()

# Final exercise
# 1) Produce descriptive statistiscs for the variable weight.
# 2) Produce descriptive statistiscs for the variable weight, separately for baby girls and boys.
# 3) Create 2 boxplots of birthweight as a function of sex.
# 4) What proportion of baby girls had a weight less than 3 kg? Compare this with the proportion for boys.
# 5) Display the birthweights of all girls born at least 2 hours after midnight.
# 6) What is the ID of the baby girl with the lowest weight?
# 7) Create a scatter plot of weight as a function of the variable time.midnight.

mydata=read.table("Data/babyboom.txt",header=T)
attach(mydata)
summary(Weight)
summary(Weight[Sex==1])
summary(Weight[Sex==2])
boxplot(Weight~Sex)
sum(Weight[Sex==1]<3000)/sum(Sex==1)
sum(Weight[Sex==2]<3000)/sum(Sex==2)
Weight[Sex==1 & Time.midnight>=120]
# ID of girl with lowest weight 
which.min(Weight[Sex==1]) # careful, this code does not work ! 
id=1:nrow(mydata)
toto=which.min(Weight[Sex==1])
id[Sex==1][toto]

plot(Weight,Time.midnight,xlab="Weight",ylab="Time.midnight")


###############
# PART 3
##############

# Create a function that takes as input a dataset (dataframe) and a vector representing column indices. 
# The function should return, for each column, the number of observations whose value is greater 
# than the column mean. 
mydata=read.table("Data/salesmen.txt",header=T)
myfunction=function(data,id)
{
  result=NULL
  for (i in id)
  {
    moy=mean(data[,i],na.rm=T)
    res=sum(data[,i]>moy,na.rm=T)
    result=c(result,res)
  }
  return(result)
}
myfunction(mydata,id=c(3,4))


# Arrays: Working with the array previously created, 
# display the number of sales for the month of March for all employees whose ID starts with 10. 
mydata=read.table("Data/salesmen.txt",header=T)
mydata_array=array(as.matrix(mydata),c(12,150,4))
extrait1=mydata_array[3,,c(1,3)] # Mois de mars, nbre de ventes
id=extrait1[,1]
extrait1[(id>=100 & id<200) | (id>=1000 & id<1100),]

# Lists: Display all the data for the total monthly sales amounts for 
# the month of March for all employees whose ID starts with 10. 
mydata_list=list(year=2012, succ="Montreal Centre", ID=unique(mydata$EmpID), 
                 nb=t(matrix(mydata$nSales,nrow=12)), 
                 amount=t(matrix(mydata$TotSales,nrow=12)) )
test=(mydata_list$ID>=100 & mydata_list$ID<110) | (mydata_list$ID>=1000 & mydata_list$ID<1100) 
mydata_list$nb[test ,3]

# Final exercise
mydata=read.table("Data/salesmen.txt",header=T)

# 1) How many missing values are there for each variable in the dataset? 
apply(is.na(x),2,sum)

# 2) What do you notice about the employees at Branch 25? 
#    Can you find a plausible reason for the missing values?
# Note: %/% indicates integer division 
mydata[mydata$EmpID%/%100==25,]
mydata[mydata$EmpID>=2500 & mydata$EmpID<2600,] # Equivalent, alternative approach 

# 3) Determine how many full-time equivalent employees there are for each branch. 
#    Note that if an employee worked for 3 months, 
#    that is equivalent to 3/12 of a full-time employee.
# Note on tapply: options are (vector, index, function)
branch=mydata$EmpID%/%100
works=!is.na(mydata$nSales)
etp=tapply(works,branch,sum)/12

# 4) Create a list where each item is a matrix of the total number of sales 
#    (rows = employees and col = months) for a branch
extract=function(i)
{
     t(matrix(mydata$TotSales[branch==i],nrow=12))
}
listbybranch=lapply(1:25,extract)
# alternative way to do this 
tmp=list(); 
for(i in 1:25){ tmp[[i]]=extract(i) }

# 5) Using one of the apply commands, calculate the total number of sales for each branch. 
#    Make a graph that illustrates the contribution of each branch to the company's sales 
#    (you can use the pie and barplot functions).
totbybranch=sapply(listbybranch,sum,na.rm=TRUE)
pie(totbybranch)
title("Annual total sales by branch")


# 6) Calculate a performance measure: the sales of each branch per full-time 
#    equivalent employee. Create a graph that allows you to compare the 
#    efficiency of the branches.
avebybranch=totbybranch/etp
barplot(avebybranch)
title("Annual sales per FTE for all branches")

# 7) Create a function that takes as input the ID of a branch and returns 
#    a summary of the evolution of sales (number and total sales) over time.
salesevolution=function(i,data)
{
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
salesevolution(1,mydata)
