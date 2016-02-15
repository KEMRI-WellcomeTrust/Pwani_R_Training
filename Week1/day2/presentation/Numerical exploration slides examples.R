## Numerical data exploration using R
## By: Waithaka Michael
# Feb 2016

#Setting up the working directory
setwd("/Users/mwaithaka/Documents/Professional/1. KEMRI /4. Sundry/Training/2016_Feb_Pwani R training")

############ Managing the data ###########
# Read in the data
data <- read.csv("data/bwmal.csv")

# Data dimension
dim(data)

#Structure of the data
str(data)

# converting to factor : method 1
data$sex = as.factor(data$sex)
data$smoke = as.factor(data$smoke)
data$pfplacen = as.factor(data$pfplacen)
data$parity = as.factor(data$parity)
data$workload = as.factor(data$workload)
data$matagegp = as.factor(data$matagegp)
data$gestcat = as.factor(data$gestcat)
str(data)

    # Alternatively : method 1
    names <- c('sex' ,'smoke' ,'pfplacen' ,'parity' ,'workload' ,'matagegp' ,'gestcat')
    data[,names] <- lapply(data[,names] , factor)
    str(data)

    # Alternatively : method 1
    names <- c(5,7:12)
    data[,names] <- lapply(data[,names] , factor)
    str(data)

# Explore variable names of the dataset
names(data)

# The dataset at a glance
head(data)
tail(data)

#Viewing data contents of a variable
data$sex
data[,5]

# Viewing specific row/observation contents
data[5,]

# Data in a range
data[2:3, 2:3]

############ Numerical exploration of continuous variables ###########
    
# summary statistics for continuous variables using the function summary()
mydata = data[,c(1:4,6)]
summary(mydata)

# summary statistics for continuous variables using the specific functions and apply()
mydata.mean=apply(mydata,MARGIN=2,FUN=mean)
mydata.mean

mydata.min=apply(mydata,MARGIN=2,FUN=min)
mydata.min

mydata.max=apply(mydata,MARGIN=2,FUN=max)
mydata.max

mydata.median=apply(mydata,MARGIN=2,FUN=median)
mydata.median

mydata.quantiles=apply(mydata,MARGIN=2,FUN=quantile)
mydata.quantiles

mydata.1st.quantile=apply(mydata,MARGIN=2,FUN=quantile)[2,]
mydata.1st.quantile

mydata.3rd.quantile=apply(mydata,MARGIN=2,FUN=quantile)[4,]
mydata.3rd.quantile

# summary statistics for the continuous variables using the specific functions and a for loop 
For.loop.min=NULL; For.loop.mean=NULL; For.loop.max=NULL; For.loop.median=NULL
For.loop.1st.quantile=NULL; For.loop.3rd.quantile=NULL

for (i in 1:(dim(mydata)[2]-1))
{
  For.loop.min[i]=min(mydata[,i])
  For.loop.mean[i]=mean(mydata[,i])
  For.loop.max[i]=max(mydata[,i])
  For.loop.median[i]=median(mydata[,i])
  For.loop.1st.quantile[i]=quantile(mydata[,i])[2]
  For.loop.3rd.quantile[i]=quantile(mydata[,i])[4]
}

For.loop.min
  mydata.min # for comparison
  
For.loop.mean
  mydata.mean # for comparison
  
For.loop.max
  mydata.max # for comparison
  
For.loop.median
  mydata.median # for comparison
  
For.loop.1st.quantile
  mydata.1st.quantile # for comparison
  
For.loop.3rd.quantile
  mydata.3rd.quantile # for comparison

############ Numerical exploration of factor variables ###########
  
#freq table for the factor variables
mydata2 = data[,c(5,7:12)]
(freq.table.sex = table(mydata2$sex)) 
(freq.table.smoke = table(mydata2$smoke))
(freq.table.pfplacen = table(mydata2$pfplacen)) 
(freq.table.parity = table(mydata2$parity)) 
(freq.table.workload = table(mydata2$workload)) 
(freq.table.matagegp = table(mydata2$matagegp)) 
(freq.table.gestcat = table(mydata2$gestcat)) 


# labeling the levels of the factor variables
levels(mydata2$sex) <- list(female=0, male=1)
levels(mydata2$smoke) <- list(nonsmoking=0, smoking=1)
levels(mydata2$smoke) <- list(nonsmoking=0, smoking=1)
levels(mydata2$matagegp) <- list("under 20"=1, "20-22"=2, "23-26"=3 , "Over 26"=4)

#################
# 2-Way Frequency Table
(mytable <- table(mydata2$sex,mydata2$smoke)) 

# tables of marginal frequencies 
margin.table(mytable, 1) # sex frequencies (summed over smoke)
margin.table(mytable, 2) # smoking status frequencies (summed over sex)

# tables of proportions
100*prop.table(mytable) # cell percentages
100*prop.table(mytable, 1) # row percentages
100*prop.table(mytable, 2) # column percentages

#testing the independence of the row and column variable
summary(mytable) # chi-square test of indepedence
chisq.test(mytable,correct = FALSE) # chi-square test of indepedence
chisq.test(mytable) # chi-square test of indepedence with Yates' continuity correction

###############
# 3-Way Frequency Table : method 1
(mytable <- table(mydata2$sex,mydata2$smoke,mydata2$matagegp)) 

# alternatively  : method 2
ftable(mytable) 

# 3-Way Frequency Table : method 3 (using xtabs)
mytable <- xtabs(~sex+smoke+matagegp, data=mydata2)
ftable(mytable) # print table

# log-linear models for 3-Way Frequency Table
library(MASS)
mytable <- xtabs(~sex+smoke+matagegp, data=mydata2)

# Mutual Independence: sex, smoking status and matagegp are pairwise independent
loglm(~sex+smoke+matagegp, mytable)

# Conditional Independence: sex is independent of smoking status, given matagegp. 
loglm(~sex+smoke+matagegp+sex*matagegp+smoke*matagegp, mytable)

# No Three-Way Interaction
loglm(~sex+smoke+matagegp+sex*smoke+sex*matagegp+smoke*matagegp, mytable)


############################
## Part 2: More exploration
###########################

#create a categorical data variable (safe_age) for "matage = 25 and above = safe" and "matage = below 25 = risky"
data$safe_age=NULL
data$safe_age[data$matage <25]="risky"
data$safe_age[data$matage >=25]="safe"
str(data)
data$safe_age=factor(data$safe_age,levels=c("risky","safe"))
str(data)

#Alternatively: creating the new categorical variable
data2=data
data2$safe_age2=NULL
for (i in 1:dim(data2)[1])
{
  if (data2$matage[i]<25)
  {data2$safe_age2[i]="risky"}
  if (data2$matage[i]>=25)
  {data2$safe_age2[i]="safe"}
}
str(data2)
data2$safe_age2=factor(data2$safe_age2, levels=c("risky","safe"))
str(data2)

######## 1.
#mean birth weight by safe_age using tapply
(mean_bw_by_safe_age=tapply(data$bweight,data$safe_age, FUN=mean))

#standard deviation of birth weight by safe_age using tapply
(mean_bw_by_safe_age=tapply(data$bweight,data$safe_age, FUN=sd))

#range of birth weight by safe_age using tapply
(mean_bw_by_safe_age=tapply(data$bweight,data$safe_age, FUN=range))

######## 2.
# mean of the birth weight and mother's height by safe_age, using apply()
(means_by_safe_age = apply(data[data$safe_age=="safe",c(3,6)],MARGIN=2,FUN=mean))
(means_by_risky_age = apply(data[data$safe_age=="risky",c(3,6)],MARGIN=2,FUN=mean))

# range of the birth weight and mother's height by safe_age, using apply()
(range_by_safe_age = apply(data[data$safe_age=="safe",c(3,6)],MARGIN=2,FUN=range))
(range_by_risky_age = apply(data[data$safe_age=="risky",c(3,6)],MARGIN=2,FUN=range))

(range_diff_by_safe_age = apply(apply(data[data$safe_age=="safe",c(3,6)],MARGIN=2,FUN=range),MARGIN=2,FUN=diff))
(range_diff_by_risky_age = apply(apply(data[data$safe_age=="risky",c(3,6)],MARGIN=2,FUN=range),MARGIN=2,FUN=diff))

# standard deviation of the birth weight and mother's height by safe_age, using apply()
(standard.dev_by_safe_age = apply(data[data$safe_age=="safe",c(3,6)],MARGIN=2,FUN=sd))
(standard.dev_by_risky_age = apply(data[data$safe_age=="risky",c(3,6)],MARGIN=2,FUN=sd))









