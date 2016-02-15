## Numerical data exploration using R - Practical session solutions 
## By: Waithaka Michael
# Feb 2016

setwd("/Users/mwaithaka/Documents/Professional/1. KEMRI /4. Sundry/Training/2016_Feb_Pwani R training")

################## QN 1: READ IN THE DATA
# Read in the csv dataset named maltreat
data <- read.csv('data/maltreat.csv')
str(data)
################## QN 2: EXPLORING THE DATA
# a. How many rows and columns does the dataset have?
dim(data)

# b. List the variable names in the dataset
names(data) 

# c. List the first 6 rows of the dataset
head(data) 

# d. List the last 6 rows of the dataset
tail(data) 

# e. Extract the data in row 23 column 6
data[23,6]  

# f. Extract the data in column 5
data[,5]

# g. Extract the data in row 306
data[306,]

# h. Extract all the data that lie within  row 12 to 14 and column 2 to 5
data[12:14,2:5]

################## QN 2: SUMMARIZING THE DATA
# Compute the summary statistics of all the continous variables 
# continuous variables - weight temp pcv falcipasex resprate heartrate enrolled age falcsex lnfalsx  
cont_data =na.omit((data[,c(8,9,12:15,17:19)]))

# a. using the function summary()
summary(cont_data)

# b. using the specific functions
mean=apply(cont_data,MARGIN=2,FUN=mean); mean
min=apply(cont_data,MARGIN=2,FUN=min); min
max=apply(cont_data,MARGIN=2,FUN=max); max
median=apply(cont_data,MARGIN=2,FUN=median); median
quantiles=apply(cont_data,MARGIN=2,FUN=quantile); quantiles
f1st.quantile=apply(cont_data,MARGIN=2,FUN=quantile)[2,]; f1st.quantile
t3rd.quantile=apply(cont_data,MARGIN=2,FUN=quantile)[4,]; t3rd.quantile

# Tabulate the categorical variables by ethnic, compute the cell percentages and test for any association between the variables
# categorical variables - enrolled ethnic sex arrival vomited bloodfilm filtertaken
cat_data = na.omit(data[,c("enrolled", "ethnic", "sex", "arrival", "vomited", "bloodfilm", "filtertaken")])

# labeling the levels of the factor variables
levels(cat_data$enrolled)
levels(cat_data$enrolled)<- list("No"="No", "Yes"="Yes")

levels(cat_data$ethnic)
levels(cat_data$ethnic)<- list("Fula"="Fula", "Jola"="Jla", "Jola"="Jola", "Madinka"="Madinka",
                               "Madinka"="Mandinka", "Madinka"="Mandnka","Other"="Other", 
                               "Serahouli"="Serahouli","Serahouli"="Serahule","Wollof"="Wolllof",
                               "Wollof"="Wollof","Wollof"="Wolof"   )
levels(cat_data$sex)
levels(cat_data$sex)<- list("Female"="Female", "Male"="Male")

levels(cat_data$arrival)
levels(cat_data$arrival)<- list("Bicycle"="Bicycle", "Horse/donkey cart"="Horse/donkey cart","Other"="Other","Taxi"="Taxi","Walking"="Walking")

levels(cat_data$vomited)
levels(cat_data$vomited)<- list("No"="No", "Yes"="Yes")

levels(cat_data$bloodfilm)
levels(cat_data$bloodfilm)<- list("No"="", "Yes"="Yes")

levels(cat_data$filtertaken)
levels(cat_data$filtertaken)<- list("No"="", "Yes"="Yes")

# 2-Way Frequency Table
(mytable1 <- table(cat_data$ethnic,cat_data$sex)) 
(mytable2 <- table(cat_data$ethnic,cat_data$arrival))
(mytable3 <- table(cat_data$ethnic,cat_data$vomited))
(mytable4 <- table(cat_data$ethnic,cat_data$bloodfilm))
(mytable5 <- table(cat_data$ethnic,cat_data$filtertaken))
(mytable6 <- table(cat_data$ethnic,cat_data$enrolled)) 

# tables of proportions
100*prop.table(mytable1) # cell percentages
100*prop.table(mytable2)
100*prop.table(mytable3)
100*prop.table(mytable4)
100*prop.table(mytable5)
100*prop.table(mytable6)

#testing the independence of the row and column variable
summary(mytable1) # chi-square test of indepedence
(test<-fisher.test(mytable1))
summary(mytable2) # chi-square test of indepedence
(test<-fisher.test(mytable2))
summary(mytable3) # chi-square test of indepedence
(test<-fisher.test(mytable3))
summary(mytable4) # chi-square test of indepedence
(test<-fisher.test(mytable4))
summary(mytable5) # chi-square test of indepedence
(test<-fisher.test(mytable5))
summary(mytable6) # chi-square test of indepedence
(test<-fisher.test(mytable6))

# Tabulate the categorical variables by ethnic and sex and then test for any pairwise association
library(MASS)
mytable1 <- xtabs(~sex+ethnic+arrival, data=cat_data)
ftable(mytable1) # print table

mytable2 <- xtabs(~sex+ethnic+vomited, data=cat_data)
ftable(mytable2) # print table

mytable3 <- xtabs(~sex+ethnic+bloodfilm, data=cat_data)
ftable(mytable3) # print table

mytable4 <- xtabs(~sex+ethnic+filtertaken, data=cat_data)
ftable(mytable4) # print table

mytable5 <- xtabs(~sex+ethnic+enrolled, data=cat_data)
ftable(mytable5) # print table

# Mutual Independence: testing for pairwise independence
loglm(~sex+ethnic+arrival, mytable1)
loglm(~sex+ethnic+vomited, mytable2)
loglm(~sex+ethnic+bloodfilm, mytable3)
loglm(~sex+ethnic+filtertaken, mytable4)
loglm(~sex+ethnic+enrolled, mytable5)

################## QN 3: SUMMARIZING THE DATA
# using the bwmal data:
#create a categorical variable pregnancy_beyond_20_weeks
# For a woman who has never carried a pregnancy beyond 20 weeks is nulliparous (parity=0), 
# A woman who has given birth once before is primiparous (parity=1), 
# A woman who has given birth two or more times is multiparous (parity>=2)
# A grand multipara is a woman who has given birth three or more times (parity>=3).
# Compute summary statistics for bweight matage by the new categorical variable 
# Read in the csv dataset named maltreat
data <- read.csv('data/bwmal.csv')
str(data)

names <- c('sex' ,'smoke' ,'pfplacen','workload' ,'matagegp' ,'gestcat')
data[,names] <- lapply(data[,names] , factor)
str(data)

data$pregnancy_beyond_20_weeks=NULL
for(i in 1:dim(data)[1])
{
  if(data$parity[i]==0)
  {data$pregnancy_beyond_20_weeks[i]="nulliparous"}
  if(data$parity[i]==1)
  {data$pregnancy_beyond_20_weeks[i]="primiparous"}
  if(data$parity[i]==2)
  {data$pregnancy_beyond_20_weeks[i]="multiparous"}
  if(data$parity[i]>2)
  {data$pregnancy_beyond_20_weeks[i]="grand multipara"}
}
head(data)

data$pregnancy_beyond_20_weeks=factor(data$pregnancy_beyond_20_weeks, levels=c("nulliparous","primiparous","multiparous","grand multipara"))
str(data)

# mean of the birth weight and mother's age by parity, using apply()
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="nulliparous",c(2,6)],MARGIN=2,FUN=mean))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="primiparous",c(2,6)],MARGIN=2,FUN=mean))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="multiparous",c(2,6)],MARGIN=2,FUN=mean))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="grand multipara",c(2,6)],MARGIN=2,FUN=mean))



# range of the birth weight and mother's age by parity, using apply()
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="nulliparous",c(2,6)],MARGIN=2,FUN=range))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="primiparous",c(2,6)],MARGIN=2,FUN=range))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="multiparous",c(2,6)],MARGIN=2,FUN=range))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="grand multipara",c(2,6)],MARGIN=2,FUN=range))

# standard deviation of the birth weight and mother's age by parity, using apply()
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="nulliparous",c(2,6)],MARGIN=2,FUN=sd))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="primiparous",c(2,6)],MARGIN=2,FUN=sd))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="multiparous",c(2,6)],MARGIN=2,FUN=sd))
(means_by_parity = apply(data[data$pregnancy_beyond_20_weeks=="grand multipara",c(2,6)],MARGIN=2,FUN=sd))
