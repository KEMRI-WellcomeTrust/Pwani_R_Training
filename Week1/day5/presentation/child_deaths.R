#setwd("~/pwani_stats")
setwd("/MYDATA/Reports/Pwani Stats/pwani2016/scripts_data")
#setwd("/MYDATA/Reports/KWTRP_Intro_To_Stats/scripts_data")


# CALCULATING RATES 


# 1.0 Load packages
library(foreign)
library(epicalc)
library(epitools)
library(epiR)

# 2.0 Load the child deaths data
childdeaths <- read.table("child.deaths.csv", header=TRUE, sep=",")

use(childdeaths)
des()
codebook()

# 2.1 label the values of the variable "status"
childdeaths$status<-as.numeric(childdeaths$status)
childdeaths$status <- factor(childdeaths$status,
                             levels = c(1,2),
                             labels = c("alive", "dead"))
table(childdeaths$status)

# 3.0 generate person-years
childdeaths$date_exit <- as.Date(childdeaths$date_exit, "%m/%d/%y")
childdeaths$date_birth <- as.Date(childdeaths$date_birth, "%m/%d/%y")

person_yrs<-difftime(childdeaths$date_exit,childdeaths$date_birth)
person_yrs<-(as.numeric(person_yrs))/365.25
label.var(person_yrs, "person years observed")
childdeaths<-data.frame(childdeaths,person_yrs)

# 4.0 Calculate rates
total_deaths<-length(childdeaths$status[childdeaths$status=="dead"])
total_deaths

total_pyrs<-sum(childdeaths$person_yrs)
total_pyrs
mort_rate<-(as.numeric(total_deaths)/total_pyrs)
mort_rate

# 5.0 calculate standard error of the rate
s.e.Rate<-sqrt(total_deaths)/total_pyrs*1000 # 
s.e.Rate
s.e.log.Rate<-1/sqrt(total_deaths)
s.e.log.Rate

# 6.0 Compute 95% CI for rate
log.rate=log(mort_rate)
lower.bound=exp(log.rate-1.96*s.e.log.Rate)
lower.bound
upper.bound=exp(log.rate+1.96*s.e.log.Rate)
upper.bound



# COMPARING TWO RATES


# 7.0 Rate difference: Males vs. Females

# 7.1 Calculate number of deaths for each gender: male and female
f.deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$sex=="f"])
f.deaths

m.deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$sex=="m"])
m.deaths

# 7.2  calculate person years for each gender
f.total_pyrs<-sum(childdeaths$person_yrs[childdeaths$sex=="f"])
f.total_pyrs

m.total_pyrs<-sum(childdeaths$person_yrs[childdeaths$sex=="m"])
m.total_pyrs

table(childdeaths$status,childdeaths$sex)

# 7.3 calculate rate difference
diff1<-(f.deaths/f.total_pyrs)-(m.deaths/m.total_pyrs)
diff1

library(fmsb)
gender_diff<-ratedifference(f.deaths,m.deaths,f.total_pyrs,m.total_pyrs,conf.level=0.95)
gender_diff


# 8.0 Rate Ratio: Home vs. Hospital delivery

# 8.1 Calculate number of deaths for each gender: male and female

home_deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$locn_birth=="Home"])
hosp_deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$locn_birth=="Hospital"])
table(childdeaths$status,childdeaths$locn_birth)

# 8.2  calculate person years for both babies delivered in hospital and at home
home_pyrs<-sum(childdeaths$person_yrs[childdeaths$locn_birth=="Home"])
hosp_pyrs<-sum(childdeaths$person_yrs[childdeaths$locn_birth=="Hospital"])
home_vs_hosp<-ratetable(home_deaths,hosp_deaths,home_pyrs,hosp_pyrs)
dimnames(home_vs_hosp)<-list(Exposure=c("Home","Hospital"), Outcome=c("Deaths","PYears"))
home_vs_hosp

# 8.3  calculate the rate ratio
rateratio.wald(home_vs_hosp,rev="r")

#8.4 Rate Ratio: mother's education (some education Vs. no education)
no_educ_deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$mom_educ=="no education"])
some_educ_deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$mom_educ=="some education"])
table(childdeaths$status,childdeaths$mom_educ)

no_educ_pyrs<-sum(childdeaths$person_yrs[childdeaths$mom_educ=="no education"])
some_educ_pyrs<-sum(childdeaths$person_yrs[childdeaths$mom_educ=="some education"])
some_educ_Vs_no_educ<-ratetable(no_educ_deaths,some_educ_deaths,no_educ_pyrs,some_educ_pyrs)
dimnames(some_educ_Vs_no_educ)<-list(Exposure=c("no education","some education"), Outcome=c("Deaths","PYears"))
some_educ_Vs_no_educ

rateratio.wald(some_educ_Vs_no_educ,rev="r")



