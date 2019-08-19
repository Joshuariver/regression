# Regression Continuity Design in R

rm(list=ls())
setwd("~/R/Regression/exercise")

## RDD Model #1

# Loading packages in R
library(AER)
library(foreign)
library(rdd)
library(stargazer)

# Read Data and do data management in R
AEJfigs=read.dta("AEJfigs.dta")
All = all deaths
AEJfigs$age = AEJfigs$agecell - 21
AEJfigs$over21 = ifelse(AEJfigs$agecell >= 21,1,0)


# RDD model no. 1
reg.1=RDestimate(all~agecell,data=AEJfigs,cutpoint = 21)
plot(reg.1)
title(main="All Causes of Death", xlab="AGE",ylab="Mortality rate from all causes (per 100,000)")
summary(reg.1)

# RDD model no. 2
reg.2=RDestimate(mva~agecell,data=AEJfigs,cutpoint = 21)
plot(reg.2)
title(main="Motor Vehicle Accidents Death", xlab="AGE",ylab="Mortality rate from all causes (per 100,000)")
summary(reg.2)

# RDD model no. 2
reg.3=RDestimate(internal~agecell,data=AEJfigs,cutpoint = 21)
plot(reg.3)
title(main="Internal Causes of Death", xlab="AGE",ylab="Mortality rate from all causes (per 100,000)")
summary(reg.3)

