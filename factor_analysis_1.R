# 요인분석 연습

rm(list=ls())

library(haven)
library(foreign)
library(devtools)
# install_github("plgrmr/readAny", force = TRUE)
library(readAny)
# install.packages("psych")
library(psych)
library(GPArotation)

setwd("D:/RLab/Regression/Data/실습자료")

mydata <-  foreign::read.spss('1. 요인분석.sav', to.data.frame=TRUE, use.value.labels=FALSE, 
                   use.missings=TRUE, reencode='UTF-8')

str(mydata)

#Remove rows with missing values and keep only complete cases
mydata=mydata[complete.cases(mydata),]
names(mydata)
describe(mydata)


#Create the correlation matrix from mydata
mydata_cor <- cor(mydata)

#Factor analysis of the data

bt <- bartlett.test(mydata)
bt
# bartlett test 구형성 검증 (변수들간의 상관이 0인지를 검증함: 유의확율이 작을수록 (p<.05)요인
# 분석하기 적합함을 의미)

KMO(mydata)
# KMO-(Kriser Meyer Olkin)-KMO 값이 1에 가까울수록 요인분석 하기에 적합함을 의미
# 기준이 0.5 이상: 0.5 미만의 경우 요인분석 자체를 문제 삼지 않을 가능성
# 개별 변수에 대한 표본의 적절성 검증(MSA-Measure of Sampling Adeguacy): 역이미지 행렬에
# 제시되는 값으로 클수록 요인분헉하기에 적합함을 의미: 기준 .50이상
# (.5보다 작은 항목은 이 변수를 제거하고 다시 분석, 2개 이상이면 가장 작은 것 부터 제거)

mydata.factor <- principal(mydata, rotate="none")
names(mydata.factor)
mydata.factor$values
plot(mydata.factor$values, type = 'b')


mydata.factor$communality
# 공통성(Communality): 추출된 요인들에 의해 설명되는 변수의 분산
# 각 항목의 값이 공통요인을 설명하는 데 있어 x%의 설명력이 있다는 의미
# 공통성의 값이 .5보다 작을 경우 일반적으로 해당 항목을 제거하고 다시 분석 시도


psych::scree(mydata)
psych::fa.parallel(mydata)             

options(digits=3)
mydata.Varimax = principal(mydata, nfactors = 3, rotate="varimax")
print(mydata.Varimax,cut=0,digits=3)
plot(mydata.Varimax)
fa.diagram(mydata.Varimax)

# Promax 를 사용하여 다른 방식으로 요인분석을 한 사

fac1 <- factanal(mydata, 3, rotation = "promax")
print(fac1, cutoff = .30)
fl <- round(unclass(fac1$loadings), 2)
fl


factors_data <- fa(r = mydata_cor, nfactors = 3,n.obs = 213,fm="wls")

factors_data <- fa(mydata, nfactors = 3,n.obs = 213,fm="wls")

# Getting the factor loadings and model analysis

print(factors_data,cut=0,digits=3)
plot(factors_data)
fa.diagram(factors_data)


# factor analysis using Promax rotation

p3p <-principal(mydata,3,n.obs = 213,rotate="Promax")
print(p3p,cut=0,digits=3)
plot(p3p)
fa.diagram(p3p)


# A higher order factor solution: Omega

om.h <- omega(mydata,n.obs=213,sl=FALSE)
op <- par(mfrow=c(1,1))

# A bifactor factor solution 

om <- omega(mydata,n.obs=213)

# Item Cluster Analysis: iclust : Alternative clustering then Factor Analysis

ic <- iclust(mydata)

