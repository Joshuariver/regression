# 광고비와 월별 매출액간의 인과관계 회귀분석

rm(list=ls())
setwd("D:/RLab/Regression/Data/실습자료")

# library(haven)
library(foreign)
# library(devtools)
# install_github("plgrmr/readAny", force = TRUE)
# library(readAny)
# install.packages("psych")
library(psych)
library(GPArotation)

setwd("D:/RLab/Regression/Data/실습자료")

# mydata <-  foreign::read.spss('3. 단순회귀분석.sav', to.data.frame=TRUE, use.value.labels=FALSE, 
#                              use.missings=TRUE, reencode='UTF-8')
mydata <-  read.spss('3. 단순회귀분석.sav', to.data.frame=TRUE, use.value.labels=FALSE, use.missings=TRUE)

names(mydata)
dim(mydata)

sts.mydata <- subset(mydata, select = c("매출", "광고비"))
summary(sts.mydata)

# 매출액과 광고비와의 상관분석
cor(sts.mydata) 

# 회귀분석 적절성 확인
# 매출과 광고비와의 관계에 대한 scatter plot
plot(sts.mydata)


# 선형회귀분석 실시
sat.mod <- lm(매출 ~ 광고비, # regression formula
                data=mydata) # data set

# Now to the liner regression step by step

class(sat.mod)

names(sat.mod)

methods(class = class(sat.mod))[1:9]

confint(sat.mod)
# hist(residuals(sat.mod))  # 잔차의 정규성 검증


# OLS(Ordinary least squares regression: 최소자승법) 선형회귀분석에는 여러가지 가설이 전제되어 있음. 
# 1. 독립변수와 종속변수의 선형적 관계
# 2. 오(잔)차의 정규성 (the residuals (errors) are normally distributed)
# 3. 오(잔)차의 등분산성 (the residuals (errors) are homoscedastic)
# 4. 오(잔)차의 독립성이 그것이다.(the residuals (errors) are independent and relationships are linear)
# 이러한 가설을 도표로 확인해 보는 단계

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional


# 모형 요약
coe.mod.r.square <- summary(sat.mod)$r.squared
coe.mod.adj.r.square <- summary(sat.mod)$adj.r.squared
coe.mod <- cbind(coe.mod.r.square, coe.mod.adj.r.square)
coe.mod

# ANOVA 분석을 통한 회귀식 자체가 적절한지 파악.
# F 값이 충분히 크고, P<.05 일 경우 회귀식이 통계적으로 유의하다고 판단할 수 있음.
options(digits=3)
anova(sat.mod)

# 회귀분석 결과를 요약 출력
summary(sat.mod) # show regression coefficients table
