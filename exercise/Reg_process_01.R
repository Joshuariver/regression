rm(list=ls())
setwd("~/R/Regression/exercise")

# Learning Data Science: Modelling Basics
# https://www.r-bloggers.com/learning-data-science-modelling-basics/

# 연령과 수입에 대한 상관관계 분석 프로세스
# 단순 연령과 수입에 대한 관계 플롯

age <- c(21, 46, 55, 35, 28)
income <- c(1850, 2500, 2560, 2230, 1800)
data <- data.frame(age, income)
plot(data, pch = 16)


# 피어슨의 상관관계 분석

cor(data) # correlation


# 회귀모델 구축
# building linear regression model

LinReg <- lm(income ~ age) # assign model to variable
abline(LinReg, col = "green", lwd = 2) # add regression line

LinReg # coefficients

summary(LinReg) # model summary


# 연령별 수입 예측
# predicting income with linear model

predict(LinReg, data.frame(age = 20))


pred_LinReg <- predict(LinReg, data.frame(age = seq(from = 0, to = 80, by = 5)))
names(pred_LinReg) <- seq(0, 80, 5)
pred_LinReg


# 수입과 연령의 관계는 단순 선형이 아니므로 변곡점을 55세로 줌.
# polynomial regression

PolyReg <- lm(income ~ poly(age, 4, raw = TRUE))
lines(c(20:55), predict(PolyReg, data.frame(age = c(20:55))), col = "red")


# 

pred_PolyReg <- predict(PolyReg, data.frame(age = seq(0, 80, 5)))
names(pred_PolyReg) <- seq(0, 80, 5)
pred_PolyReg


# mean income as a model
abline(h = mean(income), col = "blue")

title(main = "All models are wrong, some are useful")
legend("bottomright", c("Mean: Underfitting", "Linear fit", "Polynomial Reg.: Overfitting"), col = c("blue", "green", "red"), lwd = 3)

mean(income)

