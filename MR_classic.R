rm(list=ls())

library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 

# 1단계: 회귀분석의 오차/잔차의 기본 가정 검증

# Test for Autocorrelated Errors | 오차의 독립성 검증  1.8 < x < 2.2 이하면 OK
durbinWatsonTest(fit)

# 오차의 정규성 검증 Shapiro Wilks test | P-value 가 0.05미만일 경우 accept
x <- residuals(fit)
shapiro.test(x)

# Evaluate homoscedasticity | 오차의 등분산성 검증 | P-value 가 0.05미만일 경우 accept
# non-constant error variance test
ncvTest(fit)


# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
