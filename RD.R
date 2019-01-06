rm(list=ls())

# Regression Diagnotics

# Assume that we are fitting a multiple linear regression
# on the MTCARS data
library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit) # leverage plots 


# Influential Observations
# added variable plots
avPlots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 


# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(fit)

# Ceres plots
ceresPlots(fit)


# Test for Autocorrelated Errors | 잔차의 독립성 검증  1.8 < x < 2.2 이하면 OK
durbinWatsonTest(fit)

# 오차의 정규성 검증 Shapiro Wilks test | P-value 가 0.05미만일 경우 accept
x <- residuals(fit)
shapiro.test(x)

# Evaluate homoscedasticity | 잔차/오차의 등분산성 검증 | P-value 가 0.05미만일 경우 accept
# non-constant error variance test
ncvTest(fit)


# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel) 
