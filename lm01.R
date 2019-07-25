getwd() # where am I?

list.files("dataSets") # files in the dataSets folder

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

# Liner Regression (단순회귀)

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)

# correlation between expense and csat
cor(sts.ex.sat) 

# scatter plot of expense vs csat
plot(sts.ex.sat)

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

# From the first simple liner regression, it says SAT and individual expense are negatively related.
# It can be surprising fact.  Now we need to add percentage factor to the regression model

summary(lm(csat ~ expense + percent, data = states.data))

# So it says the reality is that SAT score and per capita expense are positively related. But the
# percentage of SAT test attempt for high scorer is less. Therefore more SAT test attempt needs
# more money to invest. And they are low graders. That'ss why SAT score and per CAPITA expense
# are negatively related.


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



## Comparing models

## Do congressional voting patterns predict SAT scores over and above expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)

coef(summary(sat.voting.mod))

# Modeling Interaction:  상호작용효과 확

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

