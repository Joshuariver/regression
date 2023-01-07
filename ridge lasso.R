# Ridge & Lasso & Elastic Regression
# 패널티 회귀분석(Panalized regression analysis) 라고도 함

# 지나치게 많은 독립변수를 갖는 모델에 페널티를 부과하는 방식으로 간명한 회귀모델을 생성
# 모델의 성능에 크게 기여하지 못하는 변수의 영향력을 축소하거나 모델에서 제거
# 최소자승법에 의한 잔차(관측값-예측값)의 제곱합과 페널티항의 합이 최소가 되는 회귀계수를 추정



rm(list=ls())

library(MASS)
str(Boston)

library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv, p=0.7, list = FALSE)
head(train)

Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

nrow(Boston.train)
nrow(Boston.test)

library(glmnet)

x <- model.matrix(medv ~ ., Boston.train)
head(x)
x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv


# Ridge Regression Analysis

Boston.cv <- cv.glmnet(x=x, y=y, family = "gaussian", alpha = 0)
plot(Boston.cv)
str(Boston.cv)

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

Boston.gnet <- glmnet(x = x, y = y, family="gaussian", alpha = 0, lambda = Boston.cv$lambda.min)

coef(Boston.gnet)


Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
head(Boston.pred)


# 성능평가지표 MSE RMSE MAE 구하기

postResample(pred=Boston.pred, obs = Boston.test$medv)



# Lasso Regression Analysis

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family = "gaussian", alpha = 1)

plot(Boston.cv)

Boston.cv$lambda.min
log(Boston.cv$lambda.min)


Boston.cv$lambda.1se
log(Boston.cv$lambda.1se)

coef(Boston.cv, Boston.cv$lambda.min)
coef(Boston.cv, Boston.cv$lambda.1se)


Boston.gnet1 <- glmnet(x = x, y = y, family="gaussian", alpha = 1, lambda = Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x)

Boston.gnet2 <- glmnet(x = x, y = y, family="gaussian", alpha = 1, lambda = Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x)

# 성능평가지표 MSE RMSE MAE 구하기

postResample(pred=Boston.pred1, obs = Boston.test$medv)
postResample(pred=Boston.pred2, obs = Boston.test$medv)
