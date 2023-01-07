# Elasticnet regression analysis

library(MASS)
library(caret)
library(glmnet)

set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train,
                   method="glmnet",
                   trControl = trainControl(method = "cv",
                                            number = 10),
                   tuneLength=10)

Boston.cv$bestTune
Boston.gnet <- glmnet(x = x, y = y, family="gaussian", alpha = Boston.cv$bestTune$alpha, 
                      lambda = Boston.cv$bestTune$lambda)
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)

postResample(pred=Boston.pred, obs = Boston.test$medv)


# 비교 (Comparison)

lambda <- 10^seq(-5, 5, length = 100)
lambda

set.seed(123)
ridge <- train(form=medv ~ ., data=Boston.train,
                   method="glmnet",
                   trControl = trainControl(method = "cv",
                                            number = 10),
                   tuneGrid = expand.grid(alpha=0, lambda = lambda)
               )

coef(ridge$finalModel, ridge$bestTune$lambda)

ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)


set.seed(123)
lasso <- train(form=medv ~ ., data=Boston.train,
               method="glmnet",
               trControl = trainControl(method = "cv",
                                        number = 10),
               tuneGrid = expand.grid(alpha=1, lambda = lambda)
)

coef(lasso$finalModel, lasso$bestTune$lambda)

lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)



set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train,
               method="glmnet",
               trControl = trainControl(method = "cv",
                                        number = 10),
               tuneLength = 10)


coef(elastic$finalModel, elastic$bestTune$lambda)

elastic.pred <- predict(elastic, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)


models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(models)
summary(resamples(models))
summary(resamples(models), metric = "RMSE")
summary(diff(resamples(models), metric = "RMSE"))
