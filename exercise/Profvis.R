rm(list=ls())
setwd("~/R/Regression/exercise")


# Import the restaurant inspection dataset
df.rst.insp <- read.csv("Restaurant_Inspections_-_All_Inspections.csv", header = TRUE)

# A count of rows in the dataset
num.rows <- nrow(df.rst.insp)

# Create a shuffled subset of rows
subset.sample <- sample(num.rows, floor(num.rows*.75))

# Create a training dataset using a shuffled subset of rows
df.training <- df.rst.insp[subset.sample,]

# Create a test dataset of all rows NOT in the training dataset
df.test <- df.rst.insp[-subset.sample,]

# Create the generalized linear model using the training data frame
mdlCompliance <- glm(In.Compliance ~ Core.Violations + Priority.Violations + Foundation.Violations, family = binomial, data = df.training)

# Predict the compliance of the test dataset
results <- predict(mdlCompliance, newdata=df.test, type = "response")

# Turn the response predictions into a binary yes or no
results <- ifelse(results < 0.5, "No", "Yes")

# Add the results as a new column to the data frame with the actual results
df.test$results <- results

# Output the confusion matrix
table(df.test$In.Compliance, df.test$results)

# Output the confusion matrix
library(caret)
confMat <- table(df.test$In.Compliance, df.test$results)
confusionMatrix(confMat, positive = "Yes")



# install.packages("profvis")
library("profvis")

profvis({
  
  df.rst.insp <- read.csv("Restaurant_Inspections_-_All_Inspections.csv", header = TRUE)
  num.rows <- nrow(df.rst.insp)
  subset.sample <- sample(num.rows, floor(num.rows*.75))
  df.training <- df.rst.insp[subset.sample,]
  df.test <- df.rst.insp[-subset.sample,]
  mdlCompliance <- glm(In.Compliance ~ Core.Violations + Priority.Violations + Foundation.Violations, family = binomial, data = df.training)
  results <- predict(mdlCompliance, newdata=df.test, type = "response")
  results <- ifelse(results < 0.5, "No", "Yes")
  df.test$results <- results
  confMat <- table(df.test$In.Compliance, df.test$results)
  confusionMatrix(confMat, positive = "Yes")
  
})
