library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)

#for continuous variable , use mtcars dataset
data(mtcars)

# view dataset
head(mtcars)
summary(mtcars)
cor(mtcars)

# split the dataset into training and test data
set.seed(102)
index_to_split <- createDataPartition(mtcars$mpg, p = 0.7, list = FALSE)
train_data <- mtcars[index, ]
test_data <- mtcars[-index, ]

# linear regression model (predicts with one variable)
model_linear <- lm(mpg ~ wt, data = train_data)
summary(model)

# Multiple linear regression (using multiple variables)
model_multiple <- lm(mpg ~ wt + hp + cyl, data = train_data)
summary(model)

# Predictions on the test set
predictions_linear <- predict(model_linear, newdata = test_data)
predictions_multiple <- predict(model_multiple, newdata = test_data)

# Model Evaluation using Root Mean Square Error
rmse_linear <- sqrt(mean((predictions_linear - test_data $ mpg)^2))
cat("RMSE for Linear Regression:", rmse_linear)
rmse_multiple <- sqrt(mean((predictions_multiple - test_data $ mpg)^2))
cat("RMSE for Multiple Linear Regression:", rmse_multiple)

# visualising actual vs. predicted values for linear regression model
ggplot(data = data.frame(actual = test_data $ mpg,
                         predicted = predictions_linear),
       aes(x = actual, y = predicted)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")
