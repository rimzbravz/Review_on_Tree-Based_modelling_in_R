###### Tutorial on Regression Trees
# http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/


library(MASS)

#' Problem : Predict Median House Value
# Target Variable is a continous variable, this is a Regression Problem
# Data Set: Boston (MASS)

data('Boston', package = "MASS")

# Inspect the dataset:

names(Boston)
any(is.na(Boston))
dim(Boston)
str(Boston)

# Train Test Split

set.seed(123)

train_samp <- Boston$medv %>% createDataPartition(p = 0.8, list = FALSE)

train_data <- Boston[train_samp,]
test_data <- Boston[-train_samp,]


# CReate the regression tree
  
  # the best cp value is the one that minimizes the RMSE (Root Mean Square Error)
  # RMSE = sqrt(sigma((ysubi - MUsubi)^2))

model <- caret::train(
  medv ~ ., data = train_data, method = "rpart",
  trControl = trainControl("cv", number = 10), # Set-up 10-fold CV
  tuneLength = 10 #Specify number of possible cp values to evaluate
  
)

plot(model)
model$bestTune


## Plot the final Tree Model
par(xpd = NA)
plot(model$finalModel)
text(model$finalModel, digits = 3)

## Export the rules:
print(model$finalModel)
str(model$finalModel)

## Make predictions on test Data set:

predictions <- model %>% predict(test_data)
head(predictions)

## Compute the prediction error : RMSE
RMSE(predictions, test_data$medv)

#4???














