
######## Tutorial on Implementing Decision Tree in R
# https://www.datacamp.com/community/tutorials/decision-trees-R


# install.packages(#'ISLR', 'tidyverse',
#                  'dplyr')

library(ISLR)
library(dplyr)
library(rpart)


data(package = "ISLR")

carseats <- Carseats

#require(tree), tree package not working 8/16/2019

## Take a look at Histogram of Carsales

hist(carseats$Sales)

#### Data Structure Check:

any(is.na(carseats))
# FALSE , no NA

## Transform the Sales Variable into High Sales or Not
names(carseats)
str(carseats)

carseats$High <- ifelse(carseats$Sales <= 8, "No", "Yes" )

## Remove the sales Variables as it would COnfound to the Dependent Variable

carseats <- carseats %>% select(-Sales)

names(carseats)
dim(carseats)
400*.70

#### Divide Train and Test

# 250 for train,
# 150 for test

set.seed(101)

train = sample(1:nrow(carseats), 250)

par(xpd = NA)
model_tree1 <- rpart(High ~., data = carseats[train,])

plot(model_tree1)
text(model_tree1, digits = 3)

# print the rules
print(model_tree1, digits = 3)

### Use tree to predict n test data set

pred_classes = model_tree1 %>%
  predict(carseats[-train,], type = "class")

## Accuracy:
mean(pred_classes == carseats[-train,]$High)

## .76 Accuracy

########## Pruning the tree
## Stppong the tree growth using some criteria
## cp (complexity Parameter) the higher cp -> Smaller Tree
## Lower cp -> Overfitting
## Use Cross-Validation, to obtain optimal cp value
## Best CP: maximize the CV Accuracy

library(caret)
library(e1071)

dim(carseats)

car_train <- carseats[train,]
car_test <- carseats[-train,]

set.seed(123)

model2 <- caret::train(
  High ~. , data = car_train, method = "rpart",
  trControl = trainControl("cv", number = 10), # Set-up 10-fold CV
  tuneLength = 10 #Specify number of possible cp values to evaluate
)

## plot complexity Parameter

plot(model2)

model2$bestTune

# 0.2416397

### Set the cp parameter to besttune

par(xpd = NA)
plot(model2$finalModel)
text(model2$finalModel, digits = 3)

model2$finalModel

## Pred Classes
pred_classes2 <- model2 %>% predict(car_test)


## Accuracy:
mean(pred_classes2 == car_test$High)

## .72 Accuracy comparable to .76 using Fully Grown Tree














