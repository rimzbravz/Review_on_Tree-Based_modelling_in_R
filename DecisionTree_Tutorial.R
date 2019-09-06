
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
  # We can change to "prob" if we want to output the probability instead of the Class


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


set.seed(101)

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

########## Let's Play with rpart.control

### Redo the Prediction:
### 

set.seed(101)

train = sample(1:nrow(carseats), 250)

car_train <- carseats[train,]
car_test <- carseats[-train,]


par(xpd = NA)

# Tree Model 1: Base

tree1 <- rpart(High ~., data = car_train)

plot(tree1)
text(tree1, digits = 3)

# print the rules
print(tree1, digits = 3)

### Use tree to predict n test data set

car_test$High <- as.factor(car_test$High)


Pred1 <- predict(tree1, car_test, type = "class")

## Base Model: 

BaseModel <- confusionMatrix(Pred1,car_test$High) # Should be a factor


# Let's create Model 2

ctr = rpart.control(
  minsplit = 15,
  minbucket = 20,
  maxdepth = 5
  )

tree2 <- rpart(High ~., data = car_train, control = ctr)

plot(tree2)
text(tree2, digits = 3)

# print the rules
print(tree2, digits = 3)

### Use tree to predict n test data set

car_test$High <- as.factor(car_test$High)


Pred2 <- predict(tree2, car_test, type = "class")

## Base Model: 

Model2 <- confusionMatrix(Pred2,car_test$High) # Should be a factor


printcp(tree1)
printcp(tree2)

plotcp(tree1)
plotcp(tree2)


## Compare Models:

BaseModel
Model2

# Model 3: uSing A chaid Base:
library(CHAID)

str(car_train)

## Note in CHAID, Variables need to be factors

## our data only has 3 Factors,
# Let's se if we can bin the numerics

library(tidyverse)
library(ggplot2)

# Visualize Car Seats data and Let's create a new Dataset with binned Numerics

carseats %>%
  select_if(is.numeric) %>%
  gather(metric,value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

CHAID <- chaid(High ~., data = car_train)


carseats_N <- carseats
dim(carseats_N) # 400 Sample Size

## Bin the numerics:
# For the Purpose of this example,
# Let's try to cut the bins by Equal size - Insufficient Obs to cut ths way
# Let's try to cut the bins by Equal size - Insufficient Obs to cut ths way


carseats_N <- carseats_N %>%
  mutate_if(is.numeric,
            funs(cut_interval(.,n = 5)))

names(carseats_N)
unique(carseats_N$CompPrice)
unique(carseats_N$High)

set.seed(101)

train = sample(1:nrow(carseats_N), 250)

# All SHould be Made as factors
carseats_N$High <- as.factor(carseats_N$High)
str(carseats_N)

# Train Test Split
car_train <- carseats_N[train,]
car_test <- carseats_N[-train,]

# Model Build
chaid1 <- chaid(High ~ ., data = car_train)

print(chaid1)

plot(
  chaid1,
  main = "Chaid Base with Binned",
  gp = gpar(
    #col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)

Pred_Chaid1 <- predict(chaid1, car_test, type = "response")

Model_Chaid1 <- confusionMatrix(Pred_Chaid1,car_test$High)
Model_Chaid1

# Model 4: CHAID  with Control  

ctrl <- chaid_control(
  alpha2 = .01,
  alpha4 = .015
)

# Model Build
chaid2 <- chaid(High ~ ., data = car_train, control = ctrl)

plot(
  chaid2,
  main = "Chaid Modified Controls",
  gp = gpar(
    #col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)

Pred_Chaid2 <- predict(chaid2, car_test, type = "response")

Model_Chaid2 <- confusionMatrix(Pred_Chaid2,car_test$High)
Model_Chaid2  

  
# Model 5: RandomForest  
# https://www.r-bloggers.com/how-to-implement-random-forests-in-r/  
  
library(randomForest)

set.seed(101)

str(carseats)

carseats$High <- as.factor(carseats$High)

train = sample(1:nrow(carseats), 250)

car_train <- carseats[train,]
car_test <- carseats[-train,]


# Train the Random Forest:

RF1 <- randomForest(High ~ .,
                    data = car_train,
                    importance = TRUE
                    )

RF1

Pred_RF1 <- predict(RF1, car_test, type = "class")

Model_Pred_RF1 <- confusionMatrix(Pred_RF1,car_test$High)
Model_Pred_RF1

# Check Variable Importance:
importance(RF1)
varImpPlot(RF1)

# Model 6 : Random Forest Tuned

RF2 <- randomForest(High ~ .,
                    data = car_train,
                    importance = TRUE,
                    mtry = 2,
                    ntree = 4000
                    )

RF2

# Check Variable Importance:
importance(RF2)
varImpPlot(RF2)

Pred_RF2 <- predict(RF2, car_test, type = "class")

Model_Pred_RF2 <- confusionMatrix(Pred_RF2,car_test$High)
Model_Pred_RF2

## Dtermine the Best mtry with 



#Let's compare all our models

ModelConfMatrix <- list(
  "Base Tree" = BaseModel,
  "Modified Tree" = Model2,
  "Chaid1_Default" = Model_Chaid1,
  "CHad2_Tuned" = Model_Chaid2,
  "RandomForest_Default" = Model_Pred_RF1,
  "RadomForest_Tuned" = Model_Pred_RF2)

# ModelList <- list(
#   "Base Tree" = tree1,
#   "Chaid1_Default" = chaid1,
#   "CHad2_Tuned" = chaid2,
#   "RandomForest_Default" = RF1,
#   "RadomForest_Tuned" = RF2)


ModelResults <- 
  ModelConfMatrix %>%
  map_dfr(~ cbind(as.data.frame(t(.x$overall)),as.data.frame(t(.x$byClass))), .id = "ModelNumb")


names(ModelResults)


# <Continue Learning>

## Further Resources:
# https://www.r-bloggers.com/chaid-and-caret-a-good-combo-june-6-2018/
# https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
    