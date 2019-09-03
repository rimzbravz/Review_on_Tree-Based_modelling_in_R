##### Tutorial on CHAID Trees
#https://www.r-bloggers.com/chaid-and-r-when-you-need-explanation-may-15-2018/


### Chi-square automatic interaction Detection

## Problem: Predicting Attrition

## Package Installation:

#install.packages("rsample")
install.packages("CHAID", repos="http://R-Forge.R-project.org")

require(rsample)
require(dplyr)
require(ggplot2)

theme_set(theme_bw()) # Set Theme

#install.packages('CHAID')
require(CHAID)
require(purrr)
require(caret)


### Load the Data

data("attrition")

str(attrition)
names(attrition)


### Chaid requires Nominal or Ordinal
## i.e we need to convert them as factor or ordered Factor

###### Check coverage
attrition %>% select_if(is.factor) %>% ncol
attrition %>% select_if(is.numeric) %>% ncol

#of the numeric, how many of them have small # of levels

### We need to check if we can still use some of those integers as factor
### Porbably we can also Bin them

##
attrition %>%
  select_if(function(col) 
            length(unique(col)) <= 5 & is.integer(col))%>% # This Function will Output True or False
  head # Cool!, you can do this


attrition %>%
  select_if(function(col) 
    length(unique(col)) <= 10 & is.integer(col))%>% # This Function will Output True or False
  head # Cool!, you can do this


## AFter Investogation we can see it is safe to convert this into factor

##### Version 1 but we can improve this ()
attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    NumCompaniesWorked = factor(NumCompaniesWorked),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
    
  )

###

attrition <- attrition %>%
  mutate_if(
    function(col) 
      length(unique(col)) <= 10 & is.integer(col),as.factor)

## Check the transformation
str(attrition) ## They are now factors

##As you look at the results this is a good time to remind you that
#CHAID is "non parametric" which means that we don't have to worry
#about how the distribution (normality) looks nor make any assumptions
#about the variance.

dim(attrition)

## Now we select only the Variables that are factors

newattrit <- attrition %>%
  select_if(is.factor)

## Demo without chaid_Control()
chaid_model <- chaid(Attrition ~ ., newattrit)
print(chaid_model)
plot(chaid_model)


## Test interaction on Overtime
chisq.test(newattrit$Attrition,newattrit$OverTime)

par(mfrow = c(1,2), xpd = NA)
plot(chaid_model, uniform = TRUE, type = "simple")

##
plot(
  chaid_model,
  main = "Testing Graphical Options",
  gp = gpar(fontsize = 6),
  type = "simple"
)
  
## ANother Plot Option:

plot(
  chaid_model,
  main = "Testing Graphical Options",
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3,
    fontsize = 6)
)    
    
### Now Let's try using the control Parameter

#minsplit - Number of observations in splitted response at which no further split is desired.
#minprob - Minimum frequency of observations in terminal nodes.
#maxheight - Maximum height for the tree.


# Model 2:

ctrl <- chaid_control(
  minsplit = 200,
  minprob = 0.05
 
)

ctrl


chaid_model2 <- chaid(Attrition ~ ., data = newattrit,
                      control = ctrl)

print(chaid_model2)

plot(
  chaid_model2,
  main = "minsplit = 200, minprob = 0.05",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
    ),
  type = "simple"
)

# MODEL 3:

ctrl <- chaid_control(
  maxheight = 3
)

ctrl


chaid_model3 <- chaid(Attrition ~ ., data = newattrit,
                      control = ctrl)

print(chaid_model3)

plot(
  chaid_model3,
  main = "maxheight = 3",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)

# MODEL 4:

ctrl <- chaid_control(
  alpha2 = .01,
  alpha4 = .01
)

ctrl


chaid_model4 <- chaid(Attrition ~ ., data = newattrit,
                      control = ctrl)

print(chaid_model4)

plot(
  chaid_model4,
  main = "alpha2 = .01, alpha4 = .01",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)

#########
# Performance Metrics

# How Good is our Model?

## Accuracy

pmodel1 <- predict(chaid_model)
head(pmodel1)

## Question: Why Do we assess on the Data that the model is trained with?

pmodel2 <- predict(chaid_model2)
pmodel3 <- predict(chaid_model3)
pmodel4 <- predict(chaid_model4)


confusionMatrix(pmodel1, newattrit$Attrition)
confusionMatrix(pmodel2, newattrit$Attrition)
confusionMatrix(pmodel3, newattrit$Attrition)
confusionMatrix(pmodel4, newattrit$Attrition)

## Let's use purrr package to compare for models by creating a neat dataframe

library(kableExtra) # For prettier Printing:

# Create Model

modellist <- list(Model1 = chaid_model,
                  Model2 = chaid_model2,
                  Model3 = chaid_model3,
                  Model4 = chaid_model4)

# map(list_to_iterate, function_to_be Applied for each element of the list)

CHAIDResults <- map(modellist, ~ predict(.x)) %>%
  map(~confusionMatrix(newattrit$Attrition, .x)) %>%
  map_dfr(~cbind(as.data.frame(t(.x$overall)),
                 as.data.frame(t(.x$byClass))),
          .id = "ModelNumb" #Will become the column name identifying the model
                  )

## View it using Kable
library(kableExtra)

kable(CHAIDResults, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                font_size = 9)


## PartyKit: offers a way of assessing the relative importance of the variables by the varimp command

sort(varimp(chaid_model), decreasing = TRUE)
plot(sort(varimp(chaid_model), decreasing = TRUE))

###############
# Remember we have variables that are not included in Building the Model
# Let's see how they would add value to our model

# Turning NUmeric Variable into Factors

attrition %>%
  select_if(is.numeric) %>%
  gather(metric,value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

### Looking at the Graphs, Other than Age there is no Normal Distributions
# Why Do we find those with Normal Distribution?
# Is it because we need to factorize these and need to obtain some bins in order to do so?

# What are the best cutoff points to use then?

# Bse R cut function by default is based on distances along the X-axis

table(cut(attrition$YearsWithCurrManager, breaks = 5))
table(cut(
  attrition$YearsSinceLastPromotion,
  breaks = c(-1, 0.9,1.9, 2.0, 30 ),
  labels = c("Less than 1", "1", "2", "More than 2")
  ))

## In ggplot2, we have helper functions

# cut_interval : Makes n groups with equal range
# cut_number : Makes n groups with Approximately equal number of observations
# cut_width : Makes groups of a fixed Specified width


## Moving from Numeric to Factors

# Examples: 

table(cut_interval(attrition$YearsWithCurrManager, n = 5))
table(cut_number(attrition$YearsWithCurrManager, n = 5))
table(cut_width(attrition$YearsWithCurrManager, width = 2))

# For the Purpose of this example, Let's try to cut the bins by Equal size

attrition %>%
  mutate_if(is.numeric,
            funs(cut_number(.,n = 5)))

# ^ Insufficient data to produce 5 bins
# Check if there are COlumns that we are unable to generate where equal Number of Observations

# How do we know that this is the culprit Variable?
unique(attrition$YearsSinceLastPromotion)
unique(attrition$YearsSinceLastPromotion)


##

## Mutated on itself

attrition %>%
  select(-YearsSinceLastPromotion) %>%
  mutate_if(is.numeric,
            funs(cut_number(.,n = 5))) %>% head


# It Appears that is the ACtual variable
# Let's manually cut it

attrition$YearsSinceLastPromotion <- cut(
  attrition$YearsSinceLastPromotion,
  breaks = c(-1, 0.9, 1.9, 2.9, 30),
  labels = c("Less than 1", "1", "2", "More than 2")
)

#### New Attrition DS

attrition_new <- attrition %>% 
  mutate_if(is.numeric, funs(cut_number(., n=5)))

summary(attrition_new)

## Generate new dataset withadditional Binned(Factorized Variables)

newattrit <- attrition_new %>% 
  select_if(is.factor)

dim(newattrit)

## Repeat Process to Develop 4 new models


# Model 5

chaid_model5 <- chaid(Attrition ~ ., data = newattrit)

print(chaid_model5)

plot(
  chaid_model5,
  main = "Default control sliced Numerics",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)

# MOdel 6

ctrl <- chaid_control(
  minsplit = 200,
  minprob = 0.05
  
)

ctrl


chaid_model6 <- chaid(Attrition ~ ., data = newattrit,
                      control = ctrl)

print(chaid_model6)

plot(
  chaid_model6,
  main = "minsplit = 200, minprob = 0.05",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)


# MOdel 7

ctrl <- chaid_control(
  maxheight = 3
  
)

ctrl


chaid_model7 <- chaid(Attrition ~ ., data = newattrit,
                      control = ctrl)

print(chaid_model7)

plot(
  chaid_model7,
  main = "maxheight = 3",
  gp = gpar(
    col = "blue",
    lwd = 3,
    fontsize = 5
  ),
  type = "simple"
)































