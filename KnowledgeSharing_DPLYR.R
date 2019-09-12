
### Knowledge Sharing - DPLYR Demo

library(dplyr)

# DATA > Process(Cleaning, JOining, Select, Filter) > ANalyze > Visualize > Prediction

library(ISLR)

Data <- Default

dim(Data)

head(Data)

names(Data)

str(Data)

summary(Data)

Income <- Data$income

#----- DPLYR ------#

## Select Income & Student Only from Data

# Option 1:
Income <- Data$income
Student <- Data$student

NewDS <- data.frame(Income = Income,Student = Student)


## DPLYR : WAY
library(dplyr)

# Piping:
 
NewDS_V2 <- Data %>%
  filter(student == 'No' & default == "Yes") %>%
  select(student,income, default)


# Group by and Aggregate














