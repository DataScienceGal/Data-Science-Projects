###############################################
###  RANDOM FORESTS 
###############################################
# robust against overfitting
# hyperparameter for tuning 
# fit many trees to a bootstrap sample 
# bootstrap aggregation or bagging
# requires little preprocessing such as transforming predictors
# captures threshold effects and variable interactions
# randomForests do not yield coefficients
# ensemble of nonlinear decision trees into a highly flexible
# and often quite accurate model.

# RFs resample columns of data at each split to improve accuracy
install.packages("mlbench") # Confusion Matrix
install.packages("caret") # training and test split
install.packages("ranger")
library(mlbench)
library(caret)
library(ranger) # RandomForests 
#RcppEigen

#load Sonar data in mlbench package
data(Sonar)
dim(Sonar) #208 x 61

# Column 61 is the classifer
unique(Sonar[,61]) # Levels M vs. R
Sonar[1:5, c(1:5,61)]

# set seed
set.seed(1234)

# ranger method fits a random forest
# it is faster than the original random forest method in R (randomForest) (usually)
# RFs can automatically capture interactions between variables 
model = train(Class~., data=Sonar,method="ranger")
plot(model)


# Fit random forest with 5 fold CV
model <- train(Class~., data=Sonar,method="ranger",
  tuneLength = 1,
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)
model
plot(model)

# Tuning parameter = mtry
# This is the number of randomly selected variables to use at each split
# Goes from 2 to number of variables
# Low values are more random
# High values (ex. 100 ) are less random
# Example: tree with 10 splits and mtry = 2 implies 10 samples of 2 predictors with each split.

# Tuning Grid: tuneLength
# Will take longer to run if 10, default is 3 -- does a grid search
# Fit random forest with 5 fold CV
model <- train(Class~., data=Sonar,method="ranger" ,
               tuneLength = 3,
               trControl = trainControl(
                 method = "cv", 
                 number = 5, 
                 verboseIter = TRUE
               )
)
model
plot(model)


# Make a custom tuning grid
myGrid=data.frame(mtry=c(2,3,4,5,10,20),
                  .splitrule = "variance",
                  .min.node.size = 5)
model <- train(Class~., data=Sonar,method="ranger" ,
               tuneLength = 3,
               tunGrid=myGrid,
               trControl = trainControl(
               method = "cv", 
               number = 5, 
               verboseIter = TRUE))
model
plot(model)