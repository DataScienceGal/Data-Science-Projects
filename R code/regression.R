##############################################
### LINEAR REGRESSION USING CARETS PACKAGE ###
##############################################
install.packages("ggplot2")
install.packages("caret")
library(ggplot2)
library(caret)

# load diamonds data from library ggplot2
# The dataset contains physical attributes of diamonds 
# as well as the price they sold for
data(diamonds)
head(diamonds,5)

###################################################
# Predict diamond price based on their attributes #
###################################################
# linear regression
###################################################

# Fit lm model: model
mod=lm(price~.,diamonds)

# Predict on full data: p
pred=predict(mod, diamonds, type="response")

# Compute errors: error
error=pred-diamonds$price

# Calculate RMSE
rmse = sqrt(mean(error^2)) # 1129.843
rmse 

#################################################
########## Train/test split on data  ############
#################################################
### TECHNIQUE #1 - randomly split data
#################################################
# reorder data and divide into two sets
set.seed(1234)

# shuffle row indices 
rows=sample(nrow(diamonds))

# reorder data
shuffled_diamonds=diamonds[rows,]

# split data (80% training/20% test)
split = round(nrow(shuffled_diamonds) * 0.8)
tr=shuffled_diamonds[1:split,]
ts=shuffled_diamonds[(split+1):nrow(shuffled_diamonds),]
              
# fit regression model on training data 
# using all variables as covariates
mod = lm(price~.,tr)
pred = predict(mod, ts)
              
# Compute errors: error
error=pred-test$price
              
# Calculate RMSE
rmse=sqrt(mean(error^2))
rmse #1136.596
# This RMSE is higher. 
# The model appears overfit because all predictors are used.
              
              
#################################################
########## Train/test split on data  ############
#################################################
### TECHNIQUE #2 - Using Caret package
#################################################
# Includes 5-fold cross-validation 
# for multiple estimates of out-of-sample error
# If all of estimates have similar outputs, more certainty of the model accuracy. 
# If estimates give different outputs, the model does not perform consistently 
# and suggests a problem with it.
#################################################

# Caret will split the data into CV folds.
model = train(price~., diamonds, method="lm",
              trControl=trainControl(
              method="cv",
              number=5,
              verboseIter=TRUE
              ) )
              
# RMSE = 1131.235
# R-squared = 0.9195795
# MAE = 740.5153
              
              
# repeated CV - better estimate of test error
# 5 x 5 CV
model = train(price~., diamonds, method="lm",
              trControl=trainControl(
              method="cv",
              number=5,
              repeats=5,
              verboseIter=TRUE
              ) )
              
# predict on full dataset
predict(model, diamonds)