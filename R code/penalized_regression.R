# GLMNET models
# Lasso vs. Ridge (penalty on large coeffs)
# ElasticNet (mix of Lasso and Ridge)
# alpha between 0 (ridge) and 1 (lasso);  mix is inbetween
# lambda between 0 and infinity and controls size of penalty
# higher lambda the simpler the model
# glmnet will use AUC rather than accuracy to select the final model parameters

install.packages("caret") # training and test split
install.packages("glmnet")
library(glmnet)
library(caret)
set.seed(1234)

overfit=read.csv("C:\\Users\\stef_\\Documents\\R\\Machine Learning with caret in R\\3-Penalized Regression\\overfit.csv")
myCtrl=trainControl(method="cv",number=5,
                    summaryFunction=twoClassSummary,
                    classProbs=TRUE,
                    verboseIter=TRUE)
model=train(y~.,overfit,method="glmnet",trControl=myCtrl)
model
plot(model)

# Default tuning grid has 3 alphas and 3 lambdas
# this suggests choosing alpha = 1 (lasso)
# lambda =0.0101274483

# Print maximum ROC statistic
model[["results"]]
max(model[["results"]]$ROC)

#### TUNING GRID
# submodel - for each alpha, fit all lambdas
myGrid=expand.grid(alpha=0:1,lambda=seq(.0001,0.1,length=10))
model=train(y~.,overfit,method="glmnet",
            tuneGrid=myGrid,
            trControl=myCtrl)
plot(model)
# This says use lambda=1 and alpha=0.045 abouts


# plot full regularizatoin where alpha = 0 (ridge)
plot(model$finalModel)
# plot shows how regression coeffs are shrunk from right to left 
# as penalty of coeff size increases and simplies model
# some lines hit zero as we increase lambda indicating these coeffs dropped from model


# Print maximum ROC statistic
max(model[["results"]]$ROC)


### churn data is no longer available in C50 packages.
### there is a churn dataset that is different but available in modeldata
### I am not reading it in here
myGrid=expand.grid(alpha=0:1,lambda=0:10/10))

model=train(churn~.,churnTrain,
            metric="ROC",
            method="glmnet",
            tuneGrid=mygrid,
            trControl=myCtrl)
plot(model)
# caret automatically choose the best values for alpha and lambda
# Nothing needs to be done after looking at this plot
# The plot is useful to understand how the model works

plot(model$finalModel)
    