install.packages("caret")
#install.packages("C50") # churn data used to be available here
#install.packages("BDgraph") # different churn dataset
install.packages("modeldata")
install.packages("glmnet")
install.packages("ranger")
library(caret)
library(modeldata)
library(glmnet)
library(ranger)
#library(C50) 
#library(BDgraph) 
set.seed(1234)
C:\Users\stef_\Documents\R\Machine Learning with caret in R\5-Model Comparisons

### CHURN DATA: customers stop doing business - customer attrition
# https://search.r-project.org/CRAN/refmans/BDgraph/html/churn.html
data(mlc_churn)
names(mlc_churn)
data(churn)
summary(mlc_churn)
head(churn_y) #[1] "no" "no" "no" "no" "no" "no"


# notice churn rate is about 14%
table(churnTrain$churn)/nrow(churnTrain)


# Create train/test indexes
# Preserves the churn rate of 14% in each fold
# This creates 5 folds with the index for samples taken from churnTrain$churn
myFolds = createFolds(churnTrain$churn, k=5)


# compare class distribution
i = myFolds$Fold1
table(churnTrain$churn[i])/length(i)

# In order to compare models, we need every model to have the same folds
# use this training control on all models so that they have 
# the exact same cross-validation folds for every model
# further, every model will use the same SummaryFunction and Tuning parameters
# by savings indexes in the trainControl, we can fit models on the same CV folds
myControl=trainControl(
  summaryFunction=twoClassSummary,
  classProbs=TRUE,
  verboseIter=TRUE,
  savePredictions = TRUE,
  index=myFolds)



# Now apply this control to penalized regression model
myGrid=expand.grid(alpha=0:1,lambda=0:10/10))

model_glmnet=train(churn~.,churnTrain,
            metric="ROC",
            method="glmnet",
            tuneGrid=mygrid,
            trControl=myControl)
plot(model_glmnet)
# caret automatically choose the best values for alpha and lambda
# Nothing needs to be done after looking at this plot
# The plot is useful to understand how the model works

plot(model_glmnet$finalModel)


# Now apply this control to random Forest box 
# slower than GLMNEt
# requires little preprocessing such as transforming predictors
# captures threshold effects and variable interactions
churnTrain$churn = factor(churnTrain$churn,
                          levels=c("no","yes"))
model_rf=train(churn~.,churnTrain,
            metric="ROC",
            method="ranger",
            trControl=myControl)
plot(model_rf)
# caret automatically chooses the best results for mtry
# randomForests do not yield coefficients


## Assess which of 2+ models is the best 
# selection criteria: highest mean AUC and lowest STD for AUC
# compare glmnet and rf models
model_list = list(glmnet=model_glmnet,
                  rf=model_rf)

# collect results from CV folds
resamps = resamples(model_list) # this is a caret function
resamps

summary(resamps)


######## DETERMINE BEST MODEL ##########
#Box-Whisker Plot
# compare the distribution of prediction accuracy (AUC)
# choose one with highest median and smallest range between min/max
bwplot(resamps, metric="ROC") # look for model with highest avg. AUC

# Dot plot
dotplot(resamps, metric="ROC") # confidence intervals

# Density plot
# useful to look for outlier folds with unusally high/low AUCs
densityplot(resamps, metric="ROC") # distribution of AUCs

# Scatter plot
# compare AUC on all 5 CV folds
# look to see if out model is consistently better than others across all folds
# Look to see if inferior model produces better predictions on a particular subset
# does RF model provide higher AUC tha/n GLMNET on every fold? If yes, choose RF model
xyplot(resamps, metric="ROC")


# In the caretEnsemble package, careStack function makes a stock of caret model
# where two submodels feed into another caret model that is hopefully more accurate.
# Create ensemble model: stack
stack <- caretStack(model_list, method="glm")

# Look at summary
summary(stack)