################################################
### LOGISTIC REGRESSION USING CARETS PACKAGE ###
################################################
install.packages("mlbench") # Confusion Matrix
install.packages("caret") # training and test split
install.packages("caTools") #ROC Curve
library(mlbench)
library(caret)
library(caTools)

#load Sonar data in mlbench package
data(Sonar)
dim(Sonar) #208 x 61

# Column 61 is the classifer
unique(Sonar[,61]) # Levels M vs. R
Sonar[1:5, c(1:5,61)]

#################################################
########## Train/test split on data  ############
#################################################
# Since this data is small we will use 70% training
### TECHNIQUE #1 - randomly split data
#################################################
# reorder data and divide into two sets
set.seed(1234)

# shuffle row indices 
rows=sample(nrow(Sonar))

# reorder data
shuffled_Sonar=Sonar[rows,]

# Split into 70% training and 30% test
n=round(nrow(Sonar) * 0.7)
tr=shuffled_Sonar[1:n,]
ts=shuffled_Sonar[(n+1):nrow(Sonar),]

# Fit logistic regression model using training data
model=glm(Class~.,family=binomial(link="logit"),tr)

# Predict on test
p=predict(model, ts, type="response")
summary(p) # give Min/Max, IQR, Mean

#################################################
### CONFUSION MATRIX
#################################################
# find appropriate p_0
p_0=0.9
# If prob>p_0, then classify as "M", otherwise "R"
p_class=ifelse(p>p_0,"M","R")
table(p_class) # tabulate p_class variable
p_class

# 2way freq table
table(p_class, ts[["Class"]])

# Convert p_class to a factor with 2 levels R and M
p_class=factor(p_class,levels=levels(ts[["Class"]]))

# Confusion matrix can calculate Sensitivity and Specificity
confusionMatrix(p_class, ts[["Class"]])
# No information rate = always predicts M = 43/83


#################################################
### Area under the ROC Curves
### Evaluate all possible thresholds
### True positive (sensitivity) vs. False Positive Rate (1-Specificity)
#################################################
# use caTools library
p=predict(model, ts, type="response")
colAUC(p, ts[["Class"]], plotROC=TRUE)
# x-axis is the false positive rate
# y-axis is the true positive rate
# random predictions tend to follow 45 degree diagonal line (AUC = 0.5)
# box is produced for perfect classificatons (AUC = 1)
# Model that is always wrong has AUC = 0

# Five fold CV
myCtrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # use to prevent error in twoClassSummary()
  verboseIter = TRUE
)

# Train glm with myCtrl
model = train(Class~., Sonar, method="glm", trControl=myCtrl)
model

# if warning about convergence or probabilities, then use predictions 
# to validate whether the model is accurate enough and if so, ignore.