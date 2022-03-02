install.packages("caret") # needed for Train function to pre-process data
install.packages("randomForest")
library(caret)
library(randomForest)
set.seed(1234)

myControl=trainControl(method="cv",number=5,
                    summaryFunction=twoClassSummary,
                    classProbs=TRUE,
                    verboseIter=TRUE)

############################
#### MEDIAN IMPUTATION #####
############################
# Can produce incorrect results if not missing at random
data(mtcars)

# Force missing data on hp = horse power
mtcars[sample(1:nrow(mtcars), 10),"hp"]=NA

# split target from predictors
Y=mtcars$mpg
X=mtcars[,2:4]

# fit model (needs randomForest package)
model=train(X,Y)

# Impute missing values using median
# Performed inside of each fold of CV
model=train(X,Y,preProcess="medianImpute")
model


# force missing data
mtcars[mtcars$disp<140,"hp"]=NA
Y=mtcars$mpg
X=mtcars[,2:4]

# median imputation
model=train(X,Y,method="glm",preProcess="medianImpute")
min(model$results$RMSE) # 3.022627


#load.Rdata(filename="C:\\Users\\stef_\\Documents\\R\\Machine Learning with caret in R\\Datasets\\BreastCancer.Rdata")
data(BreastCancer)
colnames(BreastCancer)
head(BreastCancer,10)

# Apply median imputation: median_model
median_model <- train(
  x = BreastCancer[,2:10], 
  y = BreastCancer$Class,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

median_model



#######################
### KNN IMPUTATION ####
#######################
# use when missing not at random
data(mtcars)

# force missing data NOT at random
mtcars[mtcars$disp<140,"hp"]=NA
Y=mtcars$mpg
X=mtcars[,2:4]

# KNN imputation
model=train(X,Y,method="glm",preProcess="knnImpute")
min(model$results$RMSE) #  3.022627


### BREAST Cancer DATA

data(BreastCancer)
colnames(BreastCancer)
head(BreastCancer,10)

# Apply median imputation: median_model
knn_model <- train(
  x = BreastCancer[,2:10], 
  y = BreastCancer$Class,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

knn_model

# resampled results of median Imput and Knn Impute = resampled
dotplot(resamples, metric="ROC")


##############################
#### PREPROCESSING STEPS #####
##############################
#1 - Impute missing values
#2 - Standardize data: Center and scale data (requires no missing data) - mean 0, std 1
#3 - transform data
#4 - Fit model
# Tree-based models don't need a lot of preprocessing
# For tree models, can get away with imputing and fitting model


data(mtcars)

# Force missing at random data
mtcars[sample(1:nrow(mtcars), 10),"hp"] = NA
Y = mtcars$mpg
X = mtcars[,2:4]


model = train(X,Y,method="glm",
              preProcess=c("medianImpute","center","scale"))
min(model$results$RMSE) # 3.378837


### ADD PCA TRANSFORMATION To MODEL ADTER IMPUTING, CENTERING, SCALING
model = train(X,Y,method="glm",
              preProcess=c("medianImpute","center","scale","pca"))
min(model$results$RMSE) # 3.29473


### ADD SPATIAL SIGN TRANSFORMATION To MODEL ADTER IMPUTING, CENTERING, SCALING
# projects data onto sphere
# good when outliers are present
# good with high dimensional data
model = train(X,Y,method="glm",
              preProcess=c("medianImpute","center","scale","spatialSign"))
min(model$results$RMSE) # 3.381101



### NOW USE BREAST CANCER DATA
data(BreastCancer)
colnames(BreastCancer)
head(BreastCancer,10)

# Apply median imputation: median_model
model <- train(
  x = BreastCancer[,2:10], 
  y = BreastCancer$Class,
  method = "glm", # fit logistic since response is binary
  trControl = myControl,
  preProcess = "medianImpute"
)

model

# Fit glm with median imputation
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute","center","scale")
)
model


#############################
### VARIANCE OF VARIABLES ###
#############################
## variables that are constant don't contain much useful info
## remove those with no or low variances since they can cause issues during CV and don't add much info
## add "zv" to remove constant columns
## add "nzv" to remove nearly constant columns
## add before imputate in preprocessing

#### MTCARS DATA
data(mtcars)

# Force missing at random data
mtcars[sample(1:nrow(mtcars), 10),"hp"] = NA
Y = mtcars$mpg
X = mtcars[,2:4]

# add constant-valued column to mtcars
X$bad = 1

## This model will produce lots of errors in preProcessing due to X$bad
model = train(X,Y,method="glm",
              preProcess=c("medianImpute","center","scale","pca"))
#  These variables have zero variances: bad
# Something is wrong; all the RMSE metric values are missing:


## This model works
model = train(X,Y,method="glm",
              preProcess=c("zv","medianImpute","center","scale","pca"))
min(model$results$RMSE) # 3.324426



##############################################
#### nearZeroVar() Utlity in PreProcessing ###
##############################################
# freqCut - Looks at ratio of most common to second most common values (default 19)
# uniqueCut - Looks at percent of distinct values of n (default 10)


#### Blood-Brain dataset available in caret
# https://rdrr.io/cran/caret/man/BloodBrain.html
# goal: predict biochemical compounds ability to cross blood-brain barrier
# given by this formula:
# log((concentration of compound in brain) /
#      (concentration of compound in blood))
# many variables have low variance because they are mostly a signle value
data(BloodBrain)
head(bbbDescr) # dataframe of chemical descriptors
head(logBBB) # vector of assay results

# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bbbDescr, names = TRUE, 
                           freqCut = 2, uniqueCut =20)

# Get all column names from bbbDescr: all_cols
all_cols=names(bbbDescr)

# Remove from data: bloodbrain_x_small
# set diff is take all column names from first argument that are not in second argument
bloodbrain_x_small <- bbbDescr[ , setdiff(all_cols, remove_cols)]

# Fit model on reduced data: model
model <- train(
  x = bloodbrain_x_small, 
  y = logBBB, 
  method = "glm"
)
model
# There were 25 warnings (use warnings() to see them)
# glm generates a lot of warnings about convergence, 
# but they're never a big deal 
# and you can use the out-of-sample accuracy to make good predictions.
