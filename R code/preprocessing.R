### PREPROCESSING


set.seed(1234)

myCtrl=trainControl(method="cv",number=5,verbose=TRUE)

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
names(bbbDescr)[nearZeroVar(bbbDescr)]
# "negative"     "peoe_vsa.2.1" "peoe_vsa.3.1" "a_acid"  


####### Remove zero variance variables
model <- train(
  x = bbbDescr, 
  y = logBBB, 
  method = "glm",
  trControl=myCtrl,
  preProcess=c("zv","center","scale")
)
model
min(model$results$RMSE) 
# There were more warnings


####### Remove near zero variance variables
model <- train(
  x = bbbDescr, 
  y = logBBB, 
  method = "glm",
  trControl=myCtrl,
  preProcess=c("nzv","center","scale")
)
model
min(model$results$RMSE) # 1.55391
# this eliminates the warnings




####### Remove zero variance variables + PCA
model <- train(
  x = bbbDescr, 
  y = logBBB, 
  method = "glm",
  trControl=myCtrl,
  preProcess=c("zv","center","scale","pca")
)
model
min(model$results$RMSE) # 0.5413753


##### Do not remove variables and run PCA
##### This approach allows for several low variance predictors
##### to be combined into one high variance PCA variable
##### thereby allowing for a positive impact on accuracy
##### Especially useful for linear models
##### PCA option will center and scale data
##### This approach is preferred over elimination of data

# Fit glm model using PCA: model
model <- train(
  x = bbbDescr, 
  y = logBBB, 
  method = "glm", 
  preProcess = "pca"
)
## produces warnings
model