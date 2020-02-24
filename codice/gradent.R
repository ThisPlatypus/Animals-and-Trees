library(caret)

all_var_zoo <- class_type~hair+feathers+eggs+milk+airborne+aquatic+predator+toothed+backbone+breathes+venomous+fins+legs+tail+domestic+catsize

rpart_1 <- rpart(all_var_zoo,
                 train,
                 method="class")
library("rpart.plot", lib.loc="~/R/win-library/3.4")
rpart.plot::rpart.plot(rpart_1, type = 2, fallen.leaves = FALSE, extra = 4)
testPred <- predict(rpart_1, newdata = test, type="class")
predictability <- sum(testPred == test$class_type)/ length(test$class_type)*100
print(predictability)
r_Part_All_Variables <- as.character(predictability)


res=as.factor(zoo$class_type)
data <- createDataPartition(y = res, p=0.7, list=FALSE)
train <- zoo[data,]
test <- zoo[-data,]

# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# train the model
set.seed(27)
caret_gbm_all <- caret::train(all_var_zoo,
                              data = train,
                              method = "gbm",
                              preProcess = NULL,
                              verbose = FALSE,
                              trControl = control)
# estimate variable importance
importance <- varImp(caret_gbm_all, scale=TRUE)
importance


gbm <- table(round(predict(caret_gbm_all, newdata = test),0), test$class_type)

#calcolare errore
######################
#STOchastic

(caret_gbm_imp <- caret::train(class_type~hair+feathers+eggs+milk+airborne+aquatic+predator+toothed+breathes+legs+tail+catsize,
                               data = train,
                               method = "gbm",
                               verbose = FALSE,
                               preProcess = NULL,
                               trControl = control))


Yhat=NULL
for (i in 1:length(predict(caret_gbm_imp, newdata = test))) {
  if(predict(caret_gbm_imp, newdata = test)[i]>(round(predict(caret_gbm_imp, newdata = test)[i])+0.5)){
    Yhat[i]=round(predict(caret_gbm_imp, newdata = test)[i])+1
  }else{
    Yhat[i]=round(predict(caret_gbm_imp, newdata = test)[i])
  }
  
}
table(Yhat, test$class_type)
