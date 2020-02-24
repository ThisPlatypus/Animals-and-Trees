library(MASS)
library(mclust)
mc <- Mclust(train, G=7) # 3 clusters
plot(mc, what=c("classification"), dimens=c(1,3)) # using 1st and 3rd column of the iris dataset
#http://www.di.fc.ul.pt/~jpn/r/EM/EM.html

model<- Mclust(train, G=7)
summary(model)
pred <- predict(model)
str(pred)
 # equal to
plot(pred$classification, col = pred$classification, pch = pred$classification)
# predict cluster over a grid

plot(pred$classification, col = mclust.options("classPlotColors")[pred$classification], pch = 15, cex = 0.5)
points(y, pch = model$classification)

plot(model)

plot(model, what = "BIC")

#######
#RF

library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2)  
set.seed(123)

rf = randomForest(y ~ .,  data=train,
                  ntree = 100)
plot(rf)  
#https://www.r-bloggers.com/random-forest-classification-of-mushrooms/


# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

# Predicting response variable
train$predicted.response = predict(rf , train)

# Create Confusion Matrix
print(  
  confusionMatrix(data = train$predicted.response,  
                  reference = y,
                  positive = 'Edible'))

# Predicting response variable
test$predicted.response <- predict(rf ,test)

# Create Confusion Matrix
print(  
  confusionMatrix(data=test$predicted.response,  
                  reference=test$predicted.response,
                  positive='Edible'))
