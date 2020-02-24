library(MASS)
library(caTools)
library(class)
library(lattice)
library(latticeExtra)

library(class)
#PEr usare il pachetto
library("devtools")
install_github("davpinto/fastknn", force=T)

zoo_knn <- zoo 
zoo_knn=zoo_knn[-1]
colnames(zoo_knn)[17] <- "Target"
summary(zoo_knn)

## Split data for training and test
set.seed(412)
sample.idx <- sample.split(zoo_knn$Target, SplitRatio = 0.7)
x <- as.matrix(zoo_knn[sample.idx, -17])
y <- as.factor(zoo_knn$Target[sample.idx])


## Split data
set.seed(123)
tr.idx <- sample.split(y, SplitRatio = 0.7)
x.tr   <- x[tr.idx, ]
x.te   <- x[-tr.idx, ]
y.tr   <- as.factor(y[tr.idx])
y.te   <- as.factor(y[-tr.idx])

## Plot decision boundary
knnDecision(x.tr, y.tr, x.te, y.te, k = 7)

#https://github.com/davpinto/fastknn
library(doSNOW)
library(foreach)
library(knn)
#Find the Best k
set.seed(123)
error=NULL
pippo=1:15
for (i in pippo) {
  cv.out <- knn.cv(x,y , k = i, l = 0, prob = FALSE, use.all = TRUE)
error[i]=1- sum(table(cv.out, y)[  table(cv.out, y)!=diag( table(cv.out, y))])/length(y)

}
(cbind(pippo, error))

#scelgo k=3 e k=1

#scalo i dati
x=scale(x)

#vedo il modello scelto
suppressWarnings(suppressMessages(library(kknn)))
model <- train.kknn(y ~ ., data = as.data.frame(x), kmax = 9)
model

prediction <- predict(model,zoo_knn[-sample.idx, -17])
table(prediction, as.factor(zoo_knn$Target[-sample.idx]))


error=1- sum(table(prediction, zoo_knn$Target[-sample.idx])[  table(prediction, zoo_knn$Target[-sample.idx])!=diag( table(prediction, zoo_knn$Target[-sample.idx]))])/length(zoo_knn$Target[-sample.idx])
