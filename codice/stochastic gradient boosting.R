attach(train)
data=data.frame(y=u,train)
data=data[-1]
attach(data)
gbm1=gbm(y~., data=train
         , distribution = "multinomial",
    n.trees = 50
   ,interaction.depth = 2,
    shrinkage = 0.01,
    bag.fraction = 0.5,
    train.fraction = 0.8,
    keep.data = TRUE,
    verbose = F,
    class.stratify.cv=NULL,
    n.cores = NULL)
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)
# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
# plot the performance # plot variable influence
summary(gbm1,n.trees=1) # based on the first tree
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))

# predict on the new data using "best" number of trees
# f.predict generally will be on the canonical scale (logit,log,etc.)
f.predict <- predict(gbm1,test,best.iter)
forse<- predict(gbm1,test, type="response")
table(ytest,forse)
# least squares error
print(sum((test$ytest-f.predict)^2))
# create marginal plots
# plot variable X1,X2,X3 after "best" iterations
par(mfrow=c(1,3))
plot(gbm1,1,best.iter)
plot(gbm1,2,best.iter)
plot(gbm1,3,best.iter)
par(mfrow=c(1,1))
# contour plot of variables 1 and 2 after "best" iterations
plot(gbm1,1:2)
# lattice plot of variables 2 and 3
plot(gbm1,2:3)
# lattice plot of variables 3 and 4
plot(gbm1,3:4)
# 3-way plots
plot(gbm1,c(1,2,6),cont=20)
plot(gbm1,1:3,best.iter)
plot(gbm1,2:4,best.iter)
plot(gbm1,3:5,best.iter)



equire(gbm)
boost.boston=gbm(all_var_zoo,data=train,distribution="multinomial",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.boston)
plot(boost.boston,i="milk")
plot(boost.boston,i="eggs")

n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=test,n.trees=n.trees)
dim(predmat)
berr=with(test,apply( (predmat-ynotf)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")

############


#fit modello
gbm1=gbm(all_var_zoo, data=train
         , distribution = "multinomial",
         n.trees = 100
         ,interaction.depth = 1,
         shrinkage = 0.001,
         bag.fraction = 0.7,
         train.fraction = 0.8,
         keep.data = TRUE,
         verbose = F,
         class.stratify.cv=T,
         n.cores = NULL)

#Perfomance
gbm.perf(gbm1)
#The first plot plots object$train.error (in black) and
#object$valid.error (in red) versus the iteration number.

#variable importance
dev.new()
summary(gbm1)

# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)

# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)

#Confusion matrix
predmat=predict(gbm1,newdata=test,n.trees=50, type = "response")
a=matrix(predmat, nrow=length(ytest), ncol=7)
colnames(a)=colnames(predmat)
forse=NULL
for (i in 1:length(ytest)) {
  forse[i]=names(which.max(a[i,]))
}
forse
forse=factor(forse)
table(forse,ytest)
#var impo model
gbm2=gbm(var_gb, data=train
         , distribution = "multinomial",
         n.trees = 100
         ,interaction.depth = 1,
         shrinkage = 0.01,
         bag.fraction = 0.5,
         train.fraction = 0.8,
         keep.data = TRUE,
         verbose = F,
         class.stratify.cv=NULL,
         n.cores = NULL)

predmat=predict(gbm2,newdata=test,n.trees=50, type = "response")
a=matrix(predmat, nrow=length(ytest), ncol=7)
colnames(a)=colnames(predmat)
forse=NULL
for (i in 1:length(ytest)) {
  forse[i]=names(which.max(a[i,]))
}
forse
forse=factor(forse)
table(forse,ytest)
