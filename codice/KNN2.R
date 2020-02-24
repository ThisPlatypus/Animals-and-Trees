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
CM=table(prediction, as.factor(zoo_knn$Target[-sample.idx]))

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy
plot(model)

####PLOT KNN
library(plyr)
library(ggplot2)
set.seed(123)

# Create training and testing data sets
idx = sample(1:nrow(iris), size = 100)
train.idx = 1:nrow(iris) %in% idx
test.idx =  ! 1:nrow(iris) %in% idx

train = iris[train.idx, 1:4]
test = iris[test.idx, 1:4]

# Get labels
labels = iris[train.idx, 5]

# Do knn
fit = knn(train, test, labels)
fit

# Create a dataframe to simplify charting
plot.df = data.frame(test, predicted = fit)

# Use ggplot
# 2-D plots example only
# Sepal.Length vs Sepal.Width

# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Sepal.Length, 
                      y = plot.df$Sepal.Width, 
                      predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)