################
#Librerie
library(ggplot2) 
library(readr) 
library(Hmisc) 
library(plyr) 
library(corrplot) 
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(randomForest)  
library(e1071)  
library(caret)
library(gbm)

###########Load File#########
class <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/class.csv")

zoo <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/zoo.csv")
#Merge
class = rename(class , c("Class_Number"="class_type" , "Class_Type" = "Types"))
zoo = merge (zoo , class , by = c("class_type")  )

data=zoo[-1]
data=data[-1]
data=data[-19]
data=data[-17]
attach(data)



##################################################
#####Descrittive totale######
par(mfrow = c(2,1))
#Correlazione
corrplot(cor(zoo[,c("hair" , "feathers" , "eggs" , "milk" , "airborne" , "aquatic" , "predator" , "toothed" , "backbone" , "breathes" , "venomous" , "fins" ,  "tail" , "domestic" , "catsize" )]), type = "upper", method="ellipse",order = "hclust",
         col = c("lightblue", "lightgreen"), bg = "lightyellow")


corrplot(cor(zoo[,c("hair" , "feathers" , "eggs" , "milk" , "airborne" , "aquatic" , "predator" , "toothed" , "backbone" , "breathes" , "venomous" , "fins" ,  "tail" , "domestic" , "catsize" )]), type = "upper", method="number",number.digits=1,order = "hclust",
         col = c("darkblue", "darkgreen"), bg = "lightyellow")

#######Istogrammi

#hair
p <- ggplot(zoo, aes(x=hair , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$hair)

#feathers
p <- ggplot(zoo, aes(x=feathers , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$feathers)

#eggs
p <- ggplot(zoo, aes(x=eggs , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$eggs)

#milk
p <- ggplot(zoo, aes(x=milk , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$milk)


#airborne
p <- ggplot(zoo, aes(x=airborne , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$airborne)

#aquatic
p <- ggplot(zoo, aes(x=aquatic , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$aquatic)

#predator
p <- ggplot(zoo, aes(x=predator , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$predator)

#toothed
p <- ggplot(zoo, aes(x=toothed , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$toothed)


#backbone
p <- ggplot(zoo, aes(x=backbone , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$backbone)


#breathes
p <- ggplot(zoo, aes(x=breathes , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$breathes)



#venomous
p <- ggplot(zoo, aes(x=venomous , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$venomous)




#fins
p <- ggplot(zoo, aes(x=fins , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$fins)




#legs
p <- ggplot(zoo, aes(x=legs , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$legs)



#tail
p <- ggplot(zoo, aes(x=tail , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$tail)




#domestic
p <- ggplot(zoo, aes(x=domestic , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$domestic)


#catsize
p <- ggplot(zoo, aes(x=catsize , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$catsize)

#####################
#######Costruzione Train e Test set######
dta <- sample(2, nrow(zoo), replace = T, prob = c(0.7, 0.3))

ytrain=data[dta == 1,17]
train <- data[dta == 1,-17]

ytest=data[dta == 2,17]
test <- data[dta == 2,-17]

#########SUMMARY train

summary(data[dta == 1,])
summary(data[dta !=1,])
#######################################################################################
##MODELLI##########
all_var_zoo <- ytrain~hair+feathers+eggs+milk+airborne+aquatic+predator+toothed+backbone+breathes+venomous+fins+legs+tail+domestic+catsize
var_impo_rf<-ytrain~hair+feathers+eggs+milk+toothed+backbone+fins+legs 
var_gb<-ytrain~milk+hair+toothed+feathers+legs+eggs+aquatic+predator+catsize

#####RPART: CART#####
#gini
a=rpart.control(minsplit = 20, cp = 0.001,
              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
              surrogatestyle = 0, maxdepth = 30)

CARTg=rpart(all_var_zoo, data=train, method="class", control = a)


#info
CARTi=rpart(all_var_zoo, data=train, method="class", control=a,parms = list( split = "information") )

#plot
dev.new()
par(mfrow = c(2,1))
rpart.plot(CARTg, type=2, main="CART usando l'indice di Gini")
rpart.plot(CARTi, type=2, main="CART usando l'error rate")

plotcp(CARTg, main="CART usando l'indice di Gini")
plotcp(CARTi, main="CART usando l'error rate")

#predict
cpredg=predict(CARTg,test, type="class")

cpredi=predict(CARTi,test, type="class")

table(cpredg, ytest)
table(cpredi, ytest)

#####RANDOM FOREST#####
set.seed(123)

rf = randomForest(all_var_zoo,  data=train,
                  ntree = 150)
dev.new()
plot(rf, main="Random Forest") 

#predict
predf=predict(rf, test, type="class")
table(predf, ytest)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=8,
           main="Top 8- Variable Importance")

rfvi = randomForest(var_impo_rf,  data=train,
                  ntree = 150)
dev.new()
plot(rf, main="Random Forest con variabili principali")  

#predict
predfvi=predict(rfvi, test, type="class")
table(predfvi, ytest)

#####Stochastic gradient boosting#####
set.seed(124)
train1=cbind.data.frame(ytrain, train)
test1=cbind.data.frame(ytest, test)

# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# train the model
set.seed(27)
caret_gbm_all <- caret::train(all_var_zoo,
                              data = train1,
                              method = "gbm",
                              preProcess = NULL,
                              verbose = FALSE,
                              trControl = control)
#Confusion MAtrix
gbm <- confusionMatrix(predict(caret_gbm_all, newdata = test1), test1$ytest)


# estimate variable importance
importance <- varImp(caret_gbm_all, scale=TRUE)
importance

plot(caret_gbm_all)

# train the model
set.seed(27)
caret_gbm <- caret::train(var_gb,
                              data = train1,
                              method = "gbm",
                              preProcess = NULL,
                              verbose = FALSE,
                              trControl = control)
#Confusion MAtrix
gbm <- confusionMatrix(predict(caret_gbm_all, newdata = test1), test1$ytest)


plot(caret_gbm)
