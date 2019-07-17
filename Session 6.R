#comprehensive analysis of model(bootstrap,k-fold, repeated k-fold)
library(caret)
airquality=airquality
myData=airquality[,1:4]

#clean the data set (remove missing values)
myData=myData[complete.cases(myData),]

#bootstrap 
train_control <- trainControl(method="boot", number=50)
m1 = train(Ozone~., data=myData, trControl=train_control, method="lm")
m2 = train(Ozone~., data=myData, trControl=train_control, method="rf")
m3 = train(Ozone~., data=myData, trControl=train_control, method="brnn")
m4 = train(Ozone~., data=myData, trControl=train_control, method="kknn")

#visualize the accuracies
allModels=resamples(list(LinearRegression=m1,RandomForst=m2,Bayesian=m3, KNearest=m4))
bwplot(allModels,scales=list(relation="free"))

#######################################################################
#k-fold cross validation
train_control <- trainControl(method="cv", number=5)
m1 = train(Ozone~., data=myData, trControl=train_control, method="lm")
m2 = train(Ozone~., data=myData, trControl=train_control, method="rf")
m3 = train(Ozone~., data=myData, trControl=train_control, method="brnn")
m4 = train(Ozone~., data=myData, trControl=train_control, method="kknn")

#visualize the accuracies
allModels=resamples(list(LinearRegression=m1,RandomForst=m2,Bayesian=m3, KNearest=m4))
bwplot(allModels,scales=list(relation="free"))

#######################################################################
#k-fold repeated cross validation
train_control <- trainControl(method="repeatedcv", number=5,repeats=10)
m1 = train(Ozone~., data=myData, trControl=train_control, method="lm")
m2 = train(Ozone~., data=myData, trControl=train_control, method="rf")
m3 = train(Ozone~., data=myData, trControl=train_control, method="brnn")
m4 = train(Ozone~., data=myData, trControl=train_control, method="kknn")

#visualize the accuracies
allModels=resamples(list(LinearRegression=m1,RandomForst=m2,Bayesian=m3, KNearest=m4))
bwplot(allModels,scales=list(relation="free"))


########################################################################
########################################################################
#unsupervised learning: clustering
#k-mean clustering model
library(stats)
myData=iris[,c(1,3)]
myclusters=kmeans(myData,3)
myclusters$cluster
myData$clusterLabel = myclusters$cluster
#visualize scatterplot and show cluster label as colore
ggplot(myData,aes(Sepal.Length,Petal.Length,color=as.factor(clusterLabel)))+geom_point(size=3)+theme_bw()

       