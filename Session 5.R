#classification model
#build a k-nearest neighbour classifier on the iris data set
# make sure to install the caret package
library (caret)

iris=iris
# Step 1: split the data to train and test
training = iris[seq(1,nrow(iris),2),c(1,3,5)]
testing = iris[seq(2,nrow(iris),2),c(1,3,5)]

#Step2: train a k-nearest classifier
model=train(Species~.,training,method="kknn")
model$finalModel

#Step3: test the model
predictedSpecies=predict(model,testing)

#Step 4: measure accuracy of our model
sum(predictedSpecies==testing$Species)/nrow(testing)

####################################################################
#SAme as above with additional features
# Step 1: split the data to train and test
training = iris[seq(1,nrow(iris),2),]
testing = iris[seq(2,nrow(iris),2),]

#Step2: train a k-nearest classifier
model=train(Species~.,training,method="rf")
model$finalModel

#Step3: test the model
predictedSpecies=predict(model,testing)

#Step 4: measure accuracy of our model
sum(predictedSpecies==testing$Species)/nrow(testing)

####################################################################


