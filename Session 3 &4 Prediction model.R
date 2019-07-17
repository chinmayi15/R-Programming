#build a regression model to predict mpg from wt
library(caret)
#4 steps for predictive modeling

#Step 1: select the columns from the data set
training=mtcars[,c("wt","mpg")]

#Step 2: train the regression model (define target variable)
model=train(mpg ~.,training,method="lm")
#model$finalModel


# hp vs mpg liner regression
training2=mtcars[,c("hp","mpg")]
model2=train(mpg ~.,training2,method="lm")
model2$finalModel

#mpg vs hp,wt,qsec (target variable is like y) multiple linier regression
training3=mtcars[,c("hp","mpg","wt","qsec")]
model3=train(mpg ~.,training3,method="lm")
model3$finalModel


#Step 3: Test our model
predictedMpgs=predict(model3,training3)

#step 4:measure accuracy of our model (wrong method because tested and trained on same)
mean(abs(predictedMpgs-training3$mpg))
MAE(predictedMpgs,training3$mpg)
RMSE(predictedMpgs,training3$mpg)
R2(predictedMpgs,training3$mpg)

###########################################################################
###########################################################################
#Day 2
###########################################################################


#predict mpg using data split
#Step1: Split to train and test
library(caret)
training = mtcars[seq(1,nrow(mtcars),2),c("hp","qsec","wt","mpg")]
testing = mtcars[seq(2,nrow(mtcars),2),c("hp","qsec","wt","mpg")]

#Step2 : train our model
model= train(mpg~.,training, method='lm')

#step3 : test the model
predictedMpgs=predict(model,testing)

#Step4: evaluate our model
MAE(predictedMpgs,testing$mpg)
RMSE(predictedMpgs,testing$mpg)
R2(predictedMpgs,testing$mpg)

###########################################################################



