library(utils)
# open the UK accident data set
ukaccidents2 = read.csv("accidents_2009_to_2011.csv",header = TRUE,skip=0, na.strings=c(""," ","NA"))
ukaccidents3 = read.csv("accidents_2012_to_2014.csv", header = TRUE,skip=0, na.strings=c(""," ","NA"))
# merge the two data sets
ukaccidents =merge(ukaccidents2,ukaccidents3,all=TRUE,sort=FALSE)
#select columns of interest
library(dplyr)
ukAccident = select(ukaccidents, Accident_Index, Accident_Severity,	Number_of_Vehicles,Number_of_Casualties,Day_of_Week, Road_Type, Speed_limit, Light_Conditions, Weather_Conditions,Road_Surface_Conditions, Urban_or_Rural_Area, Year)


#remove missing values
#ukAccident=ukAccident[complete.cases(ukAccident),]
#is.na(ukAccident)
ukAccident=na.omit(ukAccident)


ukAccident$Accident_Severity =as.factor(ukAccident$Accident_Severity)
ukAccident$Speed_limit =as.factor(ukAccident$Speed_limit)

#correlation
ggplot(ukAccident, aes(Accident_Severity,Speed_limit)) + 
  geom_point(size = 6, color= "purple", alpha= 0.4)+ theme_bw()+
  ggtitle("Correlation between Accident_Severity vs Speed_limit")+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ukAccident, aes(Accident_Severity, ..count..)) +
  geom_bar(aes(fill = Speed_limit),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Correlation between Speed_limit vs Accident_Severity")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

 # Accident_Severity vs Road_Type
ggplot(ukAccident, aes(Accident_Severity, ..count..)) +
  geom_bar(aes(fill = Road_Type),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Correlation between Road_Type vs Accident_Severity")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)


# Accident_Severity vs Light_Conditions
ggplot(ukAccident, aes(Accident_Severity, ..count..)) +
  geom_bar(aes(fill = Light_Conditions),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Accident_Severity and Light_Conditions")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

# Accident_Severity vs Weather_Conditions
ggplot(ukAccident, aes(Accident_Severity, ..count..)) +
  geom_bar(aes(fill = Weather_Conditions),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Accident Severity and Weather Conditions")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

# Accident_Severity vs Road_Surface_Conditions
ggplot(ukAccident, aes(Accident_Severity, ..count..)) +
  geom_bar(aes(fill = Road_Surface_Conditions),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Accident_Severity and Road_Surface_Conditions")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

ukAccident$Day_of_Week =as.factor(ukAccident$Day_of_Week)
ukAccident$Accident_Severity =as.factor(ukAccident$Accident_Severity)
# Accident_Severity vs Day_of_Week
ggplot(ukAccident, aes(Day_of_Week, ..count..)) +
  geom_bar(aes(fill = Accident_Severity),stat="count", position = "dodge")+
  theme_bw()+
  ggtitle("Correlation between Day_of_Week vs Accident_Severity")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

ggplot(ukAccident,aes(Road_Type,Accident_Severity))+geom_point()

#To predict severity of the accident
#convert the target feature which is numerical to as char because it is classification model
ukAccident$Accident_Severity =as.factor(ukAccident$Accident_Severity)

training=ukAccident[seq(1,nrow(ukAccident),10),]
testing=ukAccident[seq(2,nrow(ukAccident),10),]
library (caret)

train_control <- trainControl(method="boot", number=1)
#m1 = train(Ozone~., data=myData, trControl=train_control, method="lm")
m2 = train(Accident_Severity~., data=ukAccident, trControl=train_control, method="rf")
################################################################

  

# Step 1: split the data to train and test
training = ukAccident[seq(1,nrow(ukAccident),100),]
testing = ukAccident[seq(2,nrow(ukAccident),100),]

#Step2: train a k-nearest classifier
model=train(Accident_Severity~.,training,method="svm")
model$finalModel

#Step3: test the model
predictedSeverity=predict(model,testing)

#Step 4: measure accuracy of our model
sum(predictedSeverity==testing$Accident_Severity)/nrow(testing)


