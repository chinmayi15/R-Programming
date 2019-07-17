library(utils)
library(dplyr)
library(caret)
library(viridis)
library(plotly)
library(caTools)
#load fifa.csv
fifa = read.csv("fifa.csv", header = TRUE,skip=0, na.strings=c(""," ","NA"))
#select columns of interest
fifa = select(fifa,Age,Club,Potential,Nationality,Wage_in_K,Value_in_Million, Overall,ShortPassing,Dribbling,BallControl,Reactions)
# remove rows with missing values # ShortPassing+Dribbling +BallControl+ Reactions
fifa=fifa[complete.cases(fifa),]

map.world <- map_data('world')

#ggplot(fifa, aes("Club","Wage_in_K"))+geom_point(size = 2, color= "purple", alpha= 0.25)

# Italian Teams Have The Highest Overall Ratings
top_10_overall_clubs <- fifa %>%
  group_by(Club) %>%
  summarise(AverageRating = mean(Overall, na.rm = T)) %>%
  arrange(desc(AverageRating)) %>%
  head(n = 10) %>% pull(Club) 

fifa %>%
  filter(Club %in% top_10_overall_clubs) %>%
  mutate(Top3 = ifelse(Club %in% c("Juventus", "Napoli", "Inter"), "Yes", "No")) %>%
  ggplot(aes(x= reorder(Club,Overall), y= Overall, fill = Top3)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("light blue", "orange")) +
  ggtitle("Teams With The Highest Overall Player Ratings", subtitle = "The average overall rating of the 10 highest rated teams in the game, sorted in descending order") +
  coord_flip() +labs(y = "Overall", x = "Clubs")
  theme_light() +
  theme(legend.position = "none")

# Potential And Overall Talent Converges
fifa %>%
  group_by(Age) %>%
  summarise(Potential = mean(Potential),
            Overall = mean(Overall)) %>%
  ggplot(aes(x= Age)) +
  geom_line(aes(y= Potential), color = "red", size = 1) +
  geom_line(aes(y= Overall), color = "blue", size = 1) +
  annotate("text", x= 30, y=71, label = "Potential meets Overall\nrating at 29 years old", color = "red") +
  ggtitle("Potential vs Overall Rating", subtitle = "The average ratings were taken for each age") +
  theme_light()

# Clubs spendings in wages
fifa %>%
  group_by(Club) %>% summarize(spending = sum(Wage_in_K)) %>% top_n(10) %>%
  ggplot(aes(y = spending, x = reorder(Club, spending))) + 
  geom_bar(stat = "identity", fill='light blue') + coord_flip() + theme_light()+
  labs(y = "Total wages (in thousands ???)", x = "Clubs", title = "Clubs spendings in wages")

# Countries With High Overall Rating of Football Players
map_data('world') %>% group_by(region) %>% summarise() %>% print(n = Inf)  
hf <- left_join(map.world, fifa, by = c('region' = 'Nationality'))
ggplot(data = hf, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Wage_in_K)) +
  labs(title = "Countries With Highly Paid Football Players", caption = "Source:FIFA 2019 Data Set(Kaggle)")+
  theme_light()+labs(y = "Latitude", x = "Longitude")+theme(legend.position = "bottom") +
  scale_fill_viridis(option = "magma",direction = -1,name = "Average Wage",guide = guide_colorbar(direction = "horizontal",barheight = unit(2, units = "mm"),barwidth = unit(50, units = "mm"),draw.ulim = F, title.position = 'top',title.hjust = 0.5,label.hjust = 0.5))
  
  
#################################################################################
# Distribution based on Age
ggplot(data = fifa, aes(Age))+
  geom_histogram(col="orange", aes(fill = ..count..)) +
  ggtitle("Distribution based on Age")+ theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

# Distribution based on Nationality of Players (Top 10 Countries)
countries_count <- count(fifa, Nationality)
top_10_countries <- top_n(countries_count, 10, n)
top_10_country_names <- top_10_countries$Nationality
country <- filter(fifa, Nationality == top_10_country_names)
ggplot(country, aes(x = Nationality)) + 
  geom_bar(col = "red", aes(fill = ..count..)) +
  ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")+
  theme_light()+theme(plot.title = element_text(hjust = 0.5))

# Overall Rating vs age
ggplot(fifa, aes(Age,Overall)) + geom_point(size = 2, color= "purple", alpha= 0.25)+
  ggtitle("Overall Rating vs Age")+theme_light()+theme(plot.title = element_text(hjust = 0.5))

# Overall Rating vs Potential vs Age
mid<-mean(fifa$Age)
ggplot() + geom_point(data = fifa, aes(x = Overall, y = Potential, color = Age))+
  scale_color_gradient2(midpoint = mid, low="cyan", mid="blue",high="red",space ="Lab" )+
  ggtitle("Overall Rating vs Potential vs Age ")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

# or
ggplot() + geom_point(data = fifa, aes(x = Overall, y = Potential, color = Age))+
  scale_color_gradient(low="blue", high="red")+
  ggtitle("Overall Rating vs Potential vs Age ")+theme_gray()+theme(plot.title = element_text(hjust = 0.5))


# potential vs age
avg <- mean(fifa$Potential)
ggplot(fifa, aes(Age, Potential))+geom_bar(stat="identity", fill="steelblue")+
  geom_hline(aes(yintercept = mean(avg)))  +theme_bw()

#Player Potential vs Ball Control
ggplot(fifa, aes(Potential, BallControl))+geom_bar(stat="identity", fill="orange")+
  ggtitle("Player Potential vs Ball Control") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#Player Age vs Ball Control
ggplot(fifa, aes(Age, BallControl))+geom_bar(stat="identity", fill="orange")+
  ggtitle("Player Age vs Ball Control") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Fifa data correlation between BallControl, Dribbling, Potential
fifa2= select(fifa, BallControl,Overall, Age, Dribbling, Reactions,ShortPassing,StandingTackle,SlidingTackle,Potential)
str(fifa2)
library(GGally)
ggpairs(data=fifa2, columns=c(3,2,9), title="Fifa data correlation between BallControl, Dribbling, Potential")


#############################################################################

# Step 1:
training = fifa[seq(1,nrow(fifa),20),]
testing = fifa[seq(2,nrow(fifa),20),]
#Step2: train a random forest model
model=train(Potential~ Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions,training,method="rf")
model$finalModel
summary(training)
#Step3: test the model
predictedPotential=predict(model,testing)
#Step 4: measure accuracy of our model
sum(predictedPotential==testing$Potential)/nrow(testing)

#################################################################################
# Calculation of Accuracy for lm model
set.seed(101)
fifa=na.omit(fifa)
sample = sample.split(fifa, SplitRatio = 0.6)
train <- subset(fifa, sample == TRUE)
test <- subset(fifa, sample == FALSE)
fit <- lm(Potential ~ Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+ Reactions, data = train, na.action = na.omit)
summary(fit)
test_fit <- predict(fit, newdata = test)
test_fit <- round(test_fit,0)
test$Predicted.Potential <- test_fit
Success <- test[c("Potential","Predicted.Potential")]
Success <- Success %>%
  mutate(Difference = Potential - Predicted.Potential )
Success$Accuracy <- ifelse(Success$Difference > 0.10 * Success$Potential,"No",ifelse(Success$Difference < -(0.10 * Success$Potential),"No", "Yes"))
table(Success$Accuracy)

#Calculation of MAE, RMSE and R2
model= train(Potential~Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions,train, method='rf')
predictedPotential=predict(model,test)
MAE(predictedPotential,test$Potential)
RMSE(predictedPotential,test$Potential)
R2(predictedPotential,test$Potential)

#############################################################################
# Prediction and comparision by two or more model
train_control <- trainControl(method="cv", number=5)
m1 = train(Potential~Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions, data=fifa, trControl=train_control, method="lm")
m2 = train(Potential~Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions, data=fifa, trControl=train_control, method="rf")
m3 = train(Potential~Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions, data=fifa, trControl=train_control, method="brnn")
m4 = train(Potential~Age+ Wage_in_K+ShortPassing+Dribbling +BallControl+Reactions, data=fifa, trControl=train_control, method="kknn")
#visualize the accuracies
allModels=resamples(list(LinearRegression=m1,RandomForst=m2,Bayesian=m3, KNearest=m4))
bwplot(allModels,scales=list(relation="free"))


