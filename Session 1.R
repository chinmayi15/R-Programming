# load the ggplot library
library(ggplot2)
myData = mpg

# create a histogram for the hwy column
ggplot(myData, aes(hwy))+geom_density( color="red", fill="green")+
  ggtitle("My first title")+ xlab("highway mpg")+ylab("") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# scater plot for hwy and cty
ggplot(myData, aes(cty,hwy)) + geom_point(size = 6, color= "purple", alpha= 0.4)+
  theme_bw()


#create duplicate data to get large dataset
newMPG=rbind(mpg,mpg,mpg,mpg,mpg,mpg)
newMPG$hwy=newMPG$hwy+runif(nrow(newMPG),min=0,max=1)

# How to compare the distributions of hwy mpgs for different cars?
ggplot(newMPG, aes(class, hwy, color=class))+geom_point()+theme_bw()

#jitter plot
ggplot(newMPG, aes(class, hwy, color=class))+geom_jitter()+theme_bw()

#violin plot
ggplot(newMPG, aes(class, hwy, fill=class))+geom_violin()+theme_bw()

#boxplot
ggplot(newMPG, aes(class, hwy, fill=class))+geom_boxplot()+theme_bw()
