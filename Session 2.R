#import a csv file in R
library(utils)
realEstate = read.csv("Sacramentorealestatetransactions (1).csv", header = TRUE,skip=0)

#create a scatterplot
ggplot(realEstate, aes(sq__ft, price)) + geom_point()+theme_bw()
outliers=which(realEstate$sq__ft==0)
cleanRealEstate=realEstate[-outliers,]


ggplot(cleanRealEstate, aes(sq__ft, price)) + geom_point()+theme_bw()


write.csv(cleanRealEstate,"cleanRealEstate.csv")
