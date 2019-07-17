# check availability of campsites on Recreation.gov
library(RSelenium)
library(XML)

################### Inputs ##############################
parkIDs=c(232447,232450,232449)
campNames=c("upper Pines","lower Pines","north Pines")
totalAvailabilities=NULL
startDate="07/07/2019"
endDate="07/10/2019"
#########################################################

numberofDays=as.numeric(as.Date(endDate, format="%m/%d/%Y")-as.Date(startDate, format="%m/%d/%Y"))
#binman::list_versions("chromedriver") 
rD=rsDriver(browser = "chrome", chromever = "74.0.3729.6",port=4549L)
remDr=rD$client
for (parkID in parkIDs){
  print(parkID)
url=paste("https://www.recreation.gov/camping/campgrounds/",parkID,sep="")
remDr$navigate(url)
webElem=remDr$findElement(using="name","startDate")
webElem$sendKeysToElement(list(startDate))
webElem=remDr$findElement(using="name","endDate")
webElem$sendKeysToElement(list(paste(endDate,"\n",sep="")))
# click somewhere on the page
webElem=remDr$findElement(using="xpath","//*[@id='page-body']/div[2]/div/div/div[2]/div/section/div/div[1]/div/div")
webElem$clickElement()
# click on check availabilty
webElem=remDr$findElement(using="xpath","//*[@id='page-body']/div[2]/div/div/div[2]/div/section/div/div[2]/div[2]/a[2]")
webElem$clickElement()
# click on load more
x_path="//*[@id='rec-campground-availability-main']/div[2]/div[4]/div[2]/div/button"
Sys.sleep(1)
while (length(remDr$findElements(using='xpath', x_path))!=0) {
webElem=remDr$findElement(using="xpath",x_path)
webElem$clickElement()
# scroll down the page to get "Load More" visible
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
}
# parse the whole table of availabilities
doc <- htmlParse(remDr$getPageSource()[[1]])
results=readHTMLTable(doc)
availabilty=results$`availability-table`
# check the number of available days
totalAvailabilities=c(totalAvailabilities,length(which(availabilty[,3:(2+numberofDays)]=="A")))
}
# close the browser connection
rD$server$stop()
print(paste(campNames[1],": ",totalAvailabilities[1],"   ",
            campNames[2],": ",totalAvailabilities[2],"   ",
            campNames[3],": ",totalAvailabilities[3]))





