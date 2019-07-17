library(RSelenium)
RSelenium::rsDriver()
binman::list_versions("chromedriver")
RSelenium::rsDriver(browser = "chrome", chromever = "75.0.3770.90",port=4568L)


rD=rsDriver(browser = "chrome", chromever = "75.0.3770.90",port=4568L)

remDr=rd$client
remDr$navigate