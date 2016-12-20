library(ggplot2)
library(googleVis)
library(forecast)
library(lubridate)
library(zoo)

publisher_list<-read.table("./Data/publisher.txt")
publisher_list$V1<-as.character(publisher_list$V1)
publishers<-publisher_list$V1


vgsales <- read.csv("./Data/vgsales.csv")
vgsales<-vgsales[!vgsales$Year==2020,]
vgsales<-vgsales[!vgsales$Year==2017,]
  
vgsales_nintendo<-read.csv("./Data/nintendo.csv")
  
year_country_sales<-read.csv("./Data/year_country_sales.csv")
