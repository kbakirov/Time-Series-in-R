
## Importing packages
library(readr)
library(plyr)
library(astsa)
library(ggplot2)
library(xts)
library(forecast)
library(fGarch)
library(fpp)
library(tidyverse)
library(Metrics)
library(knitr)

getwd()
setwd("C:/Users/Kuanysh/Documents/GitHub/Time-Series-in-R/Exam")
data <- read.csv("C:/Users/Kuanysh/Documents/GitHub/Time-Series-in-R/Exam/Elec-train.csv")

#1.Preprocessing

#Rename
names(data)[2]<-"Power"
names(data)[3]<-"Temperature"

#Date format
data$Timestamp <- as.POSIXct(data$Timestamp, format ="%m/%d/%Y %H:%M", tz = "GMT")
power.ts <- ts(data$Power, frequency = 96)
data$time <- as.numeric(time(power.ts))
temperature.ts <- ts(data$Temperature, frequency = 96)

#Plot
autoplot(power.ts)+
  ggtitle('Power consumption per day')+
  xlab('Days')+
  ylab('Power')

autoplot(temperature.ts)+
  ggtitle('Temperature per day')+
  xlab('Days')+
  ylab('Temperature')


#2.Splitting data
nvaldays <- 3

test.power <- tail(data$Power, 96)
full.train.power <- head(data$Power, -96)
train.power <- head(full.train.power, -nvaldays*96)
val.power <- tail(train.power,nvaldays*96)


val.time <- tail(as.numeric(time(ts(full.train.power, frequency = 96))), nvaldays*96)

plot(ts(train.power, frequency = 96),xlim=c(0,50))
par(new=TRUE)
lines(val.time, val.power, col="red", xlim=c(0,50))

#3.Forecast

# simple ES with only alpha
Power<-ts(val.power, frequency = 96)
plot(Power,col="red")
SES=HoltWinters(Power,alpha=NULL,beta=FALSE,gamma=FALSE)
p1<-predict(SES,n.ahead=nvaldays*96)
par(new=TRUE)
plot(ts(as.numeric(p1),frequency = 96),col=3,ann=FALSE,axes=FALSE)
rmse(val.power, as.numeric(p1))


# full ES with alpha beta gamma
#plot(ts(val.power, frequency = 96),col="red")
SES=HoltWinters(Power,alpha=NULL,beta=NULL,gamma=NULL)
p1<-predict(SES,n.ahead=nvaldays*96)
par(new=TRUE)
plot(ts(as.numeric(p1),frequency = 96),col=4,ann=FALSE,axes=FALSE)
rmse(val.power, as.numeric(p1))


