#Load this data set and build a ts object.
data=read.csv(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/varicelle.csv")
plot(data$x)

varicelle<-ts(data$x,start=c(1931,1),end=c(1972,6),freq=12)
plot(varicelle)

#Plot the time series.
library(forecast)
library(ggplot2)
autoplot(varicelle) +
  ggtitle('Number of varicella per months')+
  xlab('year')+
  ylab('Number of varicella')


#Is there somme trend, seasonal pattern or cyclic pattern?
ggseasonplot(varicelle,year.labels= TRUE,year.labels.left=TRUE)

#What is the mean mensual number of varicella?
mean(varicelle)


#Plot the correlogram and interpret it.
tmp=acf(varicelle,type="cor",plot = FALSE)
tmp$acf[1:3,1,1]

plot(tmp)

#Plot the seasonal plot.
serie=cos(2*pi/12*(1:100))
par(mfrow=c(1,2))
plot(ts(serie))
acf(serie)


#Compute the annual numbers of varicella and plot them from 1931 to 1972.
varicelle_annual<-ts(data$x,start=c(1931,1),end=c(1972,1),freq=12)
plot(varicelle_annual)


#What can you say from this two latter graphs?
