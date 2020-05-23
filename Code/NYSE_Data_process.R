library(TSA)
library(forecast)
library(tseries)
library(zoo)
library(FinTS)
library(ggplot2)
library(timeDate)
library(timeSeries)
library(fGarch)
library(parallel)
library(rugarch)

library(pastecs)

  
NYSEcsv <- read.csv(file="NYSE.csv", header=TRUE, sep=",")
NYSEcsv<-NYSEcsv[,2]
NYSE <- ts(NYSEcsv, frequency =1, start =as.Date("1984-02-07"))
class(NYSE)
par(mfrow=c(1,1))
plot(NYSE)

#basic information of the data
stat.desc(NYSE)

  
x=c(1:6625)
#fit the data, and get the coefficients
fit <- lm(log(NYSE) ~ x+I(x^2))
#get transformed data
NYSE11<-NYSE-exp(3.482+6.837*10^(-4)*x-1.793*10^(-8)*x^2)
plot(NYSE11)

#Ljung-Box white noise test to the transformed data
Box.test(NYSE11, type="Ljung-Box")#

#check if the transformed data is stationary
adf.test(NYSE11) 