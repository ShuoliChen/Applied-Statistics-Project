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

NYSEcsv <- read.csv(file="NYSE.csv", header=TRUE, sep=",")
NYSEcsv<-NYSEcsv[,2]
NYSE <- ts(NYSEcsv, frequency =1, start =as.Date("1984-02-07"))
class(NYSE)
par(mfrow=c(1,1))
plot(NYSE)

###########

x=c(1:6625)
fit <- lm(log(NYSE) ~ x+I(x^2))
NYSE11<-NYSE-exp(3.482+6.837*10^(-4)*x-1.793*10^(-8)*x^2)

#Find the best ARIMA model
J<-5
K<-5
AIC1<-matrix(0, J+1, K+1)
for (j in 0:J)
{
  for (k in 0:K)
  {
    fit<- arima(NYSE11, order=c(j,0,k)) 
    AIC1[j+1,k+1]<-fit$aic
  }
}

#The corresponding coefcients of ARMA(5,4)
fit1<- arima(NYSE11, order=c(5,0,4)) 
Box.test(fit1$residuals, type="Ljung-Box")
plot(fit1$residuals)

#The signifcance test for coefcients of ARMA(5,4)

U1<-fit1$coef/sqrt(diag(fit1$var.coef)) 
pnorm(abs(U1),0,1,lower.tail = FALSE)*2

#The ARCH e???ect of the residual of ARMA(5,4)
NYSE2<-fit1$residuals
ArchTest(NYSE2) 
fit2<-garch(x=NYSE2 , order=c(1,1), trace = FALSE)
summary(fit2)