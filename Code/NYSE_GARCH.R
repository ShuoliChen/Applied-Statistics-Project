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


x=c(1:6625)
fit <- lm(log(NYSE) ~ x+I(x^2))
NYSE11<-NYSE-exp(3.482+6.837*10^(-4)*x-1.793*10^(-8)*x^2)



fit1<- arima(NYSE11, order=c(5,0,4)) 


NYSE2<-fit1$residuals

#The cofcients of GARCH(1,1):
fit2<-garch(x=NYSE2 , order=c(1,1), trace = FALSE)

#White noise test for the residual of GARCH(1,1) model
#The signifcance test for coefcients of GARCH(1,1)
summary(fit2)

#The ARCH effect of the residual of GARCH(1,1)
ArchTest(residuals(fit2)) 
date<-c(1:length(residuals(fit2)))
plot(date,residuals(fit2))

# Predict the residue of ARMA(5,4)
# predict(fit)

# It is not every GARCH(p,q) that works well
fit3<-garch(x=NYSE2 , order=c(2,2), trace = FALSE)
date<-c(1:length(residuals(fit3)))
plot(date,residuals(fit3))



# U1<-fit1$coef/sqrt(diag(fit1$var.coef))
# pnorm(abs(U1),0,1,lower.tail = FALSE)*2
# J<-5
# K<-5
# AIC2<-matrix(0, J+1, K+1)
# for (j in 0:J)
# {
#   for (k in 0:K)
#   {
#     fit<- arima(NYSE2, order=c(j,0,k),method="ML") #用默认方法会时p=1,q=6时的系数落入平稳域之外，因此才有极大似然估计�?.
#     AIC2[j+1,k+1]<-fit$aic
#   }
# }
# fit1<- arima(NYSE2, order=c(10,0,2),method="ML") #AIC比较小，系数全为�?
# Box.test(fit1$residuals, type="Ljung-Box") #因此残差是白噪声
# date<-c(1:length(fit1$residuals))
# plot(date,fit1$residuals) #还是没有吊用
#系数显著性正态检�?
# U1<-fit1$coef/sqrt(diag(fit1$var.coef))
# pnorm(abs(U1),0,1,lower.tail = FALSE)*2
# plot(date,sqrt(abs(fit1$residuals))-sqrt(abs(NYSE2))) #sum反而增大！�?


# AIC<-matrix(NA, 4, 3)
# fit<-garchFit( ~ garch(1,1), NYSE2,trace = FALSE)
# s<-summary(fit)
# AIC[1,1]<-5.263595
# fit<-garchFit(~ garch(1,2),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[1,2]<-5.264550 
# fit<-garchFit( ~ garch(1,3),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[1,3]<-5.265490
# fit<-garchFit( ~ garch(2,1),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[2,1]<-5.263829
# fit<-garchFit( ~ garch(2,2),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[2,2]<-5.263961
# fit<-garchFit( ~ garch(2,3),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[2,3]<-5.264909
# fit<-garchFit( ~ garch(3,1),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[3,1]<-5.264773
# fit<-garchFit( ~ garch(3,2),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[3,2]<-5.264801
# fit<-garchFit( ~ garch(3,3),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[3,3]<-5.264878
# fit<-garchFit( ~ garch(1),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[4,1]<-6.967485
# fit<-garchFit( ~ garch(2),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[4,2]<-6.222778
# fit<-garchFit( ~ garch(3),NYSE2, trace = FALSE)
# s<-summary(fit)
# AIC[4,3]<-5.709118


# NYSE3<-RES
# 
# fit3<-garchFit(~ garch(1,1), NYSE3,trace = FALSE)
# summary(fit3)
# RES1<-fit3@fitted-NYSE3
# plot(date,RES1)
#RES+NYSE2�?-fit2@residuals+NYSE2是一样的！！！说明这个方法并没有让残差收住啊！！！所以要么你从这个模型得出正确的预测方法，那么对NYSE2^2做ARMA

################差分做法�?
# ndiffs(NYSE)
# NYSE11 <- diff(NYSE)
# plot(NYSE11)
# #白噪声检�?
# Box.test(NYSE11, type="Ljung-Box") #不否认是白噪�?
# NYSE4<-NYSE11-mean(NYSE11)
# ArchTest(NYSE4,lags=12) #可见条件异方差性极为显�?
# NYSE5<-NYSE4^2
# J<-5
# K<-5
# AIC3<-matrix(0, J+1, K+1)
# for (j in 0:J)
# {
#   for (k in 0:K)
#   {
#     fit<- arima(NYSE5, order=c(j,0,k),method="ML") #用默认方法会时p=1,q=6时的系数落入平稳域之外，因此才有极大似然估计�?.
#     AIC3[j+1,k+1]<-fit$aic
#   }
# }
# fit1<- arima(NYSE5, order=c(10,0,2),method="ML") #AIC比较小，系数全为�?
# Box.test(fit1$residuals, type="Ljung-Box") #因此残差是白噪声
# date<-c(1:length(fit1$residuals))
# plot(date,fit1$residuals)
# #系数显著性正态检�?
# U1<-fit1$coef/sqrt(diag(fit1$var.coef))
# pnorm(abs(U1),0,1,lower.tail = FALSE)*2
# plot(date,sqrt(abs(fit1$residuals))-abs(NYSE4)) #sum反而增大！�?
# NYSE6<-fit1$residuals^2
# fit2<- arima(NYSE6, order=c(3,0,5),method="ML")
# Box.test(fit2$residuals, type="Ljung-Box") #因此残差是白噪声
# date<-c(1:length(fit2$residuals))
# plot(date,fit2$residuals)
# plot(sqrt(abs(fit2$residuals))-abs(NYSE6))

# fit4<-garchFit(~ garch(1,2), NYSE4 ,trace = FALSE) #这种做法得不到预测�?
# summary(fit4)
# plot(fit4@residuals)
# 
# NYSE5<-fit4@residuals
# ArchTest(NYSE3,lags=12) #虽然说还要ARCH效应，但实际上接下来再建立GARCH，系数都几乎�?0，已经没有必要了
# fit5<-garchFit(~ garch(1,1), NYSE5,trace = FALSE)
# summary(fit5)
# plot(fit5@residuals) 