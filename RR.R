rm(list=ls())

install.packages("fma")
install.packages("qqvases")
install.packages(“ggplot2“)
install.packages("sarima")

library(fma)
library(fpp)
library(forecast)
library(tseries)
library(qqvases)
library(ggplot2)
library(sarima)

########################################################################## 1.Plot
##trend,seasonality,cyclic,variance is stable or not
##plot positions
par(mfrow=c(1,1))

plot(x,xlab="",ylab="expression("CO"[2])",type="l")
ts.plot(x)

##line type, 2 is dash
plot(x,col="blue",lty=2)

##图上加线,x是数据
lines(x)

##y轴范围
plot(x,type="l",ylim=c(-20,20))

plot(price,priceL1,xy.lines = FALSE,xy.labels = FALSE)

## plot point
plot(residuals(x),type="p")

hist(wn)

##花瓶
qq_plot(eggs)

## gives an idea of the shape of the seasonality component
boxplot(beer~cycle(beer))

legend("topleft",c("Observed","SES","DES","Holt-Winters (multiplicative)"),col=c("red","green","gray","blue")
       
       ##两个表合成一个表
       plot(cbind(cpi,cost))
       ########################################################################## 2.transformation
       ## if variance is not stable but can be transformed
       plot(sqrt(airpass), ylab="")
       plot(airpass^(1/3), ylab="")
       plot(log(airpass), ylab="")
       plot(-1/airpass, ylab="")
       plot(BoxCox(airpass,lambda=0.1),ylab="")
       ##BoxCox.lambda function choose a value of lambda for you.
       lambda <- BoxCox.lambda(airpass)
       ########################################################################## 3.ARIMA
       y<-arima(x,c(2,0,0))
       #x是decompose
       y<-arima(na.omit(x$random),c(0,0,1))
       
       ##sARIMA
       y<-auto.arima(x,seasonal=TRUE)
       ##sARIMA(0,0,0)(0,1,1)
       y<-arima(x,c(0,0,0),c(0,1,1))
       
       y<-auto.arima(x, approximation=FALSE, stepwise=FALSE)
       
       ##xreg: a vector or matrix of external regressors, which must have the same number of rows as y.
       ##lambda: Box-Cox transformation parameter if used
       y<- auto.arima(cost, xreg=a, lambda=0))
## Forecast a using naive:ARIMA(0,0,0)(0,1,0)m model,取平均值带到model里做参数
forecast_a<- naive(a,h=24)$mean
forecast<- forecast(y, xreg=a)

# 拟合值
z<-fitted.values(y)
########################################################################## 4.choose better model
#############################acf,pacf
acf(beer,lag.max=40,type = 'correlation',main="")
pacf(beer,ylab="PACF")

tsdisplay(beer)
#############################AIC,BIC,probablity model
AIC(x)
BIC(x)
#############################ME,RMSE,MAE
##生成竖着表格
tab<-data.frame(a=c(4,3,5,7,5,6,4),b=c(4,4,3.5,4.25,5.73,5.26,5.73))
tab$c<-tab$a-tab$b
tab$d<-(tab$a-tab$b)^2
MAE<-sum(tab$c)/7
RMSE<-sqrt(sum(tab$d)/7)

y<-sqrt(sum((forecast-x)^2)/12)
##取绝对值
abs(x)
########################################################################## 5.residuals
mod1<-arima(diff(x),order=c(2,0,0))
r1<-residuals(mod1)
par(mfrow=c(2,2))
plot(na.omit(r1))
qqnorm(as.vector(r1),main="")
qqline(r1,distribution=qnorm)
hist(r1,main="")
acf(na.omit(r1))
#############################stationarity test
## Stationarity Test: Augmented Dickey-Fuller test
##看p-value,not allow missing values,use na.omit to remove
adf.test(na.omit(decompx$random))
Box.test(na.omit(decompx$random),type="Ljung-Box")
kpss.test(na.omit(decompx$random))
########################################################################## 6.forecast
##forecast 12 months
z<-forecast(y,h=12)

##split a time series to Cross-validation(special case of pseudo out of sample forecast)
x<- window(beer, end=c(1994,09))
y<- window(beer, start=c(1994,10))





########################################################################## trend
## Removing the trend: Least Squares Estimation
t=1:length(co2)
x<-lm(co2~t+I(t^2))
plot.ts(x$residuals,ylab='residuals')

## Removing the trend: Moving Average
plot(co2-ma(co2,12))

plot.ts(co2)
lines(ma(co2,12),col="red")

## Removing the trend: Differencing
plot(diff(co2))
## diff 12: remove seasonality
plot(diff(co2，12))

plot(co2-lag(co2,-1))
########################################################################## season
## Removing the seasonality
x<-decompose(co2)
## 乘法形式
x<-decompose(x,type="multiplicative")
## 乘法形式取log和加法形式一样
x<-decompose(log(x))
## seasonal effect in January
decompose(x)$seasonal[1]





autoplot, ggseasonplot, ggmonthplot, gglagplot, ggAcf, ggtsdisplay
##########################################################################描述数据
## get a description of the dataset
?beer
## the third value of this time series
beer[3]
start(beer)
end(beer)
time(beer)
## the number of samples per unit time
frequency(beer)
## positions in the cycle
cycle(beer)
########################################################################## 乱七八糟操作
## column bind
x<- cbind(price=price, priceL1=priceL1)
## print out the first 5 rows
x[1:5,]

#############################制作time series
white_noise<-rnorm(360)
##编一个time series, trend:c(1:360)/10，1-36被10除，seasonality:crep1-12,30次
wnTS<-white_noise+c(1:360)/10+rep(1:12,30)
##Create a time series,observed monthly,begins on January 1900,
wnTSts<-ts(wnTS,start = c(1900,1),frequency = 12)

## 编一个AR(1)：yt = zt + p.yt-1 ���zt z,parameter less than 1, stationary,stationary 可以用AR Modellt
y <- vector(length=500)
p<- 0.50
z <- rnorm(500)
y[1] <- 5
for(i in 2:500) {y[i] <- p*y[i-1] + z[i]}

## 编一个MA(2)：yt = zt + theta[1].zt-1+theta[2].zt-2
y <- vector(length=500)
theta <- c(0.5,0.5)
z <- rnorm(500)
y[1] <- z[1]
y[2] <- z[2]+theta[1]*z[1]
for(i in 3:500) {y[i] <- z[i]+theta[1]*z[i-1]+theta[2]*z[i-2]}

##编一个ARMA(2,2)
y<-arima.sim(n = 150, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),sd = sqrt(0.1796))

##编一个SARIMA
x<-sim_sarima(200,model =list(nseasons=12,iorder=0,siorder=0,ar=0,ma=0,sar=0.9,sma=0))

#############################naive method to forecast
fbeer<-mean(beer)+decompose(beer)$seasonal[9:12]
plot.ts(c(beer,fbeer))

data(co2)





########################################################################## Simple/Double Exponential Smoothing/Holt-Winters 
## 1.Simple Exponential Smoothingts: with no or little trend & no seasonality
y<- ses(x, alpha=0.5, h=12))

## 2.Double exponential Smoothing (Holt’s Linear Method): ts with trends but no seasonality
y<- holt(x,h=12)

## 3.Holt-Winters method: ts with trends & seasonality
y<- hw(x)
##because of the increasing size of the fluctuations and increasing variation with the trend.The seasonality looks like it behaves proportionally, therefore multiplicatively.
y<- hw(y, seasonal='multiplicative')

## check residual,用RMSE, MAE,不能用AIC BIC, 因为不是 probility model 
par(mfrow=c(2,2))
plot(as.vector(y$fitted),as.vector(y$residuals), xlab="Fitted values",ylab="Residuals")
hist(y$residuals,main="")
plot(y$residuals,ylab="Residuals")
acf(y$residuals)
########################################################################## Dynamic regression models
################ 1.static model 同期

################ 2.linear regression static model, month as dummy variables, static with trend and seasonality
##原有数据重复了几次
Jan<-c(rep(c(1,0,0,0,0,0,0,0,0,0,0,0),4),1,0,0,0,0,0,0,0)
Feb<-c(rep(c(0,1,0,0,0,0,0,0,0,0,0,0),4),0,1,0,0,0,0,0,0)
Mar<-c(rep(c(0,0,1,0,0,0,0,0,0,0,0,0),4),0,0,1,0,0,0,0,0)
month<-factor(cycle(beer))
cbind(beer,month)
mod1<-lm(beer~month)
##predict的x值
newdata<-data.frame(month=factor(c(9:12,1:8)))
##predict的y值
newpreds<-ts(predict(mod1,newdata),start=c(1995,09),frequency=12)
##type n看不见，为了x轴长度对
plot(ts(c(beer,newpreds),start=c(1991,01),frequency = 12),type="n",ylab="")
lines(beer)
lines(newpreds,col="blue",lwd=2)

################ 3.Regression with ARIMA errors,uncorrelated error term
##Durbin-Watson test: whether error is significantly correlated, The autocorrelation of the disturbance is 0.
dwtest(mod1,alternative = "two.sided")
##Box.test, The data are independently distributed, i.e. the autocorrelation coefficients are all zero.
##Box.test(res1, fitdf=5, lag=24, type="Lj")

z<- auto.arima(y, xreg=x)
tsdisplay(residuals(z),main="")
forecast <- forecast(z,xreg=rep(mean(x),8), h=8)
plot(forecast,main="")

################ 4.Regression with lagged predictors
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")
head(Advert,6)
##change number of regressor，找最小AIC
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
AIC(fit1)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
AIC(fit2)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
AIC(fit3)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)
AIC(fit4)
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="", ylab="Quotes")

########################################################################## VAR:vector autoregression
##predictor variables are influenced by the forecast variable
########################################################################## ARCH:autoregressive conditional heteroscedastic models
##variance term have its own random walk不是一直变大或变小
########################################################################## GARCH:generalised ARCH
##allow non-linear,non-stationary processess be applied to variance terms

