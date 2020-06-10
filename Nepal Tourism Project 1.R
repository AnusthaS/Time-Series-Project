library(forecast)
library(TSA)
library(tseries)
library(astsa)
setwd("C:/Users/Anustha Shrestha/Documents/RSTA9701")
tourism = read.csv("NepalTourism.csv")
touristArrivals = tourism[,3]#timeseries data
tourist = ts(touristArrivals, frequency = 12, start = c(1992,1))#just for plotting

par(mfrow=c(3,1))
par(mfrow=c(1,1))
#plotting the time series data
plot (tourist, ylab = "Number of Tourists")
acf(touristArrivals)
pacf(touristArrivals)
#Note that it seems to have some sort of trend
#The variance seems to be increasing after 2010
#Slow period between 2001 to 2008
#dip in 2015 (earthquake)
#presence of seasonality because tourism data

#Detrending
fit = lm(touristArrivals~time(touristArrivals), na.action=NULL) # regress touristArrivals on time
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
acf(resid(fit), 100, main="acf detrended")
pacf(resid(fit), 100, main="pacf detrended")

#Try differencing because ACF does not tail off
#Sample acf and pacf of differenced series
dtourist <- diff(touristArrivals, 1)
par(mfrow=c(1,1))
plot.ts(dtourist, ylab = "Number of Tourists: Differenced")
acf(dtourist,100, main="ACF : Differenced")
pacf(dtourist,100, main="PACF: Differenced")
#We were able to get rid of the trend and the mean seems to be stationary
#the variance is not stationary so apply variance stabalizing tools


#Box-Cox
BoxCox.lambda(touristArrivals)

#0.035 which is close to 0, so we can use log transformation 
# log of tourist arrivals and sample acf and pacf of detrended series
# log of tourist arrivals and sample acf and pacf of detrended series
logtourist = log(touristArrivals)
dfit = lm(logtourist~time(logtourist), na.action=NULL) # regress touristArrivals on time
par(mfrow=c(1,1))
plot(resid(dfit), type="o", main="detrended")
acf(resid(dfit), 100, main="ACF log and detrended")
pacf(resid(dfit), 100, main="PACF log and detrended")


#0.035 which is close to 0, so we can use log transformation 
# log of tourist arrivals and sample acf and pacf of differenced series
# log of tourist arrivals and sample acf and pacf of differenced series
logtourist = log(touristArrivals)
dlogtourist <- diff(logtourist, 1)
par(mfrow=c(3,1))
plot.ts(dlogtourist, ylab = "log of Number of Tourists")
acf(dlogtourist,100, main="ACF")
pacf(dlogtourist,100, main="PACF")

#df tests / unit root tests
adf.test(dlogtourist,k=0) #df test
adf.test(dlogtourist) #adf test
#p-value for both test is 0.01 so we can say that the process is stationary

#EACF 
eacf(dlogtourist)
#some candidate models are ARIMA(1,1,1), (2,1,1), (2,1,0), (1,1,0), (12, 1, 0), (6,1,0)

#Fitting models
arima111<-arima(dlogtourist, order=c(1,0,1)) #arima(1,1,1)
arima211<-arima(dlogtourist, order=c(2,0,1)) #arima(2,1,1)
arima210<-arima(dlogtourist, order=c(2,0,0)) #arima(2,1,0)
arima110<-arima(dlogtourist, order=c(1,0,0)) #arima(1,1,0)
arima1210<-arima(dlogtourist, order=c(12,0,0)) #arima(12,1,0)
arima1211<-arima(dlogtourist, order=c(12,0,1)) #arima(12,1,1)
arima1212<-arima(dlogtourist, order=c(12,0,2)) #arima(12,1,1)
arima610<-arima(dlogtourist, order=c(6,0,0)) #arima(6,1,0)
arima612<-arima(dlogtourist, order=c(6,0,2)) #arima(6,1,0)

#Model Selection
AIC(arima111); BIC(arima111);
AIC(arima211); BIC(arima211);
AIC(arima210); BIC(arima210);
AIC(arima110); BIC(arima110);
AIC(arima1210); BIC(arima1210);
AIC(arima1211); BIC(arima1211);
AIC(arima1212); BIC(arima1212);
AIC(arima610); BIC(arima610);
AIC(arima612); BIC(arima612);

#Model diagnostics
sarima(logtourist, 1,1,1)
sarima(logtourist, 2,1,1)
sarima(logtourist,2,1,0)
sarima(logtourist,1,1,0)
sarima(logtourist,12,1,0)
sarima(logtourist,12,1,1)
sarima(logtourist,12,1,2)
sarima(logtourist,6,1,0)
sarima(logtourist,6,1,2)

#Forecasting
logtourist = ts(logtourist, frequency =1)
nobs = length(logtourist) 
#arima (1,1,1)
arima111<-arima(logtourist, order=c(1,1,1), xreg=1:nobs) 
sarima(logtourist, 1,1,1)
fore111 = predict(arima111, 24, newxreg = (nobs+1): (nobs +24))
par(mfrow= c(1,1))
ts.plot(logtourist, fore111$pred, col=1:2, main="arima (1,1,1)")
sarima.for(logtourist, 24,1,1,1, newxreg = (nobs+1): (nobs+24))
sarima.for(dlogtourist, 24,1,0,1, newxreg = (nobs+1): (nobs+24))

#arima (2,1,1)
arima211<-arima(logtourist, order=c(2,1,1), xreg=1:nobs) #arima(2,1,1)
sarima(logtourist, 2,1,1)
par(mfrow= c(1,1))
fore211 = predict(arima211, 24, newxreg = (nobs+1): (nobs +24))
ts.plot(logtourist, fore211$pred, col=1:2, main="arima (2,1,1)")
sarima.for(logtourist, 24,2,1,1, newxreg = (nobs+1): (nobs+24))
sarima.for(dlogtourist, 24,2,0,1, newxreg = (nobs+1): (nobs+24))


#arima (12,1,1)
arima1211<-arima(logtourist, order=c(12,1,1), xreg=1:nobs) #arima(12,1,1)
sarima(logtourist, 12,1,1)
par(mfrow= c(1,1))
fore1211 = predict(arima1211, 24, newxreg = (nobs+1): (nobs +24))
ts.plot(logtourist, fore1211$pred, col=1:2, main="arima (12,1,1)")
sarima.for(logtourist, 24,12,1,1, newxreg = (nobs+1): (nobs+24))
sarima.for(dlogtourist, 24,12,0,1, newxreg = (nobs+1): (nobs+24))

#Restricted Arima(12,1,1)
RArima1211<- arima(logtourist,c(12,1,1),fixed=c(0,0,NA,0,0,NA,0,0,NA,0,0, NA, NA, 0), xreg=1:nobs)
sarima(logtourist,12,1,1,fixed=c(0,0,NA,0,0,NA,0,0,NA,0,0, NA, NA, 0))
par(mfrow= c(1,1))
foreR1211 = predict(Rarima1211, 24, newxreg = (nobs+1): (nobs +24))
ts.plot(logtourist, foreR1211$pred, col=1:2, main="Restricted ARIMA (12,1,1)")
sarima.for(logtourist, 24,12,1,1, fixed=c(0,0,NA,0,0,NA,0,0,NA,0,0, NA, NA, 0),newxreg = (nobs+1): (nobs+24))

#Model comparison using 9 period prediction against real data
whitenoise <-arima(logtourist, order=c(0,1,0), xreg=1:nobs)
sarima(logtourist, 0,1,0)
sarima.for(dlogtourist, 24, 0,1,0, newxreg = (nobs+1):(nobs+24))
true <- c(91793, 124421, 124697, 106136, 71640, 68782, 65749, 89382, 89078)
logtrue = log(true)
testforeR1211 = predict(RArima1211, 9, newxreg = (nobs+1): (nobs +9))
testfore1211 = predict (arima1211, 9, newxreg = (nobs+1): (nobs+9))
testforeWN = predict (whitenoise, 9, newxreg = (nobs+1): (nobs+9))
RArimaSSE = sum((testforeR1211$pred - logtrue)^2)
ArimaSSE = sum((testfore1211$pred - logtrue)^2)
WNSSE = sum((testforeWN$pred - logtrue)^2)
R.MSFE = RArimaSSE / 9
A.MSFE = ArimaSSE / 9
RArimaSSE
ArimaSSE
WNSSE