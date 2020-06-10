library(forecast)
library(TSA)
library(tseries)
library(astsa)
library(lmtest)
setwd("C:/Users/Anustha Shrestha/Documents/RSTA9701")
tourism = read.csv("NepalTourism.csv")
touristArrivals = tourism[,3]#timeseries data
tourist = ts(touristArrivals, frequency = 12, start = c(1992,1))#data as time series

################################# SIMPLE ARIMA MODEL (PROJECT 1)############################
#plotting the time series data
plot (tourist, ylab = "Number of Tourists", main= "Figure1: Monthly Tourist Arrivals in Nepal (1992 - 2018)")
acf(touristArrivals, main = "Figure 2: ACF and PACF for Tourist Arrivals")
pacf(touristArrivals)

#Sample acf and pacf of differenced series
dtourist <- diff(touristArrivals, 1)
par(mfrow=c(1,1))
plot.ts(dtourist, ylab = "Number of Tourists: Differenced", main="Tourist Arrivals - Log and Differenced")
acf(dtourist,100, main="ACF : Differenced")
pacf(dtourist,100, main="PACF: Differenced")

# log of tourist arrivals and sample acf and pacf of differenced series
logtourist = log(touristArrivals)
dlogtourist <- diff(logtourist, 1)
par(mfrow=c(1,1))
tsdlogtourist<-ts(dlogtourist, frequency = 12, start=c(1992,2))
par(mfrow=c(3,1))
plot.ts(tsdlogtourist, ylab = "log of Number of Tourists", main="Figure3: Tourist Arrivals - Log and Differenced")
acf(dlogtourist,100, main="Figure 4: ACF and PACF of Log and Differenced Tourist Arrivals")
pacf(dlogtourist,100, main="PACF")

#df tests / unit root tests
adf.test(dlogtourist,k=0) #df test
adf.test(dlogtourist) #adf test
#p-value for both test is 0.01 so we can say that the process is stationary

#EACF 
eacf(dlogtourist)
#some candidate models are ARIMA(1,1,1), (2,1,1), (2,1,0), (1,1,0), (12, 1, 0), (6,1,0)

#Fitting models
arima1211<-arima(dlogtourist, order=c(12,0,1)) #arima(12,1,1)

#Model Selection
AIC(arima1211); BIC(arima1211);

#Model diagnostics
sarima(logtourist,12,1,1)

#Forecasting
#arima (12,1,1)
arima1211<-arima(logtourist, order=c(12,1,1), xreg=1:nobs) #arima(12,1,1)
sarima(logtourist, 12,1,1)
par(mfrow= c(1,1))
fore1211 = predict(arima1211, 24, newxreg = (nobs+1): (nobs +24))
for1211 = predict(arima1211, 24, newxreg = (nobs+1): (nobs +24))
ts.plot(logtourist, fore1211$pred, col=1:2, main="arima (12,1,1)")
ts.plot(ts.union(ts(logtourist), ts(for1211$pred)), col=1:2, main="arima (12,1,1)")
ts.plot(ts.union(ts(x), ts(xlag)), gpars=list(col=rainbow(2)))

sarima.for(logtourist, 24,12,1,1, newxreg = (nobs+1): (nobs+24))
sarima.for(dlogtourist, 24,12,0,1, newxreg = (nobs+1): (nobs+24))

#Restricted Arima(12,1,1)
foreR1211 = sarima.for(logtourist, 24,12,1,1, fixed=c(0,0,NA,0,0,NA,0,0,NA,0,0, NA, NA, 0), newxreg = (nobs+1): (nobs+24))
foreWN = sarima.for(logtourist, 24,0,1,0, newxreg = (nobs+1): (nobs+24))

#Model comparison using 9 period prediction against real data
whitenoise <-sarima(logtourist, order=c(0,1,0), xreg=1:nobs)
sarima(logtourist, 0,1,0)
sarima.for(dlogtourist, 24, 0,1,0, newxreg = (nobs+1):(nobs+24))
true <- c(91793, 124421, 124697, 106136, 71640, 68782, 65749, 89382, 89078)
logtrue = log(true)
testforeR1211 = sarima.for(logtourist, 9,12,1,1, fixed=c(0,0,NA,0,0,NA,0,0,NA,0,0, NA, NA, 0), newxreg = (nobs+1): (nobs+9))
testfore1211 = sarima.for(logtourist, 9,12,1,1,newxreg = (nobs+1): (nobs+9))
testforeWN = sarima.for(logtourist, 9,0,1,0, newxreg = (nobs+1): (nobs+9))

RArimaSSE = sum((testforeR1211$pred - logtrue)^2)
ArimaSSE = sum((testfore1211$pred - logtrue)^2)
WNSSE = sum((testforeWN$pred - logtrue)^2)
RArimaSSE
ArimaSSE
WNSSE

#Plotting out the predicted vs. 9 period
truets <- ts(true, start = c(2019,2), frequency=12)
Rarima1211P <-exp(testforeR1211$pred)
ts.plot(cbind(truets, ts(Rarima1211P, start=c(2019,2), frequency=12)), gpars = list(lty =c(1:3),col=1:2, ylab="Number of Tourist Arrivals", main="Figure12: Forecast against Real Tourist Arrivals for First 9 months of 2019"))

###########################SEASONAL MODEL#####################################
#Differencing
logtourist = log(touristArrivals)
dlogtourist <- diff(logtourist, 1)
par(mfrow=c(1,1))
tsdlogtourist<-ts(dlogtourist, frequency = 12, start=c(1992,2))
par(mfrow=c(3,1))
plot.ts(tsdlogtourist, ylab = "log of Number of Tourists", main="Figure3: Tourist Arrivals - Log and Differenced")
acf(dlogtourist,100, main="Figure 4: ACF and PACF of Log and Differenced Tourist Arrivals")
pacf(dlogtourist,100, main="PACF")

#Seasonal Differencing
sdlogtourist <- diff(dlogtourist, 12)
adf.test(sdlogtourist)
adf.test(sdlogtourist, k=0)
par(mfrow=c(2,1))
acf(sdlogtourist, lag=50, main="Figure 5 ACF and PACF - Seasonal differencing")
pacf(sdlogtourist, lag =50)

#Model Selection
M1<-sarima(logtourist,3,1,1,1,1,1,12) 
M2<-sarima(logtourist,3,1,1,3,1,1,12) 
M3<-sarima(logtourist,3,1,0,3,1,0,12) 
M4<-sarima(logtourist,3,1,1,3,1,2,12) 
M5<-sarima(logtourist,4,0,1,2,0,1,12)
M6<-sarima(logtourist,1,0,1,2,0,2,12)
M7<-sarima(logtourist,1,0,0,0,0,1,12)
M8<-sarima(logtourist,1,1,1,0,1,1,12)##bestest AIC and BIC
M9<-sarima(logtourist,1,1,1,0,1,0,12)
M10<-sarima(logtourist,1,1,1,1,1,1,12)
M11<-sarima(logtourist,1,1,0,1,1,0,12)
M12<-sarima(logtourist,0,1,1,0,1,1,12)
M13<-sarima(logtourist,0,1,1,0,1,2,12)
M14<-sarima(logtourist,0,1,1,0,1,2,12)

AIC_BIC<- data.frame(c1 = c(M1$AIC, M2$AIC, M3$AIC, M4$AIC, M5$AIC,M6$AIC,M7$AIC,M8$AIC,M9$AIC,M10$AIC,M11$AIC,M12$AIC,M13$AIC,M14$AIC),
                     c2 = c(M1$BIC, M2$BIC, M3$BIC, M4$BIC, M5$BIC,M6$BIC,M7$BIC,M8$BIC,M9$BIC,M10$BIC,M11$BIC,M12$BIC,M13$BIC,M14$BIC ))

colnames(AIC_BIC) <- c("AIC", "BIC")
rownames(AIC_BIC) <- c( "ARIMA(3,1,1)(1,1,1)[12]", "ARIMA(3,1,1)(3,1,1)[12]", 
                  "ARIMA(3,1,0)(3,1,0)[12]", "ARIMA(3,1,1)(3,1,2)[12]", 
                  "ARIMA(4,0,1)(2,0,1)[12]", "ARIMA(1,0,1)(2,0,2)[12]",
                  "ARIMA(1,0,0)(0,0,1)[12]", "ARIMA(1,1,1)(0,1,1)[12]",
                  "ARIMA(1,1,1)(0,1,0)[12]", "ARIMA(1,1,1)(1,1,1)[12]",
                  "ARIMA(1,1,0)(1,1,0)[12]", "ARIMA(0,1,1)(0,1,1)[12]",
                  "ARIMA(0,1,1)(0,1,2)[12]", "ARIMA(0,1,1)(0,1,2)2[12]"
                  )
AIC_BIC

################################## Best Seasonal Model ###################################
par(mfrow = c(1,1))
M8<-sarima(logtourist,1,1,1,0,1,1,12)
M8

#24-period forecast and plot
SARIMA_for<-sarima.for(logtourist, 24,1,1,1,0,1,1,12, newxreg = (nobs+1): (nobs+24))
SARIMA_for_ts<-ts(SARIMA_for$pred,frequency=12, start=c(2019,1))
SARIMA_for_se<-ts(SARIMA_for$se,frequency=12, start=c(2019,1))
exp(SARIMA_for_ts) #prediction in original scale
par(cex.axis = 1, cex.lab = 1.2, cex.main = 1)
ts.plot(tourist, exp(SARIMA_for_ts),
        ylab = "Number of Tourist Arrivals",
        xlab = "Time",
        ylim = c(10000,300000))
title("Monthly Tourist Arrivals in Nepal \nJanuary 1992 through December 2018, with 2019 - 2020 Forecast", line=-2)
U1 = exp(SARIMA_for_ts+SARIMA_for_se); L1 = exp(SARIMA_for_ts-SARIMA_for_se)
xx1 = c(time(U1), rev(time(U1))); yy1 = c(L1, rev(U1))
polygon(xx1, yy1, border = 8, col = gray(.6, alpha = .2))
lines(exp(SARIMA_for_ts), type = "p", col = 2)
lines(exp(SARIMA_for_ts), type = "l", col = 2)


#Compare 9-period forecast against real
true <- c(91793, 124421, 124697, 106136, 71640, 68782, 65749, 89382, 89078)
logtrue = log(true)
SARIMAA<-sarima.for(logtourist, 9,1,1,1,0,1,1,12, newxreg = (nobs+1):(nobs+9))
#SSE
SARIMASSE = sum((SARIMAA$pred - logtrue)^2)
SARIMASSE
#9-period forecast plot
truets <- ts(true, start = c(2019,1), frequency=12)
SARIMAA_ts <-ts(SARIMAA$pred, frequency=12, start=c(2019,1))
SARIMAA_se <-ts(SARIMAA$se, frequency=12, start=c(2019,1))
ts.plot(cbind(truets, exp(SARIMAA_ts)), gpars = list(lty =c(1:3),col=1:2, ylab="Number of Tourist Arrivals", main="Figure8: Forecast against Real Tourist Arrivals for First 9 months of 2019"))
U2 = exp(SARIMAA_ts+1.96*SARIMAA_se); L2 = exp(SARIMAA_ts-1.96*SARIMAA_se)
xx2 = c(time(U2), rev(time(U2))); yy2 = c(L2, rev(U2))
polygon(xx2, yy2, border = 8, col = gray(.6, alpha = .2))
lines(exp(SARIMAA_ts), type = "p", col = 2)

########################  Outlier Detection #############################################
#outlier Detection on the log series
logtourist<-ts(logtourist, frequency = 12, start=c(1992,1))
outlier<-tso(logtourist)
outlier
plot(outlier)
title("Figure9: Identifying Outliers")

#outlier detection on the log difference series
tsdlogtourist<-ts(tsdlogtourist, frequency = 12, start=c(1992,2))
outlier1<-tso(tsdlogtourist)
outlier1
plot(outlier1)

# As expected there were 2 outliers in the series
# Note that the outliers go away when we difference the series
# I am interested in this
# 1 Additive outlier was in 2001 June <- The royal family massacre
# 2 Temporary change outlier in 2015 May <- Earthquake

# converting to a number #location of TC outlier is 281 
tc <- rep(0, length(logtourist))
tc[281]<-1
coefhat <- outlier$outliers["coefhat"]
coefhat <- coefhat[5,]
coefhat <- as.numeric(coefhat)
# obtaining the TC data with same magnitude as determined by the tso() function
tc_effect <- coefhat*tc

# defining a time series for the temporary change data
outlier_effect <- ts(tc_effect, frequency = frequency(logtourist), start = start(logtourist))

# subtracting the transient change intervention, obtaining a time series without the transient change effect
logtourist_less_outlier <- logtourist - outlier_effect

# plot of the original, without intervention and transient change time series 
plot(cbind(logtourist, logtourist_less_outlier, outlier_effect), main="")
title("Figure10: Effect of Earthquake on Tourist number")

# Estimate the coefficients
mod_outlier=arimax(logtourist,order=c(1,1,1),
                     seasonal=list(order=c(0,1,1),period=12),
                     xtransf=data.frame(EQ=1*(seq(logtourist)==281),
                                        EQ=1*(seq(logtourist)==281)),transfer=list(c(0,0),c(1,0)),
                     method='ML')
mod_outlier
coeftest(mod_outlier)
w0<-mod_outlier$coef[4]
w1<-mod_outlier$coef[6]
w2<-mod_outlier$coef[5]
w0
w1
w2

#except for EQ.1 MA0 all were significant
plot(logtourist)
points(fitted(mod_outlier), col='blue')
title("Figure11: Original Time Series against Fitted Time Series")

# plot intervention effects
EQ1p=1*(seq(logtourist)==281)
plot(ts(EQ1p*(w0) + filter(EQ1p,filter=w2,method='recursive', side=1)*
          (w1),frequency=12,start=1992),ylab='Earthquake Effects',
     type='h'); abline(h=0)
title("Figure12: Intervention Effects \n Impact of the 2015 Earthquake")

#Estimate intervention effects
Reduced = (1-exp(w0+w1))*100
Reduced

lowered12 = (1-exp(w1*(w2^12)))*100
lowered12

lowered24 = (1-exp(w1*(w2^24)))*100
lowered24

lowered36 = (1-exp(w1*(w2^36)))*100
lowered36

lowered42 = (1-exp(w1*(w2^42)))*100
lowered42

#diagnostics
outlier_eff<- ts(EQ1p*(w0) + filter(EQ1p,filter=w2,method='recursive', side=1)*
          (w1), frequency = frequency(logtourist), start= start(logtourist))
wo_outlier <- logtourist - outlier_eff 
SARIMA_out<-sarima(wo_outlier, p=1, d=1, q=1, P=0, D=1, Q=1, S=12)
SARIMA_out

#forecast
par(mfrow = c(1,1))
SARIMA_wo_p<-sarima.for(wo_outlier, 24,1,1,1,0,1,1,12, newxreg = (nobs+1): (nobs+24))
SARIMA_wo_for<-ts(SARIMA_wo_p$pred, frequency=12, start=c(2019,1))
SARIMA_wo_se<-ts(SARIMA_wo_p$se, frequency=12, start=c(2019,1))

par(cex.axis = 1, cex.lab = 1.2, cex.main = 1)
ts.plot(tourist, exp(SARIMA_wo_for),
        ylab = "Number of Tourist Arrivals",
        xlab = "Date",
        ylim = c(10000, 300000))
title("Figure14: Monthly Tourist Arrivals in Nepal \nJanuary 1992 through December 2018, with 2019 and 2020 Forecast", line=-2)
U3 = exp(SARIMA_wo_for+SARIMA_wo_se); L3 = exp(SARIMA_wo_for-SARIMA_wo_se)
xx3 = c(time(U3), rev(time(U3))); yy3 = c(L3, rev(U3))
polygon(xx3, yy3, border = 8, col = gray(.6, alpha = .2))
lines(exp(SARIMA_wo_for), type = "p", col = 2)
lines(exp(SARIMA_wo_for), type = "l", col = 2)

exp(SARIMA_wo_for)#24 period predictions

#9-period predictions
SARIMA_wo<-sarima.for(wo_outlier, 9,1,1,1,0,1,1,12, newxreg = (nobs+1):(nobs+9))
SARIMA_wo_SSE = sum((SARIMA_wo$pred - logtrue)^2)
SARIMA_wo_SSE

SARIMA_wo_pred<- ts(SARIMA_wo$pred, frequency=12, start=c(2019,1))
SARIMA_wo_se1 <-ts(SARIMA_wo$se, frequency=12, start=c(2019,1))

#plot 9-period predictions against true data
truets <- ts(true, start = c(2019,1), frequency=12)
ts.plot(cbind(truets, exp(SARIMA_wo_pred)), gpars = list(lty =c(1:3),col=1:2, ylab="Number of Tourist Arrivals", 
                                                         main="Figure15: Forecast against Real Tourist Arrivals for First 9 months of 2019"))
U4 = exp(SARIMA_wo_pred+1.96*SARIMA_wo_se1); L4 = exp(SARIMA_wo_pred-1.96*SARIMA_wo_se1)
xx4 = c(time(U4), rev(time(U4))); yy4= c(L4, rev(U4))
polygon(xx4, yy4, border = 8, col = gray(.6, alpha = .2))
lines(exp(SARIMA_wo_pred), type = "p", col = 2)

# #plot 24 period preditions
# par(cex.axis = 1, cex.lab = 1.2, cex.main = 1)
# ts.plot(tourist, SARIMA_wo_p1,
#         ylab = "Number of Tourist Arrivals",
#         xlab = "Date")
# title("Monthly Tourist Arrivals in Nepal \nJanuary 1992 through December 2018, with Forecast", line=-2)
# axis(side = 1 , at= seq(1,348,12), labels = seq(1992,2020))
# U = exp(SARIMA_wo_p$pred+1.96*SARIMA_wo_p$se); L = exp(SARIMA_wo_p$pred-1.96*SARIMA_wo_p$se)
# xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
# polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
# lines(SARIMA_wo_p1, type = "p", col = 2)
# lines(SARIMA_wo_p1, type = "l", col = 2)