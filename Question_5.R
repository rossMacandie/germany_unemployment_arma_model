##PART A

setwd("Econometrics")
getwd()
##Import dataset
Unemployment_Germany <- read.csv("Unemployment_Germany.csv")


## Install packages and import xts and t series libraries
install.packages("xts")
install.packages("tseries")


##Convert unemployment rate into a number rather than a character
Unemployment_Germany$unrate <- as.numeric(as.character(Unemployment_Germany$unrate))

##Convert the data into a recognized time series
library(xts)
unemployment <- ts(Unemployment_Germany, start = c(1969,1), frequency = 4)
unemployment <- as.xts(unemployment)
head(unemployment)

## Display data in a graph
plot.xts(unemployment$unrate,ylim = c(0,12),type="l",main = "Germany's Unemployment Rate from 1969:Q1 to 2021:Q4")
## data is clearly non-stationary as mean has moved. 


## Install and import urca library
install.packages("urca")
library(urca)

##Conduct an adf test to assess whether the series is stationary
lunrate <- log(unemployment$unrate)
plot.xts(lunrate,ylim = c(0,12),type="l",main = "Logged value of Germany's Unemployment Rate from 1969:Q1 to 2021:Q4")
summary(ur.df(lunrate, type=c("trend"), lags =8, selectlags = c("AIC")))
##In this case we can reject the null hypothesis of non-stationarity
## at the 10% level (3.402>3.13)
## but not at the 5% level 
summary(ur.df(unemployment$unrate, type=c("trend"), lags =8, selectlags = c("AIC")))

##Based on previous analysis, the series should not be considered stationary 
##Find the first difference of the series
dlunrate <-diff(lunrate,1)
##Remove first row, which is NA because of first difference
dlunrate <- dlunrate[-1,]

##Conduct an adf test on the difference
summary(ur.df(dlunrate, type = c("trend"), lags = 8, selectlags = c("AIC")))
## We can reject at the 1% level (4.3258 > 3.99) the null hypothesis of non-stationarity of the logarithmic first difference

##PART B

## I will include the period 2017Q1 to 2021Q4 as my out of sample period
dlunrate_insample <- dlunrate['/2017']
dlunrate_outofsample <- dlunrate['2018/']

##View the ACF and PACF for the logarithmic first difference of Germany's unemployment rate
library(forecast)
Acf(dlunrate_insample, lag.max = 12, type=c("correlation"), plot=TRUE, na.action = na.contiguous, demean = TRUE)
Pacf(dlunrate_insample, lag.max = 12, plot=TRUE, na.action = na.contiguous, demean = TRUE)
##ARMA(1,5) seems like an appropriate model 

##Determine the best ARMA model using the auto.arima command
Auto_ARIMA_model <- auto.arima(dlunrate_insample, max.p = 10, max.q = 10, seasonal = "FALSE")
summary(Auto_ARIMA_model)
## based on the command the best model is an ARMA(5,2) 


##Estimate all the possible combinations 
arma00 <- arima(dlunrate_insample, order = c(0,0,0))
arma01 <- arima(dlunrate_insample, order = c(0,0,1))
arma02 <- arima(dlunrate_insample, order = c(0,0,2))
arma03 <- arima(dlunrate_insample, order = c(0,0,3))
arma04 <- arima(dlunrate_insample, order = c(0,0,4))
arma05 <- arima(dlunrate_insample, order = c(0,0,5))
arma10 <- arima(dlunrate_insample, order = c(1,0,0))
arma11 <- arima(dlunrate_insample, order = c(1,0,1))
arma12 <- arima(dlunrate_insample, order = c(1,0,2))
arma13 <- arima(dlunrate_insample, order = c(1,0,3))
arma14 <- arima(dlunrate_insample, order = c(1,0,4))
arma15 <- arima(dlunrate_insample, order = c(1,0,5))

##Estimate the AIC values 
AIC_arma00 <- AIC(arma00)
AIC_arma01 <- AIC(arma01)
AIC_arma02 <-AIC(arma02)
AIC_arma03 <-AIC(arma03)
AIC_arma04 <-AIC(arma04)
AIC_arma05 <-AIC(arma05)
AIC_arma10 <-AIC(arma10)
AIC_arma11 <-AIC(arma11)
AIC_arma12 <-AIC(arma12)
AIC_arma13 <-AIC(arma13)
AIC_arma14 <-AIC(arma14)
AIC_arma15 <-AIC(arma15)

AIC_values <- c(AIC_arma01,AIC_arma02,AIC_arma03,AIC_arma04,AIC_arma10,AIC_arma11,AIC_arma12,AIC_arma13,AIC_arma14)

##Estimate BIC values
BIC_arma00 <- BIC(arma00)
BIC_arma01 <- BIC(arma01)
BIC_arma02 <-BIC(arma02)
BIC_arma03 <-BIC(arma03)
BIC_arma04 <-BIC(arma04)
BIC_arma05 <-BIC(arma05)
BIC_arma10 <-BIC(arma10)
BIC_arma11 <-BIC(arma11)
BIC_arma12 <-BIC(arma12)
BIC_arma13 <-BIC(arma13)
BIC_arma14 <-BIC(arma14)
BIC_arma15 <-BIC(arma15)

BIC_values <- c(BIC_arma01,BIC_arma02,BIC_arma03,BIC_arma04,BIC_arma10,BIC_arma11,BIC_arma12,BIC_arma13,BIC_arma14)

##My candidate are the following
## AR(1) has the lowest BIC value 
## MA(4) has the lowest AIC value
##ARMA(1,1) as it has the second lowest BIC value
##ARMA(1,4) as it has the second lowest AIC value

##Check for autocorrelation using the Breusch-Godfrey test
library(car)
##AR(1)
arma10res <- arma10$residuals
bgarma10 <- arima(arma10res, order=c(4,0,0))
linearHypothesis(bgarma10,c("ar1","ar2","ar3","ar4"),c(0,0,0,0))

##MA(4)
arma04res <- arma04$residuals
bgarma04 <- arima(arma04res, order=c(4,0,0))
linearHypothesis(bgarma04,c("ar1","ar2","ar3","ar4"),c(0,0,0,0))

##ARMA(1,1)
arma11res <- arma11$residuals
bgarma11 <- arima(arma11res, order=c(4,0,0))
linearHypothesis(bgarma11,c("ar1","ar2","ar3","ar4"),c(0,0,0,0))

##ARMA(1,4)
arma14res <- arma14$residuals
bgarma14 <- arima(arma14res, order=c(4,0,0))
linearHypothesis(bgarma14,c("ar1","ar2","ar3","ar4"),c(0,0,0,0))

##None of theses models are autocorellated, null hypothesis is accepted

##Access heteroskedasticity using the ARCH test
install.packages("FinTS")
library(FinTS)
ArchTest(arma10res,lag = 12,demean = FALSE)
ArchTest(arma04res,lag = 12,demean = FALSE)
ArchTest(arma14res,lag = 12,demean = FALSE)
ArchTest(arma11res,lag = 12,demean = FALSE)

## All the residuals of the models are heteroscedastic

##Lastly, test all the models for normality of the residuals using the Jarque-Bera test.
library(tseries)
jarque.bera.test(arma04res)
jarque.bera.test(arma10res)
jarque.bera.test(arma14res)
jarque.bera.test(arma11res)

## The residuals of the model are not normally distributed

##PART C
## Same out of sample used in part b so begin with forecasting
forecast_arma04 <- forecast(arma04, h = 20)
accuracy(forecast_arma04, dlunrate_outofsample)
plot(forecast_arma04)

forecast_arma10 <- forecast(arma10, h = 20)
accuracy(forecast_arma10, dlunrate_outofsample)
plot(forecast_arma10)

forecast_arma14 <- forecast(arma14, h = 20)
accuracy(forecast_arma14, dlunrate_outofsample)
plot(forecast_arma14)

forecast_arma11 <- forecast(arma11, h = 20)
accuracy(forecast_arma11, dlunrate_outofsample)
plot(forecast_arma11)




## ARMA(1,1) is the best model
