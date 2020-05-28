# forecasting at scale

library(prophet)
library(ggplot2)
library(zoo)

#generate some data
daysPerYear <- 360
years <- 5
nObs <- daysPerYear * years
days <- 1:nObs
#set a yearly cicle
yearlyCycle <- 0.8*sin(2*pi/360*days)
monthlyCycle <- 0.5*sin(2*pi/30*days)
weekylCycle <- 0.3*sin(2*pi/7*days)
seasonalComp <- yearlyCycle + monthlyCycle + weekylCycle
#seasonalComp <- yearlyCycle + weekylCycle
normTrend <- 0.005*days
incTrend <- 0.01*days
cutoff <- round(nObs/2)
trendComp <- normTrend
trendComp[(cutoff+1):nObs] <- incTrend[(cutoff+1):nObs]-(incTrend[cutoff]-normTrend[cutoff])

#now create a timeseries:
series <- trendComp + seasonalComp
dates <- seq(as.Date("2014-01-01"),length=nObs,by="days")

sample.data <- data.frame(ds=dates, y=series)

#now cutoff the series to predict it with prophet:
sample.cutOff <- daysPerYear*(years-1)
sample.in <- sample.data[1:sample.cutOff,]

#instantiate the fit:
fit <- prophet(sample.in)
               
#now prepare the forecast:
future <- make_future_dataframe(fit, periods = 360)
forecast <- predict(fit, future)
plot(fit,forecast)

#there seems to be still something off, let's check the residuals:
#only of the in-sample:
resids <- forecast$yhat[1:sample.cutOff]-sample.in$y
plot(resids, type="l")
#these is a clear structure still visible!

# the weekly seasons are not properly picked up in the fourier transformation - increase the frequency:
fit.2 <- prophet(sample.in, yearly.seasonality = 360)
future.2 <- make_future_dataframe(fit.2, periods = 360)
forecast.2 <- predict(fit.2,future.2)
plot(fit.2,forecast.2)

resids.2 <- forecast.2$yhat[1:sample.cutOff]-sample.in$y
plot(resids.2, type="l")

#this still doesn't look very good, so let's start from scratch
fit.3.aux <- prophet(sample.in, yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE, fit=FALSE)
fit.3.aux <- add_seasonality(fit.3.aux, name="yearly", period=360, fourier.order = 10, mode="additive")
fit.3.aux <- add_seasonality(fit.3.aux, name="monthly", period=30, fourier.order = 10, mode="additive")
fit.3.aux <- add_seasonality(fit.3.aux, name="weekly", period=7, fourier.order = 10, mode="additive")
#now fit:
fit.3 <- fit.prophet(fit.3.aux, sample.in)

future.3 <- make_future_dataframe(fit.3, periods = 360)
forecast.3 <- predict(fit.3,future.3)
resids.3 <- forecast.3$yhat[1:sample.cutOff]-sample.in$y
plot(resids.3, type="l")

#since we are in laboratory circumstances, we know that the series is composed of
#order one fourier transforms. Let us use this:
fit.4.aux <- prophet(sample.in, yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE, fit=FALSE)
fit.4.aux <- add_seasonality(fit.4.aux, name="yearly", period=360, fourier.order = 1, mode="additive")
fit.4.aux <- add_seasonality(fit.4.aux, name="monthly", period=30, fourier.order = 1, mode="additive")
fit.4.aux <- add_seasonality(fit.4.aux, name="weekly", period=7, fourier.order = 1, mode="additive")
#now fit:
fit.4 <- fit.prophet(fit.4.aux, sample.in)

future.4 <- make_future_dataframe(fit.4, periods = 360)
forecast.4 <- predict(fit.4,future.4)
resids.4 <- forecast.4$yhat[1:sample.cutOff]-sample.in$y
plot(resids.4, type="l")

#the forecast is expectedly very good:
check <- data.frame(id=1:360, orig=tail(series,360), pred=tail(forecast.4$yhat,360))
ggplot(check)+geom_point(aes(id,orig), color="grey",size=2)+geom_line(aes(id, pred), col="blue")+theme_light()