library(chron)
library(dplyr)


load("../../Dataset_pronti/weather_temp.RData")
load("../../Dataset_pronti/train.RData")

train_p44 <- train%>%filter(train$item_nbr==44, train$units>0)
# prendo store 30

key=read.table('../../Data/key.csv',sep=',',header=TRUE)
# store 30 -> station 19
weather_1<-weather[which(weather$date<chron(dates="01/01/13", format=c(dates="d/m/y")) & weather$date>=chron(dates="01/01/12", format=c(dates="d/m/y"))),]
weather_stat19 <- weather_1%>%filter(weather_1$station_nbr==19)

na_ind <- which(is.na(weather_stat19$tavg)==TRUE)
weather_stat19$tavg[na_ind] = (weather_stat19$tmax[na_ind] + weather_stat19$tmin[na_ind]) / 2

temperature<-na.omit(weather_stat19[,c(2,5)])


library(forecast)
library(tseries)

plot.ts(temperature$tavg, ylab="Temperature", main="Avarage Temperature")
plot.ts(diff(temperature$tavg), ylab="Temperature", main="Avarage Temperature First Difference") # more stationary


adf.test(diff(temperature$tavg), alternative="stationary", k=0)
# We see that the series is stationary enough to do any kind of time series modelling.

# Next step is to find the right parameters to be used in the ARIMA model

acf(temperature$tavg, lag.max = 20)
acf(diff(temperature$tavg), lag.max=20)
# AR(1), 

pacf(temperature$tavg, lag.max = 20)
pacf(diff(temperature$tavg), lag.max=20)# MA(2)

# ACF is a plot of total correlation between different lag functions
auto.arima(diff(temperature$tavg)) #AR(0), MA(3)

(fit <- arima(diff(temperature$tavg), c(1, 0, 2), seasonal = list(order = c(1, 0, 2), period = 12)))
pred <- predict(fit, n.ahead = 5) # prediction at 5 days
ts.plot(as.ts(diff(temperature$tavg)), pred$pred, lty=c(1,2), col=c(1,2))






weather_1_test<-weather[which(weather$date<chron(dates="05/01/13", format=c(dates="d/m/y")) & weather$date>=chron(dates="01/01/12", format=c(dates="d/m/y"))),]
weather_stat19_test <- weather_1_test%>%filter(weather_1_test$station_nbr==19)

temperature_test<-na.omit(weather_stat19_test[,c(2,5)])

ts.plot(as.ts(diff(temperature_test$tavg)), pred$pred, lty=c(1,2), col=c(1,2))















 