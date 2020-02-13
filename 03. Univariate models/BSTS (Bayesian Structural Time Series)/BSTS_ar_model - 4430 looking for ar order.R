
#Pacchetti
library(bsts)
library(chron)
library(tidyverse)
library(data.table)

#Dataset
load('../../Dataset_pronti/weather_temp.RData')
load('../../Dataset_pronti/train.RData')
key=read.table('../../Data/key.csv',sep=',',header=TRUE)

#Some useful function for selecting the time series
unit_series <- function(data,store,item,last_date='14/12/19',all.dates=FALSE) {
  data=data[which(data$store_nbr==store & data$item_nbr==item & data$date<chron(date=last_date,format = c(date='d/m/y'))),c(1,4)]
  
  if(all.dates)
  {
    #To be decided in the future
    #I would like to add NAs for test dates
    # and 0 for christmas
  }
  
  return(data)
}

weather_series <- function(data,station,features,last_date='14/12/19',all.dates=FALSE) {
  data=data[which(data$station_nbr==station & data$date<chron(date=last_date,format = c(date='d/m/y'))),c('date',features)]
  
  
  if(all.dates)
  {
    #To be decided in the future
  }
  
  return(data)
}


#Preliminary steps
#We try first with item 19 in store 32
last_date='19/12/19'#'01/01/13'
item=44
store=30#4
niter=2000
station=key$station_nbr[which(key$store_nbr==store)]

items_dataset=unit_series(train,item=item,last_date = last_date,store=store)
regressors_dataset=weather_series(data=weather,station=station,features =c('preciptotal','snowfall','resultspeed','rain','snow'),last_date=last_date)
working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)

plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
hist(working_dataset$units)
boxplot(working_dataset$units)

#Fixing NAs in the regressors
#working_dataset$tavg[which(is.na(working_dataset$tavg))]=(working_dataset$tmax[which(is.na(working_dataset$tavg))]+working_dataset$tmin[which(is.na(working_dataset$tavg))])/2
working_dataset$units[which(working_dataset$date=='25/12/12')]=0
working_dataset$units[which(working_dataset$date=='25/12/13')]#=0
sum(is.na(working_dataset$units))
#working_dataset$tavg=c(0,diff(working_dataset$tavg)) #Taking the difference in avg(temp)
summary(working_dataset)
working_dataset$preciptotal[which(is.na(working_dataset$preciptotal))]=0 #because rain==0
working_dataset$snowfall[which(is.na(working_dataset$snowfall))]=0 #beacuse snow==0
summary(working_dataset)
working_dataset$snow_tomorrow=shift(working_dataset$snow,n=-1)
working_dataset$snowfall_tomorrow=shift(working_dataset$snowfall,n=-1)
working_dataset$snow_tomorrow[which(is.na(working_dataset$snow_tomorrow))]=0
working_dataset$snowfall_tomorrow[which(is.na(working_dataset$snowfall_tomorrow))]=0

sd.orig=sd(na.omit(working_dataset$units))
working_dataset$units=scale(working_dataset$units)

weekend=as.numeric(is.weekend(working_dataset$date))
working_dataset$weekend=weekend
working_dataset$sundays=as.numeric(weekdays(working_dataset$date)=='Sun')
working_dataset$saturdays=as.numeric(weekdays(working_dataset$date)=='Sat')

#Holidays
july4 <- FixedDateHoliday("July4", "July", 4)
christmas <- FixedDateHoliday("Christmas", "Dec", 25)

#Constructiong zoo objects for time series
y=zoo(x=working_dataset$units,order.by = as.Date(working_dataset$date)) #Problema di Natale

options=BstsOptions(save.state.contributions = TRUE,
                    save.prediction.errors = TRUE,
                    bma.method = c("SSVS", "ODA"),
                    oda.options = list(
                      fallback.probability = 0.0,
                      eigenvalue.fudge.factor = 0.01),
                    timeout.seconds = Inf,
                    save.full.state = FALSE)

sdy=sd(na.omit(y))

# Model 0: Only linear trend
order=0
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
fit0 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit0)

PlotBstsComponents(fit0,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err0=colMeans(bsts.prediction.errors(fit0,burn = burnin)$in.sample)
err0[which(is.na(y))]=NA
plot(err0)
error=sd.orig*na.omit(err0)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)


# Model 1: AR 1
order=1
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit1 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit1)

PlotBstsComponents(fit1,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err1=colMeans(bsts.prediction.errors(fit1,burn = burnin)$in.sample)
err1[which(is.na(y))]=NA
plot(err1)
error=sd.orig*na.omit(err1)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 2: AR 2
order=2
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit2 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit2)

PlotBstsComponents(fit2,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err2=colMeans(bsts.prediction.errors(fit2,burn = burnin)$in.sample)
err2[which(is.na(y))]=NA
plot(err2)
error=sd.orig*na.omit(err2)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 4: AR 4
order=4
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit4 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit4)

PlotBstsComponents(fit4,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err4=colMeans(bsts.prediction.errors(fit4,burn = burnin)$in.sample)
err4[which(is.na(y))]=NA
plot(err4)
error=sd.orig*na.omit(err4)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 6: AR 6
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit6 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6)

PlotBstsComponents(fit6,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6=colMeans(bsts.prediction.errors(fit6,burn = burnin)$in.sample)
err6[which(is.na(y))]=NA
plot(err6)
error=sd.orig*na.omit(err6)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

for(i in 1:order)
{
  chain <- fit6$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}


# Model 7: AR 7
order=7
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit7 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit7)

PlotBstsComponents(fit7,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err7=colMeans(bsts.prediction.errors(fit7,burn = burnin)$in.sample)
err7[which(is.na(y))]=NA
plot(err7)
error=sd.orig*na.omit(err7)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 8: AR 8
order=8
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit8 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit8)

PlotBstsComponents(fit8,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err8=colMeans(bsts.prediction.errors(fit8,burn = burnin)$in.sample)
err8[which(is.na(y))]=NA
plot(err8)
error=sd.orig*na.omit(err8)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 9: AR 9
order=9
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit9 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit9)

PlotBstsComponents(fit9,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err9=colMeans(bsts.prediction.errors(fit9,burn = burnin)$in.sample)
err9[which(is.na(y))]=NA
plot(err9)
error=sd.orig*na.omit(err9)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 6.1: AR 6 +weekly
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components,
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components,
                                                 y = y,holiday=christmas))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 7))

fit6.1 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.1)

PlotBstsComponents(fit6.1,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.1=colMeans(bsts.prediction.errors(fit6.1,burn = burnin)$in.sample)
err6.1[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.1)
plot(error)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

for(i in 1:order)
{
  chain <- fit6.1$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

# Model 6.1.1: AR 6 + weekly (regression holiday version)
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 7))

fit6.1.1 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.1.1)

PlotBstsComponents(fit6.1.1,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.1.1=colMeans(bsts.prediction.errors(fit6.1.1,burn = burnin)$in.sample)
err6.1.1[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.1.1)
plot(error)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

CompareBstsModels(model.list=list(Model6.1=fit6.1,Model6.1.1=fit6.1.1))

# Model 6.2: AR 6 + weekend Regression
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))

fit6.2 <- bsts(units ~ data.zoo$weekend,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.2)

PlotBstsComponents(fit6.2,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.2=colMeans(bsts.prediction.errors(fit6.2,burn = burnin)$in.sample)
err6.2[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.2)
plot(error,type = 'l')
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

summary(fit6.2)

PlotBstsForecastDistribution(fit6.2, burn = burnin,show.actuals = FALSE)
lines(working_dataset$date,working_dataset$units,col = 'green4')


for(i in 1:order)
{
  chain <- fit6.2$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

i=1
chain <- fit6.2$coefficients[-(1:burnin),2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main=paste("Trace plot of beta",i))
acf(chain,lwd=3,col="red3",main=paste("autocorrelation of beta",i))
hist(chain,nclass="fd",freq=F,main=paste("Posterior of beta",i),col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


# Model 6.3: AR 6 + weekend Regression + All regression
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))

fit6.3 <- bsts(units ~ .,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.3)

PlotBstsComponents(fit6.3,burn = burnin)
PlotBstsPredictors(fit6.3,burn=burnin)
PlotBstsCoefficients(fit6.3,burn=burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.3=colMeans(bsts.prediction.errors(fit6.3,burn = burnin)$in.sample)
err6.3[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.3)
plot(error,type = 'l')
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

summary(fit6.3)
CompareBstsModels(model.list=list(Model6.1=fit6.1,Model6.2=fit6.2,Model6.3=fit6.3))


# Final camparison
CompareBstsModels(model.list=list(Model1=fit1,Model2=fit2,Model4=fit4,Model6=fit6,Model7=fit7,Model8=fit8,Model9=fit9,Model6.1=fit6.1))
acf(y, lag.max = 30,na.action = na.pass)
hist(sd.orig*na.omit(y),nclass="fd",freq=F,main="Distribution of y",col="gray", xlab = "mean(y)")
lines(density(sd.orig*na.omit(y)),col="blue",lwd=2)
abline(v=quantile(sd.orig*na.omit(y),prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(sd.orig*na.omit(y),prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(sd.orig*na.omit(y),prob=c(0.975)),col="red",lty=2,lwd=2)

CompareBstsModels(model.list=list(Model6.1=fit6.1,Model6.2=fit6.2))


#-------------------------------------------------------------------------------------------------------------------------------------------
# Going on from 6.2: AR 6 + Regression Holiday + weekend Regression
niter=4000

# Model 6.2: AR 6 + weekend Regression
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))

# prior=SpikeSlabPrior(model.matrix(data.zoo$units ~ data.zoo$weekend),
#                      y = NULL,
#                      expected.r2 = .5,
#                      prior.df = .01,
#                      expected.model.size = 1,
#                      prior.information.weight = .01,
#                      diagonal.shrinkage = .5,
#                      optional.coefficient.estimate =c(0, 0.5),
#                      max.flips = -1,
#                      mean.y = mean(y, na.rm = TRUE),
#                      sdy = 3*sd(as.numeric(y), na.rm = TRUE),
#                      prior.inclusion.probabilities = NULL,
#                      sigma.upper.limit = sdy)

prior=SpikeSlabPrior(model.matrix(data.zoo$units ~ data.zoo$weekend),
                     y = data.zoo$units,
                     expected.r2 = .5,
                     prior.df = .01,
                     expected.model.size = 1,
                     prior.information.weight = .1,
                     diagonal.shrinkage = .5,
                     max.flips = -1,
                     prior.inclusion.probabilities = c(0, 0.8),
                     sigma.upper.limit = sdy)
  
fit6.2 <- bsts(units ~ data.zoo$weekend,data=data.zoo,state.specification=model_components,prior=prior, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.2)

PlotBstsComponents(fit6.2,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.2=colMeans(bsts.prediction.errors(fit6.2,burn = burnin)$in.sample)
err6.2[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.2)
plot(error,type = 'l')
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

summary(fit6.2)

PlotBstsForecastDistribution(fit6.2, burn = burnin,show.actuals = FALSE)
lines(working_dataset$date,working_dataset$units,col = 'green4')


for(i in 1:order)
{
  chain <- fit6.2$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

i=1
chain <- fit6.2$coefficients[-(1:burnin),2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main=paste("Trace plot of beta",i))
acf(chain,lwd=3,col="red3",main=paste("autocorrelation of beta",i))
hist(chain,nclass="fd",freq=F,main=paste("Posterior of beta",i),col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

for(i in 1:3)
{
  chain <- fit6.2$Christmas[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of christmas",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of christmas",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of christmas",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}


# Model 6.2.1: + annual cycle
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))
summary(model_components <- AddMonthlyAnnualCycle(model_components,
                                                  y=y))

fit6.2.1 <- bsts(units ~ data.zoo$weekend,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.2.1)

PlotBstsComponents(fit6.2.1,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.2.1=colMeans(bsts.prediction.errors(fit6.2.1,burn = burnin)$in.sample)
err6.2.1[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.2.1)
plot(error,type = 'l')
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

summary(fit6.2.1)
layout(matrix(c(1),1,1,byrow=T))
PlotBstsForecastDistribution(fit6.2.1, burn = burnin,show.actuals = FALSE)
lines(working_dataset$date,working_dataset$units,col = 'green4')

CompareBstsModels(model.list=list(Model6.2=fit6.2,Model6.2.1=fit6.2.1))
350*sd.orig

for(i in 1:order)
{
  chain <- fit6.2.1$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

i=1
chain <- fit6.2.1$coefficients[-(1:burnin),2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main=paste("Trace plot of beta",i))
acf(chain,lwd=3,col="red3",main=paste("autocorrelation of beta",i))
hist(chain,nclass="fd",freq=F,main=paste("Posterior of beta",i),col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

# Model 6.2.2: no weekend but sun + sat
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
order=6
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = order,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                           upper.limit=sdy,
                                                                           sample.size=length(y))))
summary(model_components <- AddRegressionHoliday(model_components,
                                                 y = y,holiday.list = list(christmas,july4)))

fit6.2.2 <- bsts(units ~ data.zoo$saturdays + data.zoo$sundays,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6.2.2)

PlotBstsComponents(fit6.2.2,burn = burnin)
layout(matrix(c(1,2),1,2,byrow=T))
err6.2.2=colMeans(bsts.prediction.errors(fit6.2.2,burn = burnin)$in.sample)
err6.2.2[which(is.na(y))]=NA
error=sd.orig*na.omit(err6.2.2)
plot(error,type = 'l')
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

summary(fit6.2.2)
layout(matrix(c(1),1,1,byrow=T))
PlotBstsForecastDistribution(fit6.2.2, burn = burnin,show.actuals = FALSE)
lines(working_dataset$date,working_dataset$units,col = 'green4')

CompareBstsModels(model.list=list(Model6.2=fit6.2,Model6.2.1=fit6.2.1,Model6.2.2=fit6.2.2))
350*sd.orig

for(i in 1:order)
{
  chain <- fit6.2.2$AR6.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

for(i in 2:3)
{
  chain <- fit6.2.2$coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of beta",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of beta",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of beta",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}


CompareBstsModels(model.list=list(Model6.2=fit6.2,Model0=fit0,Model6=fit6))
CompareBstsModels(model.list=list(Model6.2=fit6.2,Model0=fit0,Model6.2c=fit6.2c))
