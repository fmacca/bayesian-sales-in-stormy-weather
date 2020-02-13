
#Pacchetti
library(bsts)
library(chron)
library(tidyverse)
library(data.table)

#Dataset
load('../../../Dataset_pronti/weather_temp.RData')
load('../../../Dataset_pronti/train.RData')
key=read.table('../../../Data/key.csv',sep=',',header=TRUE)

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
item=91
store=21
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
working_dataset$rain[which(is.na(working_dataset$preciptotal))]
working_dataset$preciptotal[which(is.na(working_dataset$preciptotal))]=0 #because rain==0 NOT REALLY!
working_dataset$snow[which(is.na(working_dataset$snowfall))]
working_dataset$snowfall[which(is.na(working_dataset$snowfall))]=0 #beacuse snow==0
#working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]]=working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]-1]
#working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]]=working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]+1]
working_dataset$snow_tomorrow=shift(working_dataset$snow,n=-1)
working_dataset$snowfall_tomorrow=shift(working_dataset$snowfall,n=-1)
working_dataset$snow_tomorrow[which(is.na(working_dataset$snow_tomorrow))]=0
working_dataset$snowfall_tomorrow[which(is.na(working_dataset$snowfall_tomorrow))]=0

sd.orig=sd(na.omit(working_dataset$units))
#working_dataset$units=scale(working_dataset$units)

#Holidays
july4 <- FixedDateHoliday("July4", "July", 4)
christmas <- FixedDateHoliday("Christmas", "Dec", 25)

#Constructiong zoo objects for time series
y=zoo(x=working_dataset$units,order.by = as.Date(working_dataset$date)) #Problema di Natale

sdy=sd(na.omit(y))
#Adding model components
(model_components <- list())
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))

options=BstsOptions(save.state.contributions = TRUE,
                    save.prediction.errors = TRUE,
                    bma.method = c("SSVS", "ODA"),
                    oda.options = list(
                      fallback.probability = 0.0,
                      eigenvalue.fudge.factor = 0.01),
                    timeout.seconds = Inf,
                    save.full.state = FALSE)
fit <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit)

colnames(fit$state.contributions)
par(mfrow=c(1,1))
plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"trend",]),col='red')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]),col='blue')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"seasonal.30.1",]),col='orange')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Monthly",]),col='black')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Ar4",]),col='black')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"July4",]),col='red')
# lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Christmas",]),col='red')

PlotBstsForecastDistribution(fit, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit)
CompareBstsModels(model.list=list(fit,fit))

par(mfrow=c(1,1))
plot(fit,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

#Diagnostic
for(i in 1:4)
{
  chain <- fit$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

par(mfrow=c(1,1))
err1=colMeans(bsts.prediction.errors(fit,burn = burnin)$in.sample)
err1[which(is.na(y))]=NA
plot(err1)
error=sd.orig*na.omit(err1)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

#Model 2
#Holidays
july4 <- FixedDateHoliday("July4", "July", 4)
christmas <- FixedDateHoliday("Christmas", "Dec", 25)

(model_components <- list())
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))

options=BstsOptions(save.state.contributions = TRUE,
                    save.prediction.errors = TRUE,
                    bma.method = c("SSVS", "ODA"),
                    oda.options = list(
                      fallback.probability = 0.0,
                      eigenvalue.fudge.factor = 0.01),
                    timeout.seconds = Inf,
                    save.full.state = FALSE)
fit2 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit2)

colnames(fit2$state.contributions)
par(mfrow=c(1,1))
plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
lines(working_dataset$date,colMeans(fit2$state.contributions[-(1:burnin),"Ar4",]),col='black')
lines(working_dataset$date,colMeans(fit2$state.contributions[-(1:burnin),"July4",]),col='red')
lines(working_dataset$date,colMeans(fit2$state.contributions[-(1:burnin),"Christmas",]),col='red')

par(mfrow=c(1,1))
plot(fit2,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsForecastDistribution(fit2, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit2)
CompareBstsModels(model.list=list(fit,fit2))


#Diagnostic
for(i in 1:4)
{
  chain <- fit2$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

#MODEL 3: ARLESS
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 7))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 30))
summary(model_components <- AddMonthlyAnnualCycle(model_components,
                                                  y=y))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))

options=BstsOptions(save.state.contributions = TRUE,
                    save.prediction.errors = TRUE,
                    bma.method = c("SSVS", "ODA"),
                    oda.options = list(
                      fallback.probability = 0.0,
                      eigenvalue.fudge.factor = 0.01),
                    timeout.seconds = Inf,
                    save.full.state = FALSE)
fit3 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit3)

colnames(fit3$state.contributions)
par(mfrow=c(1,1))
plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"trend",]),col='red')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"seasonal.7.1",]),col='blue')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"seasonal.30.1",]),col='orange')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"Monthly",]),col='black')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"July4",]),col='red')
lines(working_dataset$date,colMeans(fit3$state.contributions[-(1:burnin),"Christmas",]),col='red')

par(mfrow=c(1,1))
plot(fit3,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsComponents(fit3,burn = burnin)

PlotBstsForecastDistribution(fit3, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit3)
CompareBstsModels(model.list=list(fit,fit2,fit3))


#MOdel4: AR + trend + holiday
niter=4000
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit4 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit4)

par(mfrow=c(1,1))
plot(fit4,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsComponents(fit4,burn = burnin)

PlotBstsForecastDistribution(fit4, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit4)
CompareBstsModels(model.list=list(fit,fit2,fit3,fit4))


for(i in 1:4)
{
  chain <- fit4$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

err4=colMeans(bsts.prediction.errors(fit4,burn = burnin)$in.sample)
err4[which(is.na(y))]=NA
plot(err4)
error=sd.orig*na.omit(err4)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

#Saving the error for Jack
#save(err4,list="err4",file="Residuals/err44_30_model4.RData")

#Model 5: AR + trend + holiday + REGRESSORS
data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
niter=4000
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))
# summary(model_components <- AddRandomWalkHoliday(model_components, 
#                                                  y = y,holiday=july4))
# summary(model_components <- AddRandomWalkHoliday(model_components, 
#                                                  y = y,holiday=christmas))
fit5 <- bsts(units ~ .,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit5)

par(mfrow=c(1,1))
plot(fit5,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsComponents(fit5,burn = burnin)

for(i in 1:4)
{
  chain <- fit5$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}
summary(fit5,burn = burnin)

par(mfrow=c(1,1))
err5=colMeans(bsts.prediction.errors(fit4,burn = burnin)$in.sample)
err5[which(is.na(y))]=NA
plot(err5)
error=sd.orig*na.omit(err5)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)


CompareBstsModels(model.list=list(fit,fit2,fit3,fit4,fit5))

#MOdel4: AR + trend + holiday
niter=4000
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))
fit4 <- bsts(y,model_components, niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit4)

par(mfrow=c(1,1))
plot(fit4,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsComponents(fit4,burn = burnin)

PlotBstsForecastDistribution(fit4, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit4)
CompareBstsModels(model.list=list(fit,fit2,fit3,fit4))


for(i in 1:4)
{
  chain <- fit4$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

err4=colMeans(bsts.prediction.errors(fit4,burn = burnin)$in.sample)
err4[which(is.na(y))]=NA
plot(err4)
error=sd.orig*na.omit(err4)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

#Saving the error for Jack
#save(err4,list="err4",file="Residuals/err44_30_model4.RData")

#Model 6: AR + family=poisson
(model_components <- list())
summary(model_components <- AddAr(model_components, 
                                  y = y,lags = 4,sigma.prior = SdPrior(sigma.guess=3*sdy,
                                                                       upper.limit=sdy,
                                                                       sample.size=length(y))))

fit6 <- bsts(y,model_components,family = "poisson", niter = niter,model.options = options)

burnin=SuggestBurn(.1, fit6)

par(mfrow=c(1,1))
plot(fit6,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

PlotBstsComponents(fit6,burn = burnin)

PlotBstsForecastDistribution(fit6, burn = burnin,ylim=c(-1,9))
lines(working_dataset$date,working_dataset$units,col = 'green4')

PlotBstsPredictionErrors(fit6)


for(i in 1:4)
{
  chain <- fit6$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

err6=colMeans(bsts.prediction.errors(fit6,burn = burnin)$in.sample)
err6[which(is.na(y))]=NA
plot(err6)
error=sd.orig*na.omit(err4)
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)

CompareBstsModels(model.list=list(fit,fit2,fit3,fit4,fit5,fit6))

