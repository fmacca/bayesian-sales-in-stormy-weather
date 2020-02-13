
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
item=44
store=2#30
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
working_dataset$preciptotal[which(is.na(working_dataset$preciptotal))]=0 #because rain==0 NOT REALLY!
working_dataset$snowfall[which(is.na(working_dataset$snowfall))]=0 #beacuse snow==0
working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]]=working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]-1]
working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]]=working_dataset$resultspeed[which(is.na(working_dataset$resultspeed))[1]+1]
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
summary(model_components <- AddMonthlyAnnualCycle(model_components,
                                                  y=y))

fit6.2.2 <- bsts(units ~ data.zoo$saturdays,data=data.zoo,state.specification=model_components, niter = niter,model.options = options)

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

CompareBstsModels(model.list=list(Model6.2=fit6.2,Model6.2.2=fit6.2.2))
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

