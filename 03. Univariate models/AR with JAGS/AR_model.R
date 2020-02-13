
#Pacchetti
library(bsts)
library(chron)
library(tidyverse)
library(data.table)
library(dplyr)
library(forecast)
library(tseries)
library(rjags)
library(coda) 
library(plotrix) 


#Dataset
load("../../Dataset_pronti/weather_temp.RData")
load("../../Dataset_pronti/train.RData")
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
last_date='01/01/13'
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

#working_dataset$units=scale(working_dataset$units)

#Holidays
july4 <- FixedDateHoliday("July4", "July", 4)
christmas <- FixedDateHoliday("Christmas", "Dec", 25)

#acf e pacf test
plot.ts(working_dataset$units)
adf.test(working_dataset$units, alternative="stationary", k=0) # stazionario

# let's try only with AR model
acf(working_dataset$units) 
pacf(working_dataset$units) #AR(6)






# JAGS
data = list(x = working_dataset$units,
            n = dim(working_dataset)[1],
            m = 4)

modelRegress=jags.model("AR_model.bug",data=data,n.adapt=1000,n.chains=1)

update(modelRegress,n.iter=19000)

variable.names=c("a","b", "tau")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)

data.out=as.matrix(outputRegress) 
data.out=data.frame(data.out)    

#summary(data.out)
head(data.out)

par(mfrow=c(1,1))
acf(data.out[,'a'],lwd=3,col="red3",main="autocorrelation of a0")

#par(mfrow=c(1,2))
acf(data.out[,'b'],lwd=3,col="red3",main="autocorrelation of b1")
# acf(data.out[,'b.2.'],lwd=3,col="red3",main="autocorrelation of b1")
#acf(data.out[,'b.3.'],lwd=3,col="red3",main="autocorrelation of b2")
# acf(data.out[,'b.4.'],lwd=3,col="red3",main="autocorrelation of b3")

par(mfrow=c(1,1))
acf(data.out[,'tau'],lwd=3,col="red3",main="autocorrelation of tau")



alfa.post <- data.out[,1]
beta.post <- data.out[,2]
sigma.post <- data.out[,3]
#posterior mean of the alfa and beta parameters
alfa.bayes  <- mean(alfa.post)
alfa.bayes
beta.bayes  <- mean(beta.post)
beta.bayes
sigma.bayes <- mean(sigma.post)
sigma.bayes

### ALFA
## Representation of the posterior chain of  a0
chain <- alfa.post
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of a0")
acf(chain,lwd=3,col="red3",main="autocorrelation of a0")
hist(chain,nclass="fd",freq=F,main="Posterior of a0",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


### BETA
## Representation of the posterior chain of  b1
chain <- beta.post
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b1")
acf(chain,lwd=3,col="red3",main="autocorrelation of b1")
hist(chain,nclass="fd",freq=F,main="Posterior of b1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b2
chain <- beta.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b2")
acf(chain,lwd=3,col="red3",main="autocorrelation of b2")
hist(chain,nclass="fd",freq=F,main="Posterior of b2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b3
chain <- beta.post[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b3")
acf(chain,lwd=3,col="red3",main="autocorrelation of b3")
hist(chain,nclass="fd",freq=F,main="Posterior of b3",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b4
chain <- beta.post[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b4")
acf(chain,lwd=3,col="red3",main="autocorrelation of b4")
hist(chain,nclass="fd",freq=F,main="Posterior of b4",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of sigma
chain <- sigma.post
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of sigma")
acf(chain,lwd=3,col="red3",main="autocorrelation of sigma")
hist(chain,nclass="fd",freq=F,main="Posterior of sigma",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

par(mfrow=c(1,1))


### We first compare the observed data distribution with the posterior 
### predictive distribution of Y

ypred <- rep(0, dim(working_dataset)[1])
ypred[1] <- working_dataset$units[1]


for ( i in 2:dim(working_dataset)[1] ){
  mu <- alfa.bayes + beta.bayes[1] * working_dataset$units[i-1]
  #+ beta.bayes[2] * working_dataset$units[i-2]
  #+ beta.bayes[3] * working_dataset$units[i-3]
    # +beta.bayes[4] * working_dataset$units[i-4]
  ypred[i] <- rnorm(1, mu, 10)
}

par(mfrow=c(1,1))
plot(working_dataset$units, col="darkgreen", type="l", lty=1, lwd=1)
lines(ypred, col="red", type="l", lty=1, lwd=1)


error=working_dataset$units-ypred
hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)") 
lines(density(error),col="blue",lwd=2)
abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)


ypred[2:365] <- ypred[3:366]
ypred[366] <- working_dataset$units[366]


 par(mfrow=c(1,1))
 plot(working_dataset$units, col="darkgreen", type="l", lty=1, lwd=1)
 lines(ypred, col="red", type="l", lty=1, lwd=1)
#
#
# error=working_dataset$units-ypred
# hist(error,nclass="fd",freq=F,main="ERROR",col="gray", xlab = "mean(y)")
# lines(density(error),col="blue",lwd=2)
# abline(v=quantile(error,prob=c(0.025)),col="red",lty=2,lwd=2)
# abline(v=quantile(error,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(error,prob=c(0.975)),col="red",lty=2,lwd=2)









