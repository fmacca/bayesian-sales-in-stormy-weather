load("../../../Dataset_pronti/mydata.RData")
load("../../../Dataset_pronti/for_multivariate/ar1data.RData")
library(chron)
library(tidyverse)
library(data.table)
library(dplyr)
library(rjags)
library(coda) 
library(plotrix) 

### multivariate.bug
ar1data[,c(2,3,4,5,6,40,41,42,43,44)]<-scale(ar1data[,c(2,3,4,5,6,40,41,42,43,44)])
data = list(y1 = ar1data$units_2,
            y2 = ar1data$units_21,
            y3 = ar1data$units_30,
            y4 = ar1data$units_33,
            y5 = ar1data$units_36,
            x11 = ar1data$tavg_2,
            x21 = ar1data$tavg_21,
            x31 = ar1data$tavg_30,
            x41 = ar1data$tavg_33,
            x51 = ar1data$tavg_36,
            x12 = ar1data$avgspeed_2,
            x22 = ar1data$avgspeed_21,
            x32 = ar1data$avgspeed_30,
            x42 = ar1data$avgspeed_33,
            x52 = ar1data$avgspeed_36,
            x13 = ar1data$snow_2,
            x23 = ar1data$snow_21,
            x33 = ar1data$snow_30,
            x43 = ar1data$snow_33,
            x53 = ar1data$snow_36,
            x14 = ar1data$weekend,
            x24 = ar1data$weekend,
            x34 = ar1data$weekend,
            x44 = ar1data$weekend,
            x54 = ar1data$weekend,
            x15 = ar1data$holiday,
            x25 = ar1data$holiday,
            x35 = ar1data$holiday,
            x45 = ar1data$holiday,
            x55 = ar1data$holiday,
            z1 = ar1data$units_2_t1,
            z2 = ar1data$units_21_t1,
            z3 = ar1data$units_30_t1,
            z4 = ar1data$units_33_t1,
            z5 = ar1data$units_36_t1,
            n = dim(ar1data)[1],
            m = 50)


modelRegress=jags.model("multivariate.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("b", "tau1", "tau2", "tau3", "tau4", "tau5")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out1=as.matrix(outputRegress) 
data.out1=data.frame(data.out1) 
head(data.out1)
save(data.out1,list="data.out1",file="data.out1.RData")

par(mfrow=c(2,5))
acf(data.out1[,'b.1.'],lwd=3,col="red3",main="autocorrelation of b1")
acf(data.out1[,'b.2.'],lwd=3,col="red3",main="autocorrelation of b2")
acf(data.out1[,'b.3.'],lwd=3,col="red3",main="autocorrelation of b3")
acf(data.out1[,'b.4.'],lwd=3,col="red3",main="autocorrelation of b4")
acf(data.out1[,'b.5.'],lwd=3,col="red3",main="autocorrelation of b5")
acf(data.out1[,'b.6.'],lwd=3,col="red3",main="autocorrelation of b6")
acf(data.out1[,'b.7.'],lwd=3,col="red3",main="autocorrelation of b7")
acf(data.out1[,'b.8.'],lwd=3,col="red3",main="autocorrelation of b8")
acf(data.out1[,'b.9.'],lwd=3,col="red3",main="autocorrelation of b9")
acf(data.out1[,'b.10.'],lwd=3,col="red3",main="autocorrelation of b10")

par(mfrow=c(2,5))
acf(data.out1[,'b.11.'],lwd=3,col="red3",main="autocorrelation of b11")
acf(data.out1[,'b.12.'],lwd=3,col="red3",main="autocorrelation of b12")
acf(data.out1[,'b.13.'],lwd=3,col="red3",main="autocorrelation of b13")
acf(data.out1[,'b.14.'],lwd=3,col="red3",main="autocorrelation of b14")
acf(data.out1[,'b.15.'],lwd=3,col="red3",main="autocorrelation of b15")
acf(data.out1[,'b.16.'],lwd=3,col="red3",main="autocorrelation of b16")
acf(data.out1[,'b.17.'],lwd=3,col="red3",main="autocorrelation of b17")
acf(data.out1[,'b.18.'],lwd=3,col="red3",main="autocorrelation of b18")
acf(data.out1[,'b.19.'],lwd=3,col="red3",main="autocorrelation of b19")
acf(data.out1[,'b.20.'],lwd=3,col="red3",main="autocorrelation of b20")

beta.post1 <- data.out1[,1:50]
sigma.post1 <- data.out1[,51:55]
#posterior mean of the beta parameters
beta.bayes1  <- apply(beta.post1,2,"mean")
beta.bayes1

## Representation of the posterior chain of b1
chain <- beta.post1[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of intercept")
acf(chain,lwd=3,col="red3",main="autocorrelation of intercept")
hist(chain,nclass="fd",freq=F,main="Posterior of intercept",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b2
chain <- beta.post1[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_tavg")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_tavg")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_tavg",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b3
chain <- beta.post1[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_weekend")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_weekend")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_weekend",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b4
chain <- beta.post1[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_snow")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_snow")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_snow",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b5
chain <- beta.post1[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of holiday")
acf(chain,lwd=3,col="red3",main="autocorrelation of holiday")
hist(chain,nclass="fd",freq=F,main="Posterior of holiday",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b6
chain <- beta.post1[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of avgspeed")
acf(chain,lwd=3,col="red3",main="autocorrelation of avgspeed")
hist(chain,nclass="fd",freq=F,main="Posterior of avgspeed",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of thunder
chain <- beta.post1[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of thunder")
acf(chain,lwd=3,col="red3",main="autocorrelation of thunder")
hist(chain,nclass="fd",freq=F,main="Posterior of thunder",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of sigma_res
chain <- sigma.post1[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of sigma_res")
acf(chain,lwd=3,col="red3",main="autocorrelation of sigma_res")
hist(chain,nclass="fd",freq=F,main="Posterior of sigma_res",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)









### multivariate_matr_diag.bug
ar1data[,c(2,3,4,5,6,40,41,42,43,44)]<-scale(ar1data[,c(2,3,4,5,6,40,41,42,43,44)])
data = list(y = ar1data[,c(2,3,4,5,6)],
            x1 = ar1data[,c(17,18,19,20,21)],
            x2 = ar1data[,c(22,23,24,25,26)],
            x3 = ar1data[,c(32,33,34,35,36)],
            x4 = ar1data[,c(37,37,37,37,37)],
            x5 = ar1data[,c(38,38,38,38,38)],
            z = ar1data[,c(40,41,42,43,44)],
            n = dim(ar1data)[1],
            m = 5)

modelRegress=jags.model("multivariate_matr_diag.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("a", "b1","b2","b3","b4","b5", "tau")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out=as.matrix(outputRegress) 
data.out=data.frame(data.out) 
head(data.out)
save(data.out,list="data.out",file="data.out.RData")

alpha.post <- data.out[,1:25]
beta.post <- data.out[,26:50]
sigma.post <- data.out[,51:75]


#posterior mean of the beta parameters
alpha.bayes <- apply(alpha.post,2,"mean")
alpha.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes

## Representation of the posterior chain of b1.1
chain <- beta.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of intercept")
acf(chain,lwd=3,col="red3",main="autocorrelation of intercept")
hist(chain,nclass="fd",freq=F,main="Posterior of intercept",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b1.2
chain <- beta.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_tavg")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_tavg")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_tavg",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b1.3
chain <- beta.post[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_weekend")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_weekend")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_weekend",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b1.4
chain <- beta.post[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_snow")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_snow")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_snow",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b1.5
chain <- beta.post[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of holiday")
acf(chain,lwd=3,col="red3",main="autocorrelation of holiday")
hist(chain,nclass="fd",freq=F,main="Posterior of holiday",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b2.1
chain <- beta.post[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of avgspeed")
acf(chain,lwd=3,col="red3",main="autocorrelation of avgspeed")
hist(chain,nclass="fd",freq=F,main="Posterior of avgspeed",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b2.2
chain <- beta.post[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of thunder")
acf(chain,lwd=3,col="red3",main="autocorrelation of thunder")
hist(chain,nclass="fd",freq=F,main="Posterior of thunder",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of b2.3
chain <- sigma.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of sigma_res")
acf(chain,lwd=3,col="red3",main="autocorrelation of sigma_res")
hist(chain,nclass="fd",freq=F,main="Posterior of sigma_res",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##
####
######
########
#########################################################################
########
######
####
##

### multivariate_wishart.bug
ar1data[,c(2,3,4,5,6,40,41,42,43,44)]<-scale(ar1data[,c(2,3,4,5,6,40,41,42,43,44)])
data = list(y = ar1data[,c(2,3,4,5,6)],
            x1 = ar1data[,c(17,18,19,20,21)],
            x2 = ar1data[,c(22,23,24,25,26)],
            x3 = ar1data[,c(32,33,34,35,36)],
            x4 = ar1data[,c(37,37,37,37,37)],
            x5 = ar1data[,c(38,38,38,38,38)],
            z = ar1data[,c(40,41,42,43,44)],
            n = dim(ar1data)[1],
            m = 5,
            R=rbind(c(5,0,0,0,0),c(0,5,0,0,0), c(0,0,5,0,0), c(0,0,0,5,0), c(0,0,0,0,5)))

modelRegress=jags.model("multivariate_wishart.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("a", "b1","b2","b3","b4","b5", "tau")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out_wish=as.matrix(outputRegress) 
data.out_wish=data.frame(data.out_wish) 
head(data.out_wish)
save(data.out_wish,list="data.out_wish",file="data.out_wish.RData")

alpha.post <- data.out_wish[,1:25]
beta.post <- data.out_wish[,26:50]
sigma.post <- data.out_wish[,51:75]

## Representation of the posterior chain of beta_temperature in store 2
chain <- beta.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_temperature_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_temperature_2")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_temperature_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Representation of the posterior chain of beta_wind in store 2
chain <- beta.post[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_wind_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_wind_2")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_wind_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Representation of the posterior chain of beta_snow in store 2
chain <- beta.post[,11]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_snow_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_snow_2")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_snow_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Representation of the posterior chain of beta_weekend in store 2
chain <- beta.post[,16]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_weekend_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_weekend_2")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_weekend_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Representation of the posterior chain of beta_holiday in store 2
chain <- beta.post[,21]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_holiday_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_holiday_2")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_holiday_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Effect of unit2(t-1) on unit2(t)
chain <- alpha.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of alpha1_1")
acf(chain,lwd=3,col="red3",main="autocorrelation of alpha1_1")
hist(chain,nclass="fd",freq=F,main="Posterior of alpha1_1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

###########################

## Effect of unit21(t-1) on unit2(t)
chain <- alpha.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of alpha2_1")
acf(chain,lwd=3,col="red3",main="autocorrelation of alpha2_1")
hist(chain,nclass="fd",freq=F,main="Posterior of alpha2_1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Effect of unit2(t-1) on unit21(t)
chain <- alpha.post[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of alpha1_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of alpha1_2")
hist(chain,nclass="fd",freq=F,main="Posterior of alpha1_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

## Effect of unit21(t-1) on unit21(t)
chain <- alpha.post[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of alpha2_2")
acf(chain,lwd=3,col="red3",main="autocorrelation of alpha2_2")
hist(chain,nclass="fd",freq=F,main="Posterior of alpha2_2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##############################

##
####
######
########
#########################################################################
########
######
####
##


### multivariate_matr_diag_predictive.bug
load("../../../Dataset_pronti/for_multivariate/ar1data.RData")
yorig=ar1data[,2:6]
means=colMeans(yorig)
sds=apply(yorig,2,sd)
ar1data[,c(2,3,4,5,6,40,41,42,43,44)]<-scale(ar1data[,c(2,3,4,5,6,40,41,42,43,44)])
# head(yorig)
# rec=ar1data[i,2:6]
# for(i in 1:364)
# {
#   rec[i,]=ar1data[i,2:6]*sds+means
# }
# head(rec)

n.train=300
n.test=10
ar1data.train=ar1data[1:n.train,]
ar1data.test=ar1data[(n.train+1):(n.train+n.test),]
ytest.orig=yorig[(n.train+1):(n.train+n.test),]

data = list(y = ar1data.train[,c(2,3,4,5,6)],
            x1 = ar1data.train[,c(17,18,19,20,21)],
            x2 = ar1data.train[,c(22,23,24,25,26)],
            x3 = ar1data.train[,c(32,33,34,35,36)],
            x4 = ar1data.train[,c(37,37,37,37,37)],
            x5 = ar1data.train[,c(38,38,38,38,38)],
            z = ar1data.train[,c(40,41,42,43,44)],
            n = dim(ar1data.train)[1],
            m = 5,
            x1star = ar1data.test[,c(17,18,19,20,21)],
            x2star = ar1data.test[,c(22,23,24,25,26)],
            x3star = ar1data.test[,c(32,33,34,35,36)],
            x4star = ar1data.test[,c(37,37,37,37,37)],
            x5star = ar1data.test[,c(38,38,38,38,38)],
            zstar = ar1data.test[,c(40,41,42,43,44)],
            nstar = dim(ar1data.test)[1])

modelRegress=jags.model("multivariate_matr_diag_predictive.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=1000)

variable.names=c("a", "b1","b2","b3","b4","b5", "tau","ystar")
n.iter=5000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out=as.matrix(outputRegress) 
data.out=data.frame(data.out) 
head(data.out)
save(data.out,list="data.out",file="data.out.prediciton.RData")

alpha.post <- data.out[,1:25]
beta.post <- data.out[,26:50]
sigma.post <- data.out[,51:75]
ystar.post <- data.out[,76:125]


#posterior mean of the beta parameters
alpha.bayes <- apply(alpha.post,2,"mean")
alpha.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes
ystar.bayes  <- apply(ystar.post,2,"mean")
ystar.bayes

ytest=ar1data.test[,2:6]

## Representation of the posterior chain of ystar (treaceplots)
layout(matrix(1:n.test,2,5,byrow=T))
for(j in 1:5){
  for(i in 1:n.test){
    chain <- ystar.post[,(j-1)*n.test+i]
    title=paste("ystar",j,"[t=n.train+",i,"]",sep="")
    plot(chain,type="l",main=paste("Trace plot of",title))
  }
}

## Representation of the posterior chain of ystar (autocorrealations)
layout(matrix(1:n.test,2,5,byrow=T))
for(j in 1:5){
  for(i in 1:n.test){
    chain <- ystar.post[,(j-1)*n.test+i]
    title=paste("ystar",j,"[t=n.train+",i,"]",sep="")
    acf(chain,lwd=3,col="red3",main=paste("autocorrelation of",title))
  }
}

## Representation of the posterior chain of ystar
layout(matrix(1:n.test,2,5,byrow=T))
for(j in 1:5){
  for(i in 1:n.test){
    chain <- ystar.post[,(j-1)*n.test+i]
    title=paste("ystar",j,"[t=n.train+",i,"]",sep="")
    hist(chain,nclass="fd",freq=F,main=paste("Posterior of",title),xlim=c(-3,3),col="gray") 
    lines(density(chain),col="blue",lwd=2)
    abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
    abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
    abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
    abline(v=ytest[i,j],col="green4",lty=1,lwd=2)
  }
}

## Representation of the posterior chain of ystar (back to original scale)
ystar.post.rescaled=ystar.post
for(j in 1:5){
  for(i in 1:n.test){
    ystar.post.rescaled[,(j-1)*n.test+i]=ystar.post[,(j-1)*n.test+i]*sds[j]+means[j]
  }
}
layout(matrix(1:n.test,2,5,byrow=T))
for(j in 1:5){
  for(i in 1:n.test){
    chain <- ystar.post.rescaled[,(j-1)*n.test+i]
    title=paste("ystar",j,"[t=n.train+",i,"]",sep="")
    hist(chain,nclass="fd",freq=F,main=paste("Posterior of",title),col="gray") 
    lines(density(chain),col="blue",lwd=2)
    abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
    abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
    abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
    abline(v=ytest.orig[i,j],col="green4",lty=1,lwd=2)
  }
}

