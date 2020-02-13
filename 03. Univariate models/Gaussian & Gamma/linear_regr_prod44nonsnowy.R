library(dplyr)
library(chron)
library(rjags)
library(coda) 
library(plotrix) 


### Prepare the dataset

train <- read.table('../../Data/train.csv', header = TRUE, sep = ',')
train <- train %>% filter(train$store_nbr == 2, train$item_nbr == 44) # item 44, store 2
key <- read.table('../../Data/key.csv', header = TRUE, sep = ',') # store 2 --> station 14
load('../../Dataset_pronti/weather_temp.RData')
meteo <- weather %>% filter(weather$station_nbr == 14)
weather_temp <- read.table('../../Data/weather_temp.csv', header = TRUE)
meteo_temp <- weather_temp %>% filter(weather_temp$station_nbr == 14)

# integrate train dataset (units sold of product 44 in store 2) with meteo 
index <- rep(0,160)
j = 1
k = 1
for (i in 1:dim(meteo)[1]) {
  if (meteo_temp$date[i] == train$date[j]){
    j = j + 1
  }
  else{
    index[k] = i
    k = k + 1
  }
}

meteo <- meteo[-index,]

# remove some useless coloumns and keep 1 year of observations
mydata <- data.frame(train,meteo)
mydata <- mydata[,c(4,6:9,11,12,16:18,21:30)]
mydata <- mydata[which(mydata$date.1<chron(dates="01/01/13", format=c(dates="d/m/y"))),]

# introduce weekends
sett <- c(1,0,0,0,0,0,1)
weekend <- c(rep(sett,52),1,0)
weekend <- weekend[-360]
mydata <- data.frame(mydata,weekend)

# introduce holidays
holiday <- rep(0,365) 
for (i in 1:dim(mydata)[1]) {
  if (mydata$date.1[i]==chron(dates="01/01/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="16/01/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="14/02/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="20/02/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="08/04/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="13/05/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="28/05/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="17/06/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="04/07/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="03/09/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="08/10/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="31/10/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="11/11/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="22/11/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="24/12/12", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="31/12/12", format=c(dates="d/m/y"))) {
    holiday[i] = 1
  }
}  
mydata <- data.frame(mydata,holiday)

# introduce blackfriday  
blackfriday <- rep(0,365)  
blackfriday[which(mydata$date.1<chron(dates="27/11/12", format=c(dates="d/m/y")) & 
                    mydata$date.1>chron(dates="19/11/12", format=c(dates="d/m/y")))] = rep(1,7) 
mydata <- data.frame(mydata,blackfriday)

# rename coloumn date.1 --> date
colnames(mydata)[2] <- "date"

# remove hail (always 0)
mydata <- mydata[,-15]

# change variable snow with dayBeforeSnow (more meaningful)
aux <- c(mydata$snow[2:dim(mydata)[1]],1)
mydata$snow <- aux

# change variable holiday with dayBeforeHoliday (maybe more meaningful)
aux <- c(mydata$holiday[2:dim(mydata)[1]],1)
mydata$holiday <- aux

# remove last row (only NA values)
mydata <- mydata[-365,]

# is there any NA value?
sum(is.na(mydata)) # --> 22

NAind <- matrix(0,22,2)
k <- 1
for (i in 1:dim(mydata)[1]) {
  for (j in 1:dim(mydata)[2]) {
    if (is.na(mydata[i,j]) == TRUE) {
      NAind[k,1] <- i
      NAind[k,2] <- j
      k <- k + 1
    }
  }
}
NAind

# Substitute NA values in tavg with the mean of tmax and tmin
indna <- which(is.na(mydata$tavg) == TRUE)
mydata$tavg[indna] <- (mydata$tmin[indna] + mydata$tmax[indna])/2 


################################################################


### Begin with a Gaussian model
data = list(y = mydata$units,
             x1 = mydata$tavg,
             x2 = mydata$weekend,
             x3 = mydata$snow,
             x4 = mydata$holiday,
             x5 = mydata$avgspeed,
             x6 = mydata$thunder,
             n = dim(mydata)[1],
             m = 7)

modelRegress=jags.model("lm_regr.bug",data=data,n.adapt=1000,n.chains=1)

update(modelRegress,n.iter=19000)

variable.names=c("b", "sigma")
n.iter=50000 
thin=10  

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)

data.out=as.matrix(outputRegress) 
data.out=data.frame(data.out)     

#summary(data.out)
head(data.out)

par(mfrow=c(2,4))
acf(data.out[,'b.1.'],lwd=3,col="red3",main="beta_0")
acf(data.out[,'b.2.'],lwd=3,col="red3",main="beta_1")
acf(data.out[,'b.3.'],lwd=3,col="red3",main="beta_2")
acf(data.out[,'b.4.'],lwd=3,col="red3",main="beta_3")
acf(data.out[,'b.5.'],lwd=3,col="red3",main="beta_4")
acf(data.out[,'b.6.'],lwd=3,col="red3",main="beta_5")
acf(data.out[,'b.7.'],lwd=3,col="red3",main="beta_6")
acf(data.out[,8],lwd=3,col="red3",main="sigma_res")

beta.post <- data.out[,1:7]
sigma.post <- data.out[,8]
#posterior mean of the beta parameters
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes

## Representation of the posterior chain of intercept
chain <- beta.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of intercept")
acf(chain,lwd=3,col="red3",main="autocorrelation of intercept")
hist(chain,nclass="fd",freq=F,main="Posterior of intercept",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of tavg
chain <- beta.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_tavg")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_tavg")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_tavg",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of weekend
chain <- beta.post[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_weekend")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_weekend")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_weekend",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of snow
chain <- beta.post[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_snow")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_snow")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_snow",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of holiday
chain <- beta.post[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of holiday")
acf(chain,lwd=3,col="red3",main="autocorrelation of holiday")
hist(chain,nclass="fd",freq=F,main="Posterior of holiday",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of avgspeed
chain <- beta.post[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of avgspeed")
acf(chain,lwd=3,col="red3",main="autocorrelation of avgspeed")
hist(chain,nclass="fd",freq=F,main="Posterior of avgspeed",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of thunder
chain <- beta.post[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of thunder")
acf(chain,lwd=3,col="red3",main="autocorrelation of thunder")
hist(chain,nclass="fd",freq=F,main="Posterior of thunder",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of sigma_res
chain <- sigma.post
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of sigma_res")
acf(chain,lwd=3,col="red3",main="autocorrelation of sigma_res")
hist(chain,nclass="fd",freq=F,main="Posterior of sigma_res",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


par(mfrow=c(1,1))


### Our Data vs Predictive Distribution
n.chain <- dim(data.out)[1]
Pred <- matrix(ncol=dim(mydata)[1],nrow=n.chain)
ypred = rep(0,n.chain * dim(mydata)[1])
for(i in 1:n.chain){
  Pred[i,]= beta.post[i,1] + beta.post[i,2] * mydata$tavg + 
    beta.post[i,3] * mydata$weekend + beta.post[i,4] * mydata$snow + 
    beta.post[i,5] * mydata$holiday + beta.post[i,6] * mydata$avgspeed +
    beta.post[i,7] * mydata$thunder
}
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(mydata)[1]){
    ypred[k] = rnorm(1, Pred[i,j], sigma.post[i])
    k = k + 1
  }
}

hist(mydata$units,nclass="fd",freq=F,
     main="Sample vs Predictive Distribution",
     col="gray", xlim = c(0,300), xlab = "units") 
lines(density(mydata$units),col="red",lwd=2)
lines(density(ypred),col="darkgreen",lwd=5)
abline(v=quantile(mydata$units,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=quantile(ypred,prob=c(0.025)),col="darkgreen",lty=2,lwd=2)
abline(v=quantile(ypred,prob=c(0.5)),col="darkgreen",lty=1,lwd=2)
abline(v=quantile(ypred,prob=c(0.975)),col="darkgreen",lty=2,lwd=2)
legend("topright",legend=c("sample", "predictive"),
       lwd=c(2,5), col=c("red","darkgreen"),lty=c(1,1))




### Try with a Gamma model
dataG = list(y = mydata$units,
            x1 = mydata$tavg,
            x2 = mydata$weekend,
            x3 = mydata$snow,
            x4 = mydata$holiday,
            x5 = mydata$avgspeed,
            x6 = mydata$thunder,
            n = dim(mydata)[1],
            m = 6)

modelRegressG=jags.model("gamma_regr.bug",data=dataG,n.adapt=1000,n.chains=1)

update(modelRegressG,n.iter=19000)

variable.namesG=c("a","b","shape")
n.iter=50000 
thin=10  

outputRegressG=coda.samples(model=modelRegressG,variable.names=variable.namesG,n.iter=n.iter,thin=thin)

data.outG=as.matrix(outputRegressG) 
data.outG=data.frame(data.outG)     

summary(data.outG)
head(data.outG)

par(mfrow=c(2,4))
acf(data.outG[,1],lwd=3,col="red3",main="alpha")
acf(data.outG[,'b.1.'],lwd=3,col="red3",main="beta_1")
acf(data.outG[,'b.2.'],lwd=3,col="red3",main="beta_2")
acf(data.outG[,'b.3.'],lwd=3,col="red3",main="beta_3")
acf(data.outG[,'b.4.'],lwd=3,col="red3",main="beta_4")
acf(data.outG[,'b.5.'],lwd=3,col="red3",main="beta_5")
acf(data.outG[,'b.6.'],lwd=3,col="red3",main="beta_6")
acf(data.outG[,8],lwd=3,col="red3",main="shape")

alfa.postG <- data.outG[,1]
beta.postG <- data.outG[,2:7]
shape.postG <- data.outG[,8]
#posterior mean of the beta parameters
beta.bayesG  <- apply(beta.postG,2,"mean")
beta.bayesG

## Representation of the posterior chain of alfa
chain <- alfa.postG
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of alpha")
acf(chain,lwd=3,col="red3",main="Autocorrelation of alpha")
hist(chain,nclass="fd",freq=F,main="Posterior of alpha",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of tavg
chain <- beta.postG[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of tavg")
acf(chain,lwd=3,col="red3",main="autocorrelation of tavg")
hist(chain,nclass="fd",freq=F,main="Posterior of tavg",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of weekend
chain <- beta.postG[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_weekend")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_weekend")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_weekend",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of snow
chain <- beta.postG[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of beta_snow")
acf(chain,lwd=3,col="red3",main="autocorrelation of beta_snow")
hist(chain,nclass="fd",freq=F,main="Posterior of beta_snow",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of holiday
chain <- beta.postG[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of holiday")
acf(chain,lwd=3,col="red3",main="autocorrelation of holiday")
hist(chain,nclass="fd",freq=F,main="Posterior of holiday",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of avgspeed
chain <- beta.postG[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of avgspeed")
acf(chain,lwd=3,col="red3",main="autocorrelation of avgspeed")
hist(chain,nclass="fd",freq=F,main="Posterior of avgspeed",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of thunder
chain <- beta.postG[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of thunder")
acf(chain,lwd=3,col="red3",main="autocorrelation of thunder")
hist(chain,nclass="fd",freq=F,main="Posterior of thunder",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of shape
chain <- shape.postG
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of shape")
acf(chain,lwd=3,col="red3",main="Autocorrelation of shape")
hist(chain,nclass="fd",freq=F,main="Posterior of shape",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


par(mfrow=c(1,1))


### Our Data vs Predictive Distribution
n.chain <- dim(data.outG)[1]
PredG <- matrix(ncol=dim(mydata)[1],nrow=n.chain)
ypredG = rep(0,n.chain * dim(mydata)[1])
for(i in 1:n.chain){
  PredG[i,]= alfa.postG[i] + beta.postG[i,1] * mydata$tavg + 
    beta.postG[i,2] * mydata$weekend + beta.postG[i,3] * mydata$snow + 
    beta.postG[i,4] * mydata$holiday + beta.postG[i,5] * mydata$avgspeed +
    beta.postG[i,6] * mydata$thunder
}
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(mydata)[1]){
    ypredG[k] = rgamma(1, shape.postG[i], shape.postG[i] / exp(PredG[i,j]))
    k = k + 1
  }
}

hist(mydata$units,nclass="fd",freq=F,
     main="Sample vs Predictive Distribution",
     col="gray", xlim = c(0,300), xlab = "units") 
lines(density(mydata$units),col="red",lwd=2)
lines(density(ypredG),col="darkgreen",lwd=5)
abline(v=quantile(mydata$units,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=quantile(ypredG,prob=c(0.025)),col="darkgreen",lty=2,lwd=2)
abline(v=quantile(ypredG,prob=c(0.5)),col="darkgreen",lty=1,lwd=2)
abline(v=quantile(ypredG,prob=c(0.975)),col="darkgreen",lty=2,lwd=2)
legend("topright",legend=c("sample", "predictive"),
       lwd=c(2,5), col=c("red","darkgreen"),lty=c(1,1))



### Confronto tra i modelli

par(mfrow = c(1,2))

hist(mydata$units,nclass="fd",freq=F,
     main="Gaussian Model",
     col="grey", xlim = c(0,300), xlab = "units") 
lines(density(mydata$units),col="indianred1",lwd=2)
lines(density(ypred),col="mediumpurple4",lwd=5)
abline(v=quantile(mydata$units,prob=c(0.025)),col="indianred1",lty=2,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.5)),col="indianred1",lty=1,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.975)),col="indianred1",lty=2,lwd=2)
abline(v=quantile(ypred,prob=c(0.025)),col="mediumpurple4",lty=2,lwd=2)
abline(v=quantile(ypred,prob=c(0.5)),col="mediumpurple4",lty=1,lwd=2)
abline(v=quantile(ypred,prob=c(0.975)),col="mediumpurple4",lty=2,lwd=2)


hist(mydata$units,nclass="fd",freq=F,
     main="Gamma Model",
     col="grey", xlim = c(0,300), xlab = "units") 
lines(density(mydata$units),col="darkturquoise",lwd=2)
lines(density(ypredG),col="deepskyblue4",lwd=5)
abline(v=quantile(mydata$units,prob=c(0.025)),col="darkturquoise",lty=2,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.5)),col="darkturquoise",lty=1,lwd=2)
abline(v=quantile(mydata$units,prob=c(0.975)),col="darkturquoise",lty=2,lwd=2)
abline(v=quantile(ypredG,prob=c(0.025)),col="deepskyblue4",lty=2,lwd=2)
abline(v=quantile(ypredG,prob=c(0.5)),col="deepskyblue4",lty=1,lwd=2)
abline(v=quantile(ypredG,prob=c(0.975)),col="deepskyblue4",lty=2,lwd=2)





