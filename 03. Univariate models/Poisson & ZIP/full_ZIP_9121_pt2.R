library(dplyr)
library(chron)
library(rjags)
library(coda) 
library(plotrix) 


### Prepare the dataset

train <- read.table('../../Data/train.csv', header = TRUE, sep = ',')
train <- train %>% filter(train$store_nbr == 21, train$item_nbr == 91) # item 91, store 21
key <- read.table('../../Data/key.csv', header = TRUE, sep = ',') # store 21 --> station 3
load('../../Dataset_pronti/weather_temp.RData')
meteo <- weather %>% filter(weather$station_nbr == 3)
weather_temp <- read.table('../../Data/weather_temp.csv', header = TRUE)
meteo_temp <- weather_temp %>% filter(weather_temp$station_nbr == 3)

# integrate train dataset (units sold of product 91 in store 21) with meteo 
index <- rep(0,121)
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
sum(is.na(mydata)) # --> 0


################################################################

acf(mydata$units)
pacf(mydata$units)

### Begin with a Poisson model
dataP = list(y = mydata$units,
             x1 = mydata$tavg,
             x2 = mydata$foschia,
             x3 = mydata$snow,
             x4 = mydata$weekend,
             x5 = mydata$avgspeed,
             x6 = mydata$holiday,
             x7 = mydata$thunder,
             n = dim(mydata)[1],
             m = 8)

modelRegressP=jags.model("poisson_9121.bug",data=dataP,n.adapt=1000,n.chains=1)

update(modelRegressP,n.iter=19000)

variable.namesP=c("b")
n.iter=50000 
thin=10

outputRegressP=coda.samples(model=modelRegressP,variable.names=variable.namesP,n.iter=n.iter,thin=thin)

data.outP=as.matrix(outputRegressP) 
data.outP=data.frame(data.outP)    

#summary(data.outP)
head(data.outP)

par(mfrow=c(2,4))
acf(data.outP[,'b.1.'],lwd=3,col="red3",main="autocorrelation of b0")
acf(data.outP[,'b.2.'],lwd=3,col="red3",main="autocorrelation of b1")
acf(data.outP[,'b.3.'],lwd=3,col="red3",main="autocorrelation of b2")
acf(data.outP[,'b.4.'],lwd=3,col="red3",main="autocorrelation of b3")
acf(data.outP[,'b.5.'],lwd=3,col="red3",main="autocorrelation of b4")
acf(data.outP[,'b.6.'],lwd=3,col="red3",main="autocorrelation of b5")
acf(data.outP[,'b.7.'],lwd=3,col="red3",main="autocorrelation of b6")
acf(data.outP[,'b.8.'],lwd=3,col="red3",main="autocorrelation of b7")

beta.postP <- data.outP[,1:8]
#posterior mean of the beta parameters
beta.bayesP  <- apply(beta.postP,2,"mean")
beta.bayesP


### BETA
## Representation of the posterior chain of b0 (intercept)
chain <- beta.postP[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b0")
acf(chain,lwd=3,col="red3",main="autocorrelation of b0")
hist(chain,nclass="fd",freq=F,main="Posterior of b0",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b1 (tavg)
chain <- beta.postP[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b1")
acf(chain,lwd=3,col="red3",main="autocorrelation of b1")
hist(chain,nclass="fd",freq=F,main="Posterior of b1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b2 (foschia)
chain <- beta.postP[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b2")
acf(chain,lwd=3,col="red3",main="autocorrelation of b2")
hist(chain,nclass="fd",freq=F,main="Posterior of b2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b3 (snow)
chain <- beta.postP[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b3")
acf(chain,lwd=3,col="red3",main="autocorrelation of b3")
hist(chain,nclass="fd",freq=F,main="Posterior of b3",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b4 (weekend)
chain <- beta.postP[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b4")
acf(chain,lwd=3,col="red3",main="autocorrelation of b4")
hist(chain,nclass="fd",freq=F,main="Posterior of b4",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b5 (avgspeed)
chain <- beta.postP[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b5")
acf(chain,lwd=3,col="red3",main="autocorrelation of b5")
hist(chain,nclass="fd",freq=F,main="Posterior of b5",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b6 (holiday)
chain <- beta.postP[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b6")
acf(chain,lwd=3,col="red3",main="autocorrelation of b6")
hist(chain,nclass="fd",freq=F,main="Posterior of b6",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b7 (thunder)
chain <- beta.postP[,8]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b7")
acf(chain,lwd=3,col="red3",main="autocorrelation of b7")
hist(chain,nclass="fd",freq=F,main="Posterior of b7",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


par(mfrow=c(1,1))


### We first compare the observed data distribution with the posterior 
### predictive distribution of Y

lambdamodP <- rep(0,dim(mydata)[1])
ymeanP <- rep(0,dim(mydata)[1])
ysdP <- rep(0,dim(mydata)[1])
for(i in 1 : dim(mydata)[1]) {
  xb <- beta.bayesP[1] + beta.bayesP[2] * mydata$tavg[i] + 
    beta.bayesP[3] * mydata$foschia[i] + beta.bayesP[4] * mydata$snow[i] + 
    beta.bayesP[5] * mydata$weekend[i] + beta.bayesP[6] * mydata$avgspeed[i] +
    beta.bayesP[7] * mydata$holiday[i] + beta.bayesP[8] * mydata$thunder[i]
  lambdamodP[i] <- exp(xb)
  ysdP[i] <- sqrt(lambdamodP[i]) 
}

hist(lambdamodP,nclass="fd",freq=F,main="Posterior Mean",col="gray", xlab = "mean(y)") 
lines(density(lambdamodP),col="blue",lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=mean(mydata$units), col = "limegreen", lty = 1, lwd = 5)
legend("topright",legend=c("posterior median", "95% Credible bounds","sample mean"),
       lwd=c(2,2,5), col=c("red","red","limegreen"),lty=c(1,2,1))

hist(ysdP,nclass="fd",freq=F,main="Posterior Std Deviation",col="gray",xlab = "sd(y)") 
lines(density(ysdP),col="blue",lwd=2)
abline(v=quantile(ysdP,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ysdP,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ysdP,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=sd(mydata$units), col = "limegreen", lty = 1, lwd = 5)
legend("topright",legend=c("posterior median", "95% Credible bounds","sample std deviation"),
       lwd=c(2,2,5), col=c("red","red","limegreen"),lty=c(1,2,1))

plot(1:364, lambdamodP, type = 'l', lty = 1, lwd = 1.5, col = "red3", xlab = "days",
     ylab = "lambda_post", main = "Posterior lambda in the first year")


### Our Data vs Predictive Distribution
n.chain <- dim(data.outP)[1]
PredP <- matrix(ncol=dim(mydata)[1],nrow=n.chain)
for(i in 1:n.chain){
  PredP[i,]= beta.postP[i,1] + beta.postP[i,2] * mydata$tavg + 
    beta.postP[i,3] * mydata$foschia + beta.postP[i,4] * mydata$snow + 
    beta.postP[i,5] * mydata$weekend + beta.postP[i,6] * mydata$avgspeed +
    beta.postP[i,7] * mydata$holiday + beta.postP[i,8] * mydata$thunder
}
PredP = exp(PredP)
ypredP = rep(0,n.chain * dim(mydata)[1])
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(mydata)[1]){
    ypredP[k] = rpois(1,PredP[i,j])
    k = k + 1
  }
}
perc0 <- table(mydata$units)/length(mydata$units)
perc0predP <- table(ypredP)/length(ypredP)

par(mfrow=c(1,1))
plot(perc0, type = "b", col = "orange1", lwd = 6, 
     main = "Sample distribution vs Predictive distribution", ylab = "%", 
     xlab = "units", xlim = c(0,11), ylim = c(0,0.75))
lines(perc0predP, type = "b", col = "firebrick", lwd = 3, lty = 2)
legend("topright",legend=c("sample","predictive"),
       lwd=c(3,3), col=c("orange1","firebrick"),lty=c(1,2))



### Try with a ZIP model (zero-inflated poisson)
data = list(y = mydata$units,
            x1 = mydata$tavg,
            x2 = mydata$foschia,
            x3 = mydata$snow,
            x4 = mydata$weekend,
            x5 = mydata$avgspeed,
            x6 = mydata$holiday,
            x7 = mydata$thunder,
            n = dim(mydata)[1],
            ma = 3,
            mb = 8)

modelRegress=jags.model("full_ZIP_9121_pt2.bug",data=data,n.adapt=1000,n.chains=1)

update(modelRegress,n.iter=19000)

variable.names=c("a", "b")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)

data.out=as.matrix(outputRegress) 
data.out=data.frame(data.out)    

#summary(data.out)
head(data.out)

layout(matrix(c(1:3,0,4:11),3,4,byrow=T))
acf(data.out[,'a.1.'],lwd=3,col="red3",main="alpha_0")
acf(data.out[,'a.2.'],lwd=3,col="red3",main="alpha_1")
acf(data.out[,'a.3.'],lwd=3,col="red3",main="alpha_2")
acf(data.out[,'b.1.'],lwd=3,col="red3",main="beta_0")
acf(data.out[,'b.2.'],lwd=3,col="red3",main="beta_1")
acf(data.out[,'b.3.'],lwd=3,col="red3",main="beta_2")
acf(data.out[,'b.4.'],lwd=3,col="red3",main="beta_3")
acf(data.out[,'b.5.'],lwd=3,col="red3",main="beta_4")
acf(data.out[,'b.6.'],lwd=3,col="red3",main="beta_5")
acf(data.out[,'b.7.'],lwd=3,col="red3",main="beta_6")
acf(data.out[,'b.8.'],lwd=3,col="red3",main="beta_7")


alfa.post <- data.out[,1:3]
beta.post <- data.out[,4:11]
#posterior mean of the alfa and beta parameters
alfa.bayes  <- apply(alfa.post,2,"mean")
alfa.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes


### ALFA
## Representation of the posterior chain of  a0 (intercept)
chain <- alfa.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of a0")
acf(chain,lwd=3,col="red3",main="autocorrelation of a0")
hist(chain,nclass="fd",freq=F,main="Posterior of a0",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of a1 (foschia)
chain <- alfa.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of a1")
acf(chain,lwd=3,col="red3",main="autocorrelation of a1")
hist(chain,nclass="fd",freq=F,main="Posterior of a1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

##Representation of the posterior chain of a2 (thunder)
chain <- alfa.post[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of a2")
acf(chain,lwd=3,col="red3",main="autocorrelation of a2")
hist(chain,nclass="fd",freq=F,main="Posterior of a2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

### BETA
## Representation of the posterior chain of  b0 (intercept)
chain <- beta.post[,1]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b0")
acf(chain,lwd=3,col="red3",main="autocorrelation of b0")
hist(chain,nclass="fd",freq=F,main="Posterior of b0",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b1 (tavg)
chain <- beta.post[,2]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b1")
acf(chain,lwd=3,col="red3",main="autocorrelation of b1")
hist(chain,nclass="fd",freq=F,main="Posterior of b1",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b2 (foschia)
chain <- beta.post[,3]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b2")
acf(chain,lwd=3,col="red3",main="autocorrelation of b2")
hist(chain,nclass="fd",freq=F,main="Posterior of b2",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b3 (snow)
chain <- beta.post[,4]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b3")
acf(chain,lwd=3,col="red3",main="autocorrelation of b3")
hist(chain,nclass="fd",freq=F,main="Posterior of b3",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b4 (weekend)
chain <- beta.post[,5]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b4")
acf(chain,lwd=3,col="red3",main="autocorrelation of b4")
hist(chain,nclass="fd",freq=F,main="Posterior of b4",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b5 (avgspeed)
chain <- beta.post[,6]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b5")
acf(chain,lwd=3,col="red3",main="autocorrelation of b5")
hist(chain,nclass="fd",freq=F,main="Posterior of b5",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b6 (holiday)
chain <- beta.post[,7]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b6")
acf(chain,lwd=3,col="red3",main="autocorrelation of b6")
hist(chain,nclass="fd",freq=F,main="Posterior of b6",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


##Representation of the posterior chain of b7 (thunder)
chain <- beta.post[,8]
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of b7")
acf(chain,lwd=3,col="red3",main="autocorrelation of b7")
hist(chain,nclass="fd",freq=F,main="Posterior of b7",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)


par(mfrow=c(1,1))


### We first compare the observed data distribution with the posterior 
### predictive distribution of Y

lambdamod <- rep(0,dim(mydata)[1])
pmod <- rep(0,dim(mydata)[1])
ymean <- rep(0,dim(mydata)[1])
ysd <- rep(0,dim(mydata)[1])
for(i in 1 : dim(mydata)[1]) {
  xa <- alfa.bayes[1] + alfa.bayes[2] * mydata$foschia[i] + 
    alfa.bayes[3] * mydata$thunder[i] 
  xb <- beta.bayes[1] + beta.bayes[2] * mydata$tavg[i] + 
    beta.bayes[3] * mydata$foschia[i] + beta.bayes[4] * mydata$snow[i] + 
    beta.bayes[5] * mydata$weekend[i] + beta.bayes[6] * mydata$avgspeed[i] +
    beta.bayes[7] * mydata$holiday[i] + beta.bayes[8] * mydata$thunder[i]
  pmod[i] <- exp(xa)/(1 + exp(xa))
  lambdamod[i] <- exp(xb)
  ymean[i] <- (1 - pmod[i]) * lambdamod[i]
  ysd[i] <- sqrt(lambdamod[i] * (1 - pmod[i]) * (1 + pmod[i] * lambdamod[i]))
}

par(mfrow=c(1,1))
hist(ymean,nclass="fd",freq=F,main="Posterior Mean",col="gray", xlab = "mean(y)") 
lines(density(ymean),col="blue",lwd=2)
abline(v=quantile(ymean,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ymean,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ymean,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=mean(mydata$units), col = "limegreen", lty = 1, lwd = 5)
legend("topright",legend=c("posterior median", "95% Credible bounds","sample mean"),
       lwd=c(2,2,5), col=c("red","red","limegreen"),lty=c(1,2,1))

hist(ysd,nclass="fd",freq=F,main="Posterior Std Deviation",col="gray",xlab = "sd(y)") 
lines(density(ysd),col="blue",lwd=2)
abline(v=quantile(ysd,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ysd,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ysd,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=sd(mydata$units), col = "limegreen", lty = 1, lwd = 5)
legend("topright",legend=c("posterior median", "95% Credible bounds","sample std deviation"),
       lwd=c(2,2,5), col=c("red","red","limegreen"),lty=c(1,2,1))


### Our Data vs Predictive Distribution
n.chain <- dim(data.out)[1]
Preda <- matrix(ncol=dim(mydata)[1],nrow=n.chain)
Predb <- matrix(ncol=dim(mydata)[1],nrow=n.chain)
for(i in 1:n.chain){
  Predb[i,]= beta.post[i,1] + beta.post[i,2] * mydata$tavg + 
    beta.post[i,3] * mydata$foschia + beta.post[i,4] * mydata$snow + 
    beta.post[i,5] * mydata$weekend + beta.post[i,6] * mydata$avgspeed + 
    beta.post[i,7] * mydata$holiday + beta.post[i,8] * mydata$thunder
  Preda[i,]= alfa.post[i,1] + alfa.post[i,2] * mydata$foschia + 
    alfa.post[i,3] * mydata$thunder
}
Predb = exp(Predb)
Preda = exp(Preda)/(1 + exp(Preda))
ypred = rep(0,n.chain * dim(mydata)[1])
a = 0
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(mydata)[1]){
    a = rbinom(1,1,Preda[i,j])
    ypred[k] = (1 - a) * rpois(1,Predb[i,j])
    k = k + 1
  }
}
perc0 <- table(mydata$units)/length(mydata$units)
perc0pred <- table(ypred)/length(ypred)

par(mfrow=c(1,1))
plot(perc0, type = "b", col = "orange1", lwd = 6, 
     main = "Sample distribution vs Predictive distribution", ylab = "%", 
     xlab = "units", xlim = c(0,11), ylim = c(0,0.75))
lines(perc0pred, type = "b", col = "firebrick", lwd = 3, lty = 2)
legend("topright",legend=c("sample","predictive"),
       lwd=c(3,3), col=c("orange1","firebrick"),lty=c(1,2))



### Confronto tra i due modelli

par(mfrow = c(1,2))

# Posterior Mean
hist(lambdamodP,nclass="fd",freq=F,main="Posterior Mean (Poisson)",col="gray", xlab = "mean(y)") 
lines(density(lambdamodP),col="blue",lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(lambdamodP,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=mean(mydata$units), col = "limegreen", lty = 1, lwd = 5)

hist(ymean,nclass="fd",freq=F,main="Posterior Mean (ZIP)",col="gray", xlab = "mean(y)") 
lines(density(ymean),col="blue",lwd=2)
abline(v=quantile(ymean,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ymean,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ymean,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=mean(mydata$units), col = "limegreen", lty = 1, lwd =   5)


# Posterior Std Deviation
hist(ysdP,nclass="fd",freq=F,main="Posterior Std Dev (Poisson)",col="gray",xlab = "sd(y)") 
lines(density(ysdP),col="blue",lwd=2)
abline(v=quantile(ysdP,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ysdP,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ysdP,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=sd(mydata$units), col = "limegreen", lty = 1, lwd = 5)

hist(ysd,nclass="fd",freq=F,main="Posterior Std Dev (ZIP)",col="gray",xlab = "sd(y)") 
lines(density(ysd),col="blue",lwd=2)
abline(v=quantile(ysd,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(ysd,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(ysd,prob=c(0.975)),col="red",lty=2,lwd=2)
abline(v=sd(mydata$units), col = "limegreen", lty = 1, lwd = 5)


# Sample Distribution vs Posterior Distribution
plot(perc0, type = "b", col = "indianred1", lwd = 6, 
     main = "Sample vs Predictive (Poisson)", ylab = "%", xlab = "units", 
     xlim = c(0,11), ylim = c(0,0.75))
lines(perc0predP, type = "b", col = "mediumpurple4", lwd = 3, lty = 2)

plot(perc0, type = "b", col = "mediumspringgreen", lwd = 6, 
     main = "Sample vs Predictive (ZIP)", ylab = "%", xlab = "units", 
     xlim = c(0,11), ylim = c(0,0.75))
lines(perc0pred, type = "b", col = "aquamarine4", lwd = 3, lty = 2)




####################################################################


# TEST PREDICTION 2013

test9121 <- read.table("test_9121.txt", header=TRUE)

### Our Data vs Predictive Distribution
n.chain <- dim(data.outP)[1]
PredP_test <- matrix(ncol=dim(test9121)[1],nrow=n.chain)
for(i in 1:n.chain){
  PredP_test[i,]= beta.postP[i,1] + beta.postP[i,2] * test9121$tavg + 
    beta.postP[i,3] * test9121$foschia + beta.postP[i,4] * test9121$snow + 
    beta.postP[i,5] * test9121$weekend + beta.postP[i,6] * test9121$avgspeed +
    beta.postP[i,7] * test9121$holiday + beta.postP[i,8] * test9121$thunder
}
PredP_test = exp(PredP_test)
ypredP_test = rep(0,n.chain * dim(test9121)[1])
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(test9121)[1]){
    ypredP_test[k] = rpois(1,PredP_test[i,j])
    k = k + 1
  }
}
perc0_test <- table(test9121$units)/length(test9121$units)
perc0predP_test <- table(ypredP_test)/length(ypredP_test)

par(mfrow=c(1,1))
plot(perc0_test, type = "b", col = "orange1", lwd = 6, 
     main = "Sample distribution vs Predictive distribution", ylab = "%", 
     xlab = "units", xlim = c(0,11), ylim = c(0,0.75))
lines(perc0predP_test, type = "b", col = "firebrick", lwd = 3, lty = 2)
legend("topright",legend=c("sample","predictive"),
       lwd=c(3,3), col=c("orange1","firebrick"),lty=c(1,2))


n.chain <- dim(data.out)[1]
Preda_test <- matrix(ncol=dim(test9121)[1],nrow=n.chain)
Predb_test <- matrix(ncol=dim(test9121)[1],nrow=n.chain)
for(i in 1:n.chain){
  Predb_test[i,]= beta.post[i,1] + beta.post[i,2] * test9121$tavg + 
    beta.post[i,3] * test9121$foschia + beta.post[i,4] * test9121$snow + 
    beta.post[i,5] * test9121$weekend + beta.post[i,6] * test9121$avgspeed + 
    beta.post[i,7] * test9121$holiday
  Preda_test[i,]= alfa.post[i,1] + alfa.post[i,2] * test9121$foschia + 
    alfa.post[i,3] * test9121$thunder
}
Predb_test = exp(Predb_test)
Preda_test = exp(Preda_test)/(1 + exp(Preda_test))
ypred_test = rep(0,n.chain * dim(test9121)[1])
a = 0
k = 1
for(i in 1:n.chain){
  for(j in 1:dim(test9121)[1]){
    a = rbinom(1,1,Preda_test[i,j])
    ypred_test[k] = (1 - a) * rpois(1,Predb_test[i,j])
    k = k + 1
  }
}
perc0pred_test<- table(ypred_test)/length(ypred_test)

par(mfrow=c(1,1))
plot(perc0_test, type = "b", col = "orange1", lwd = 6, 
     main = "Sample distribution vs Predictive distribution", ylab = "%", 
     xlab = "units", xlim = c(0,11), ylim = c(0,0.75))
lines(perc0pred_test, type = "b", col = "firebrick", lwd = 3, lty = 2)
legend("topright",legend=c("sample","predictive"),
       lwd=c(3,3), col=c("orange1","firebrick"),lty=c(1,2))


### Confronto tra i modelli

par(mfrow = c(1,2))

# Sample Distribution vs Posterior Distribution
plot(perc0_test, type = "b", col = "indianred1", lwd = 6, 
     main = "Prediction on 2013 (Poisson)", ylab = "%", xlab = "units", 
     xlim = c(0,11), ylim = c(0,0.75))
lines(perc0predP_test, type = "b", col = "mediumpurple4", lwd = 3, lty = 2)

plot(perc0_test, type = "b", col = "mediumspringgreen", lwd = 6, 
     main = "Prediction on 2013 (ZIP)", ylab = "%", xlab = "units", 
     xlim = c(0,11), ylim = c(0,0.75))
lines(perc0pred_test, type = "b", col = "aquamarine4", lwd = 3, lty = 2)










