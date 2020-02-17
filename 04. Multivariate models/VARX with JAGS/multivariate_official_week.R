library(chron)
library(tidyverse)
library(data.table)
library(dplyr)
library(rjags)
library(coda) 
library(plotrix)

load("../../Dataset_pronti/for_multivariate/ar1data_week.RData")
ar1data_week=ar1data_week[7:364,]

### multivariate_wishart.bug
yorig=ar1data_week[,2:6]
means=colMeans(yorig)
sds=apply(yorig,2,sd)
ar1data_week[,c(2,3,4,5,6,40,41,42,43,44)]<-scale(ar1data_week[,c(2,3,4,5,6,40,41,42,43,44)])

n.train=300
n.test=10
ar1data_week.train=ar1data_week[1:n.train,]
ar1data_week.test=ar1data_week[(n.train+1):(n.train+n.test),]
ytest.orig=yorig[(n.train+1):(n.train+n.test),]



data = list(y = ar1data_week[,c(2,3,4,5,6)],
            x1 = ar1data_week[,c(17,18,19,20,21)],
            x2 = ar1data_week[,c(22,23,24,25,26)],
            x3 = ar1data_week[,c(32,33,34,35,36)],
            x4 = ar1data_week[,c(37,37,37,37,37)],
            x5 = ar1data_week[,c(40,40,40,40,40)],
            x6 = ar1data_week[,c(41,41,41,41,41)],
            x7 = ar1data_week[,c(42,42,42,42,42)],
            x8 = ar1data_week[,c(43,43,43,43,43)],
            x9 = ar1data_week[,c(44,44,44,44,44)],
            x10 = ar1data_week[,c(45,45,45,45,45)],
            x11 = ar1data_week[,c(46,46,46,46,46)],
            z = ar1data_week[,c(47,48,49,50,51)],
            n = dim(ar1data_week)[1],
            m = 5,
            R=rbind(c(5,0,0,0,0),c(0,5,0,0,0), c(0,0,5,0,0), c(0,0,0,5,0), c(0,0,0,0,5)),
            x1star = ar1data_week.test[,c(17,18,19,20,21)],
            x2star = ar1data_week.test[,c(22,23,24,25,26)],
            x3star = ar1data_week.test[,c(32,33,34,35,36)],
            x4star = ar1data_week.test[,c(37,37,37,37,37)],
            x5star = ar1data_week.test[,c(40,40,40,40,40)],
            x6star = ar1data_week.test[,c(41,41,41,41,41)],
            x7star = ar1data_week.test[,c(42,42,42,42,42)],
            x8star = ar1data_week.test[,c(43,43,43,43,43)],
            x9star = ar1data_week.test[,c(44,44,44,44,44)],
            x10star = ar1data_week.test[,c(45,45,45,45,45)],
            x11star = ar1data_week.test[,c(46,46,46,46,46)],
            zstar = ar1data_week.test[,c(47,48,49,50,51)],
            nstar = dim(ar1data_week.test)[1])

modelRegress=jags.model("multivariate_wishart_week.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("a", "b1","b2","b3","b4","b5","b6","b7", "b8","b9", "b10","b11", "tau","ystar")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out_wish_week=as.matrix(outputRegress) 
data.out_wish_week=data.frame(data.out_wish_week) 
head(data.out_wish_week)
save(data.out_wish_week,list="data.out_wish_week",file="output/data.out_wish_week.RData")

alpha.post <- data.out_wish_week[,1:25]
beta.post <- data.out_wish_week[,26:80]
sigma.post <- data.out_wish_week[,81:105]
ystar.post <- data.out_wish_week[,106:155]


#posterior mean of the beta parameters
alpha.bayes <- apply(alpha.post,2,"mean")
alpha.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes
sigma.bayes  <- apply(sigma.post,2,"mean")
sigma.bayes
ystar.bayes  <- apply(ystar.post,2,"mean")
ystar.bayes

ytest=ar1data_week.test[,2:6]

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
    hist(chain,nclass="fd",freq=F,main=paste("Posterior of",title),xlim=c(0,400),col="gray") 
    lines(density(chain),col="blue",lwd=2)
    abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
    abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
    abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
    abline(v=ytest.orig[i,j],col="green4",lty=1,lwd=2)
  }
}


# Prediciton error MSE (scaled)
ystarpred=ytest.orig
for(j in 1:5){
  for(i in 1:n.test){
    ystarpred[i,j]=ystar.bayes[(j-1)*n.test+i]
  }
}

testdates=ar1data_week$date[(n.train+1):(n.train+n.test)]
#x11()
layout(matrix(1:5,5,1,byrow=T))
for(j in 1:5){
  plot(testdates,ytest[,j],xlab='Date',ylab='Units',col = 'green4', type='l')
  lines(testdates,ystarpred[,j],col = 'blue')
}

error=ytest-ystarpred
MSE=1/50*sum(sum(error^2))
MSE



