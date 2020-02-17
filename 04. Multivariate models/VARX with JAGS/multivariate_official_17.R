library(chron)
library(tidyverse)
library(data.table)
library(dplyr)
library(rjags)
library(coda) 
library(plotrix)

load("../../Dataset_pronti/for_multivariate/ar7data.RData")

### multivariate_wishart.bug
yorig=ar7data[,2:6]
means=colMeans(yorig)
sds=apply(yorig,2,sd)
ar7data[,c(2,3,4,5,6,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74)]<-scale(ar7data[,c(2,3,4,5,6,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74)])

n.train=299
# dovrebbe essere 300
n.test=10
ar7data.train=ar7data[1:n.train,]
ar7data.test=ar7data[(n.train+1):(n.train+n.test),]
ytest.orig=yorig[(n.train+1):(n.train+n.test),]



data = list(y = ar7data[,c(2,3,4,5,6)],
            x1 = ar7data[,c(17,18,19,20,21)],
            x2 = ar7data[,c(22,23,24,25,26)],
            x3 = ar7data[,c(32,33,34,35,36)],
            x4 = ar7data[,c(37,37,37,37,37)],
            x5 = ar7data[,c(38,38,38,38,38)],
            z1 = ar7data[,c(40,41,42,43,44)],
            z7 = ar7data[,c(70,71,72,73,74)],
            n = dim(ar7data)[1],
            m = 5,
            R=rbind(c(5,0,0,0,0),c(0,5,0,0,0), c(0,0,5,0,0), c(0,0,0,5,0), c(0,0,0,0,5)),
            x1star = ar7data.test[,c(17,18,19,20,21)],
            x2star = ar7data.test[,c(22,23,24,25,26)],
            x3star = ar7data.test[,c(32,33,34,35,36)],
            x4star = ar7data.test[,c(37,37,37,37,37)],
            x5star = ar7data.test[,c(38,38,38,38,38)],
            zstar1 = ar7data.test[,c(40,41,42,43,44)],
            zstar7 = ar7data.test[,c(70,71,72,73,74)],
            nstar = dim(ar7data.test)[1])

modelRegress=jags.model("multivariate_wishart_17.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("a1", "a7", "b1","b2","b3","b4","b5", "tau","ystar")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out_wish=as.matrix(outputRegress) 
data.out_wish=data.frame(data.out_wish) 
head(data.out_wish)
save(data.out_wish,list="data.out_wish",file="output/data.out_wish_17.RData")

alpha1.post <- data.out_wish[,1:25]
alpha7.post <- data.out_wish[,26:50]
beta.post <- data.out_wish[,51:75]
sigma.post <- data.out_wish[,76:100]
ystar.post <- data.out_wish[,101:150]


#posterior mean of the beta parameters
alpha1.bayes <- apply(alpha1.post,2,"mean")
alpha1.bayes
alpha7.bayes <- apply(alpha7.post,2,"mean")
alpha7.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes
sigma.bayes  <- apply(sigma.post,2,"mean")
sigma.bayes
ystar.bayes  <- apply(ystar.post,2,"mean")
ystar.bayes

ytest=ar7data.test[,2:6]

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

testdates=ar7data$date[(n.train+1):(n.train+n.test)]
#x11()
#layout(matrix(1:5,5,1,byrow=T))
par(mfrow=c(5,2))
for(j in 1:5){
  plot(testdates,ytest[,j],xlab='Date',ylab='Units',col = 'green4', type='l')
  lines(testdates,ystarpred[,j],col = 'blue')
}

error=ytest-ystarpred
MSE=1/50*sum(sum(error^2))




