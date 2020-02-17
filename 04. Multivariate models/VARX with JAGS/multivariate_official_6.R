library(chron)
library(tidyverse)
library(data.table)
library(dplyr)
library(rjags)
library(coda) 
library(plotrix)
library(bsts)

load("../../Dataset_pronti/for_multivariate/ar6data.RData")
ar6data <- ar6data[-1,]

### multivariate_wishart.bug
yorig=ar6data[,2:6]
means=colMeans(yorig)
sds=apply(yorig,2,sd)
ar6data[,c(2,3,4,5,6,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69)]<-scale(ar6data[,c(2,3,4,5,6,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69)])

n.train=300
n.test=10
ar6data.train=ar6data[1:n.train,]
ar6data.test=ar6data[(n.train+1):(n.train+n.test),]
ytest.orig=yorig[(n.train+1):(n.train+n.test),]



data = list(y = ar6data[,c(2,3,4,5,6)],
            x1 = ar6data[,c(17,18,19,20,21)],
            x2 = ar6data[,c(22,23,24,25,26)],
            x3 = ar6data[,c(32,33,34,35,36)],
            x4 = ar6data[,c(37,37,37,37,37)],
            x5 = ar6data[,c(38,38,38,38,38)],
            z1 = ar6data[,c(40,41,42,43,44)],
            z2 = ar6data[,c(45,46,47,48,49)],
            z3 = ar6data[,c(50,51,52,53,54)],
            z4 = ar6data[,c(55,56,57,58,59)],
            z5 = ar6data[,c(60,61,62,63,64)],
            z6 = ar6data[,c(65,66,67,68,69)],
            n = dim(ar6data)[1],
            m = 5,
            R=rbind(c(5,0,0,0,0),c(0,5,0,0,0), c(0,0,5,0,0), c(0,0,0,5,0), c(0,0,0,0,5)),
            x1star = ar6data.test[,c(17,18,19,20,21)],
            x2star = ar6data.test[,c(22,23,24,25,26)],
            x3star = ar6data.test[,c(32,33,34,35,36)],
            x4star = ar6data.test[,c(37,37,37,37,37)],
            x5star = ar6data.test[,c(38,38,38,38,38)],
            zstar1 = ar6data.test[,c(40,41,42,43,44)],
            zstar2 = ar6data.test[,c(45,46,47,48,49)],
            zstar3 = ar6data.test[,c(50,51,52,53,54)],
            zstar4 = ar6data.test[,c(55,56,57,58,59)],
            zstar5 = ar6data.test[,c(60,61,62,63,64)],
            zstar6 = ar6data.test[,c(65,66,67,68,69)],
            nstar = dim(ar6data.test)[1])

modelRegress=jags.model("multivariate_wishart_6.bug",data=data,n.adapt=1000,n.chains=1)
update(modelRegress,n.iter=10000)

variable.names=c("a1", "a2", "a3", "a4", "a5", "a6", "b1","b2","b3","b4","b5", "tau","ystar")
n.iter=50000 
thin=10

outputRegress=coda.samples(model=modelRegress,variable.names=variable.names,n.iter=n.iter,thin=thin)
data.out_wish=as.matrix(outputRegress) 
data.out_wish=data.frame(data.out_wish) 
head(data.out_wish)
save(data.out_wish,list="data.out_wish",file="output/data.out_wish_6.RData")

alpha1.post <- data.out_wish[,1:25]
alpha2.post <- data.out_wish[,26:50]
alpha3.post <- data.out_wish[,51:75]
alpha4.post <- data.out_wish[,76:100]
alpha5.post <- data.out_wish[,101:125]
alpha6.post <- data.out_wish[,126:150]
beta.post <- data.out_wish[,151:175]
sigma.post <- data.out_wish[,176:200]
ystar.post <- data.out_wish[,201:250]


#posterior mean of the beta parameters
alpha1.bayes <- apply(alpha1.post,2,"mean")
alpha1.bayes
alpha2.bayes <- apply(alpha2.post,2,"mean")
alpha2.bayes
alpha3.bayes <- apply(alpha3.post,2,"mean")
alpha3.bayes
alpha4.bayes <- apply(alpha4.post,2,"mean")
alpha4.bayes
alpha5.bayes <- apply(alpha5.post,2,"mean")
alpha5.bayes
alpha6.bayes <- apply(alpha6.post,2,"mean")
alpha6.bayes
beta.bayes  <- apply(beta.post,2,"mean")
beta.bayes
sigma.bayes  <- apply(sigma.post,2,"mean")
sigma.bayes
ystar.bayes  <- apply(ystar.post,2,"mean")
ystar.bayes

ytest=ar6data.test[,2:6]

layout(rbind(1:5,6:10,11:15))

plot(beta.post[,11],type="l",main="Store 2",ylab = 'beta_snow')
plot(beta.post[,12],type="l",main="Store 21",ylab = 'beta_snow')
plot(beta.post[,13],type="l",main="Store 30",ylab = 'beta_snow')
plot(beta.post[,14],type="l",main="Store 33",ylab = 'beta_snow')
plot(beta.post[,15],type="l",main="Store 36",ylab = 'beta_snow')

acf(beta.post[,11],lwd=3,col="red3",main='')
acf(beta.post[,12],lwd=3,col="red3",main='')
acf(beta.post[,13],lwd=3,col="red3",main='')
acf(beta.post[,14],lwd=3,col="red3",main='')
acf(beta.post[,15],lwd=3,col="red3",main='')

hist(beta.post[,11],nclass="fd",freq=F,col="gray",main='',xlab = 'beta_snow') 
lines(density(beta.post[,11]),col="blue",lwd=2)
abline(v=quantile(beta.post[,11],prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(beta.post[,11],prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(beta.post[,11],prob=c(0.975)),col="red",lty=2,lwd=2)

hist(beta.post[,12],nclass="fd",freq=F,col="gray",main='',xlab = 'beta_snow') 
lines(density(beta.post[,12]),col="blue",lwd=2)
abline(v=quantile(beta.post[,12],prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(beta.post[,12],prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(beta.post[,12],prob=c(0.975)),col="red",lty=2,lwd=2)

hist(beta.post[,13],nclass="fd",freq=F,col="gray",main='',xlab = 'beta_snow') 
lines(density(beta.post[,13]),col="blue",lwd=2)
abline(v=quantile(beta.post[,13],prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(beta.post[,13],prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(beta.post[,13],prob=c(0.975)),col="red",lty=2,lwd=2)

hist(beta.post[,14],nclass="fd",freq=F,col="gray",main='',xlab = 'beta_snow') 
lines(density(beta.post[,14]),col="blue",lwd=2)
abline(v=quantile(beta.post[,14],prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(beta.post[,14],prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(beta.post[,14],prob=c(0.975)),col="red",lty=2,lwd=2)

hist(beta.post[,15],nclass="fd",freq=F,col="gray",main='',xlab = 'beta_snow') 
lines(density(beta.post[,15]),col="blue",lwd=2)
abline(v=quantile(beta.post[,15],prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(beta.post[,15],prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(beta.post[,15],prob=c(0.975)),col="red",lty=2,lwd=2)




par(mfrow=c(1,2))

hist(beta.post[,11],nclass="fd",freq=F,main="In a NON Snowy Area",col="gray") 
lines(density(beta.post[,11]),col="forestgreen",lwd=2)
abline(v=quantile(beta.post[,11],prob=c(0.025)),col="coral",lty=2,lwd=2)
abline(v=quantile(beta.post[,11],prob=c(0.5)),col="coral",lty=1,lwd=3)
abline(v=quantile(beta.post[,11],prob=c(0.975)),col="coral",lty=2,lwd=2)

hist(beta.post[,13],nclass="fd",freq=F,main="In a Snowy Area",col="gray") 
lines(density(beta.post[,13]),col="dodgerblue1",lwd=2)
abline(v=quantile(beta.post[,13],prob=c(0.025)),col="cyan",lty=2,lwd=2)
abline(v=quantile(beta.post[,13],prob=c(0.5)),col="cyan",lty=1,lwd=3)
abline(v=quantile(beta.post[,13],prob=c(0.975)),col="cyan",lty=2,lwd=2)



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

testdates=ar6data$date[(n.train+1):(n.train+n.test)]
#x11()
layout(matrix(1:5,5,1,byrow=T))
for(j in 1:5){
  plot(testdates,ytest[,j],xlab='Date',ylab='Units',col = 'green4', type='l')
  lines(testdates,ystarpred[,j],col = 'blue')
}

error=ytest-ystarpred
MSE=1/50*sum(sum(error^2))
MSE

###################################

index <- rbind(1:5,6:10,11:15,16:20,21:25)

z1 <- data$z1[1:n.train,]
z2 <- data$z1[1:n.train,]
z3 <- data$z1[1:n.train,]
z4 <- data$z1[1:n.train,]
z5 <- data$z1[1:n.train,]
z6 <- data$z1[1:n.train,]

x1 <- data$x1[1:n.train,]
x2 <- data$x1[1:n.train,]
x3 <- data$x1[1:n.train,]
x4 <- data$x1[1:n.train,]
x5 <- data$x1[1:n.train,]

mu <- matrix(0,data$m,n.train)

for(j in 1:data$m){
  for(i in 1:n.train){
    
    a1 <- alpha1.bayes[(data$m*(j-1)+1):(data$m*j)]
    a2 <- alpha2.bayes[(data$m*(j-1)+1):(data$m*j)]
    a3 <- alpha3.bayes[(data$m*(j-1)+1):(data$m*j)]
    a4 <- alpha4.bayes[(data$m*(j-1)+1):(data$m*j)]
    a5 <- alpha5.bayes[(data$m*(j-1)+1):(data$m*j)]
    a6 <- alpha6.bayes[(data$m*(j-1)+1):(data$m*j)]
    
    b1 <- beta.bayes[j]
    b2 <- beta.bayes[data$m + j]
    b3 <- beta.bayes[2 * data$m + j]
    b4 <- beta.bayes[3 * data$m + j]
    b5 <- beta.bayes[4 * data$m + j]
    
    mu[j,i] <- as.vector(as.numeric(z1[i,])) %*% as.vector(a1) 
    + as.vector(as.numeric(z2[i,])) %*% as.vector(a2) 
    + as.vector(as.numeric(z3[i,])) %*% as.vector(a3) 
    + as.vector(as.numeric(z4[i,])) %*% as.vector(a4)
    + as.vector(as.numeric(z5[i,])) %*% as.vector(a5)
    + as.vector(as.numeric(z6[i,])) %*% as.vector(a6) 
    + x1[i,j] * b1 + x2[i,j] * b2 + x3[i,j] * b3 + x4[i,j] * b4 + x5[i,j] * b5
    
  }
}

y <- data$y[1:n.train,]

for (i in 1:data$m) {
  plot(1:n.train,y[,i], type = 'l', col = 'red', lwd = 1, lty = 1)
  lines(1:n.train,mu[i,], type = 'l')  
}

new_index <- rbind(1:10,11:20,21:30,31:40,41:50)
new_title <- c('Store 2','Store 21','Store 30','Store 33','Store 36')

layout(rbind(c(1,2),c(3,5),c(4,4)))
for (i in 1:data$m) {
  ypred <- ystar.post[,new_index[i,]]
  PlotDynamicDistribution(ypred,
                          timestamps = 1:n.test,
                          quantile.step=.05,
                          xlim = NULL,
                          xlab = "Time",
                          ylim=c(-4,4),
                          ylab = "distribution",
                          add=FALSE, main = new_title[i])
  lines(1:n.test,ytest[,i],type = 'l',lty = 6,lwd = 3,col = 'coral1')
}

par(mfrow=c(1,1))

ypred4 <- ystar.post[,new_index[4,]]
PlotDynamicDistribution(ypred4,
                        timestamps = 1:n.test,
                        quantile.step=.05,
                        xlim = NULL,
                        xlab = "Time",
                        ylim=c(-4,4),
                        ylab = "distribution",
                        add=FALSE, main='Predictive vs Observations: Store 33')
lines(1:n.test,ytest[,4],type = 'l',lty = 6,lwd = 3,col = 'coral1')


MSE_vec <- c(0.929818,0.9882739,1.124757,1.01433,0.8137718,0.7080111,0.7584223)
plot(1:7,MSE_vec,col='lightseagreen',type = 'b',lwd=4,lty=1,ylim = c(0.65,1.2),
     xlab = 'AR Order', ylab = 'Predictive MSE', main = 'Selection of the AR model')
for (i in 1:7) {
  lines(c(i,i),c(0,MSE_vec[i]),col='grey',lwd=1,lty=2)
}
lines(6,MSE_vec[6],col='coral3',type = 'o',lwd=25)



layout(matrix(1:n.test,2,5,byrow=T))
for(i in 1:n.test){
  chain <- ystar.post.rescaled[,(4-1)*n.test+i]
  title=paste("Day ",i,sep="")
  hist(chain,nclass="fd",freq=F,main=title,xlim=c(0,400),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
  abline(v=ytest.orig[i,4],col="green4",lty=1,lwd=2)
}



