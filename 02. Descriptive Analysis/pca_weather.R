# I try to perform a PCA on the weather dataset
library(chron)

load("../Dataset_pronti/weather_temp.RData")
to_pca=c(3:8,11:17)
data=weather[,to_pca]



n <- dim(data)[1]
p <- dim(data)[2]


data.sd <- data #scale(data)
data.sd <- data.frame(data.sd)

pc.weather <- princomp(na.omit(data.sd), scores=T,)
pc.weather
summary(pc.weather)

# explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.weather$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(data.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(pc.weather$sdev^2)/sum(pc.weather$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.sd),labels=1:ncol(data.sd),las=2)


# scores
scores.weather <- pc.weather$scores
scores.weather


# loadings
load.weather    <- pc.weather$loadings
load.weather

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3)barplot(load.weather[,i], ylim = c(-1, 1))

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 4:6)barplot(load.weather[,i], ylim = c(-1, 1))