
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
last_date='01/01/13'
item=44
store=30#4
niter=2000
station=key$station_nbr[which(key$store_nbr==store)]

items_dataset=unit_series(train,item=item,last_date = last_date,store=store)
regressors_dataset=weather_series(data=weather,station=station,features =c('preciptotal','snowfall','resultspeed','rain','snow'),last_date=last_date)
working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)

working_dataset$units=scale(working_dataset$units)

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

#Holidays
july4 <- FixedDateHoliday("July4", "July", 4)
christmas <- FixedDateHoliday("Christmas", "Dec", 25)


#BSTS
#THE EXAMPLE FROM THE PACKAGE

# data(iclaims)
# .data <- initial.claims
# claims <- .data$iclaimsNSA
# plot(claims, ylab = "")
# 
# (model_components <- list())
# 
# summary(model_components <- AddLocalLinearTrend(model_components, 
#                                                 y = claims))
# summary(model_components <- AddSeasonal(model_components, y = claims, 
#                                         nseasons  = 52))
# fit <- bsts(claims, model_components, niter = 2000)
# burnin <- 500 # Throw away first 500 
# tibble(
#   date = as.Date(time(claims)),
#   trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
#   seasonality = colMeans(fit$state.contributions[-(1:burnin),"seasonal.52.1",])) %>%
#   gather("component", "value", trend, seasonality) %>%
#   ggplot(aes(x = date, y= value)) + 
#   geom_line() + theme_bw() + 
#   theme(legend.title = element_blank()) + ylab("") + xlab("") +
#   facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
#   theme(axis.text.x=element_text(angle = -90, hjust = 0))



#Constructiong zoo objects for time series
y=zoo(x=working_dataset$units,order.by = as.Date(working_dataset$date)) #Problema di Natale
#y=zoo(x=working_dataset$units)


#Adding model components
(model_components <- list())
summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = y))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 7))
summary(model_components <- AddSeasonal(model_components, y = y, 
                                        nseasons  = 30))
summary(model_components <- AddMonthlyAnnualCycle(model_components,
                                          y=y))
summary(model_components <- AddAr(model_components, 
                                      y = y,lags = 4,sigma.prior=SdPrior(0.5, 1)))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                      y = y,holiday=july4))
summary(model_components <- AddRandomWalkHoliday(model_components, 
                                                 y = y,holiday=christmas))

sd(y)



#Model fitting: Only time series
options=BstsOptions(save.state.contributions = TRUE,
                    save.prediction.errors = TRUE,
                    bma.method = c("SSVS", "ODA"),
                    oda.options = list(
                      fallback.probability = 0.0,
                      eigenvalue.fudge.factor = 0.01),
                    timeout.seconds = Inf,
                    save.full.state = FALSE)
fitold <- bsts(y, model_components, niter = niter,model.options = options)

save(fit,list="fit",file="BSTS_44_30_firty.RData")

#burnin <- 500 # Throw away first 500 
burnin=SuggestBurn(.1, fit)
tibble(
  date = as.Date(time(y)),
  trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
  seasonality = colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x = date, y= value)) + 
  geom_line() + theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#par(mfrow=c(4,1))
colnames(fit$state.contributions)
par(mfrow=c(1,1))
plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')


lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"trend",]),col='red')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]),col='blue')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"seasonal.30.1",]),col='orange')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Monthly",]),col='black')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Ar4",]),col='black')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"July4",]),col='red')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"Christmas",]),col='red')

plot(fit,burn=burnin)
lines(working_dataset$date,working_dataset$units,col = 'green4')
allcontribs=colMeans(fit$state.contributions[-(1:burnin),"Christmas",])+colMeans(fit$state.contributions[-(1:burnin),"July4",])+colMeans(fit$state.contributions[-(1:burnin),"Ar4",])+colMeans(fit$state.contributions[-(1:burnin),"trend",])+colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",])+colMeans(fit$state.contributions[-(1:burnin),"seasonal.30.1",])+colMeans(fit$state.contributions[-(1:burnin),"Monthly",])
#lines(working_dataset$date,allcontribs,col='red')
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

plot(fit,y="forecast.distribution")
PlotBstsComponents(fit,burn = burnin)

err=working_dataset$units-allcontribs
plot(working_dataset$date,err,type='l')
plot(working_dataset$date,err)
hist(err,nclass=100,freq=F,main="Distribution of the error",col="gray")
working_dataset$date[which(err==min(err))]

horizon=100
pred <- predict(fit, horizon = horizon, burn = burnin, quantiles = c(.05, .95))
plot(pred)
lines(unit_series(train,item=item,store=store)$date[1:(366+horizon)],unit_series(train,item=item,store=store)$unit[1:(366+horizon)],col = 'green4', type='l')
lines(unit_series(train,item=item,store=store)$date[1:(366+horizon)],colMeans(fit$state.contributions[-(1:burnin),"trend",])+colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]),col='firebrick3')

#Saving the error for Jack
#save(err,list="err",file="Residuals/err44_2.RData")


# #Model fitting: Adding regressors
# data.zoo=zoo(working_dataset[,-1],order.by = as.Date(working_dataset$date))
# 
# fit2 <- bsts(units ~ ., state.specification = model_components, 
#              data = data.zoo, niter = 1000)
# 
# colMeans(fit2$coefficients)
# 
# summary(fit2)
# 
# PlotBstsCoefficients(fit2, burn = burnin,
#                      inclusion.threshold = 0, number.of.variables = NULL)
# 
# coeff=fit2$coefficients

#I do something that Jack does
chain <- fit$coefficients[,'snowfall']
layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(chain,type="l",main="Trace plot of a0")
acf(chain,lwd=3,col="red3",main="autocorrelation of a0")
hist(chain,nclass="fd",freq=F,main="Posterior of a0",col="gray") 
lines(density(chain),col="blue",lwd=2)
abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)

for(i in 1:4)
{
  chain <- fitold$AR4.coefficients[-(1:burnin),i]
  layout(matrix(c(1,2,3,3),2,2,byrow=T))
  plot(chain,type="l",main=paste("Trace plot of a",i))
  acf(chain,lwd=3,col="red3",main=paste("autocorrelation of a",i))
  hist(chain,nclass="fd",freq=F,main=paste("Posterior of a",i),col="gray") 
  lines(density(chain),col="blue",lwd=2)
  abline(v=quantile(chain,prob=c(0.025)),col="red",lty=2,lwd=2)
  abline(v=quantile(chain,prob=c(0.5)),col="red",lty=1,lwd=2)
  abline(v=quantile(chain,prob=c(0.975)),col="red",lty=2,lwd=2)
}

#Full state space plot
par(mfrow=c(4,3))
for(i in 1:61){
  chain <- colMeans(fit$full.state[-(1:burnin),i,1:366])
  plot(chain,type="l",main=paste("Mean behaviour of state",i))
}



