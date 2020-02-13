
#Pacchetti
library(bsts)
library(chron)
library(tidyverse)

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
item=68
store=27
station=key$station_nbr[which(key$store_nbr==store)]

items_dataset=unit_series(train,item=item,last_date = last_date,store=store)
regressors_dataset=weather_series(data=weather,station=station,features =c('tavg','tmax','tmin'),last_date=last_date)
working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)


plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
hist(working_dataset$units)
boxplot(working_dataset$units)

#Fixing NAs in the regressors
working_dataset$tavg[which(is.na(working_dataset$tavg))]=(working_dataset$tmax[which(is.na(working_dataset$tavg))]+working_dataset$tmin[which(is.na(working_dataset$tavg))])/2
working_dataset$units[which(working_dataset$date=='25/12/12')]=0
#working_dataset$tavg=c(0,diff(working_dataset$tavg)) #Taking the difference in avg(temp)

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


#Model fitting: Only time series
fit <- bsts(y, model_components, niter = 2000)

burnin <- 500 # Throw away first 500 
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

plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"trend",]))
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]))

plot(working_dataset$date,working_dataset$units,col = 'green4', type='l')
lines(working_dataset$date,colMeans(fit$state.contributions[-(1:burnin),"trend",])+colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]))
lines(working_dataset$date,rep(0,length(working_dataset$date)),col='red')

horizon=100
pred <- predict(fit, horizon = horizon, burn = burnin, quantiles = c(.05, .95))
plot(pred)
lines(unit_series(train,item=item,store=store)$date[1:(366+horizon)],unit_series(train,item=item,store=store)$unit[1:(366+horizon)],col = 'green4', type='l')
lines(unit_series(train,item=item,store=store)$date[1:(366+horizon)],colMeans(fit$state.contributions[-(1:burnin),"trend",])+colMeans(fit$state.contributions[-(1:burnin),"seasonal.7.1",]),col='firebrick3')

#Model fitting: Adding regressors
data.zoo=zoo(working_dataset[,c(2,5)],order.by = as.Date(working_dataset$date))

fit2 <- bsts(units ~ ., state.specification = model_components, 
             data = data.zoo, niter = 1000)

colMeans(fit2$coefficients)

summary(fit2)

PlotBstsCoefficients(fit2, burn = burnin,
                     inclusion.threshold = 0, number.of.variables = NULL)

coeff=fit2$coefficients


