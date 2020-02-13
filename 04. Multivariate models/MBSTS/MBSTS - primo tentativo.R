

#Pacchetti
library(bsts)
library(chron)
library(tidyverse)
library(data.table)

#Dataset
load('../../Dataset_pronti/weather_temp.RData')
load('../../Dataset_pronti/train.RData')
key=read.table('../../Data/key.csv',sep=',',header=TRUE)
wiw=read.table('../../Dataset_pronti/which_in_which.csv',sep=',',header=TRUE)

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

summary(train)
#Preliminary steps
#We try first with item 19 in store 32
last_date='01/01/13'
item=44
niter=2000
x11()
layout(matrix(1:5,5,1,byrow=T))
actually=0
working_full=data.frame()
for(i in 1:45){
  store=i
  if(wiw[item,store]=='!')
  {
    station=key$station_nbr[which(key$store_nbr==store)]
    items_dataset=unit_series(train,item=item,last_date = last_date,store=store)
    regressors_dataset=weather_series(data=weather,station=station,features =c('preciptotal','snowfall','resultspeed','rain','snow'),last_date=last_date)
    working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)
    
    working_dataset$units[which(working_dataset$date=='25/12/12')]=0
    
    actually=actually+1
    if(actually==6 || actually== 11)
    {
      x11()
      layout(matrix(1:5,5,1,byrow=T))
    }
    plot(working_dataset$date,working_dataset$units,xlab='Date',ylab='Units',main=paste('Product',item,'in store',store),col = 'green4', type='l')
    if(actually==1)
    {
      working_full=working_dataset[,c(1,7)]
      cn=c('date',paste('units',store,sep='_'))
    }
    else
    {
      working_full[,actually+1]=working_dataset[,7]
      cn=c(cn,paste('units',store,sep='_'))
    }
  }
}
colnames(working_full)=cn

y=zoo(x=working_full[,2:6],order.by = as.Date(working_dataset$date))

# Model 0: Only linear trend
order=0
(model_components_shared <- list())
summary(model_components_shared <- AddSharedLocalLevel(model_components_shared, 
                                                response = y,
                                                nfactors = 5))
fit0 <- mbsts(y,shared.state.specification = model_components_shared, niter = niter,data.format = "wide")

plot(fit0)

