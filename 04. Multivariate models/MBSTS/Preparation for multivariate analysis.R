#Pacchetti
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

## SETTINGS
last_date='01/01/13'
item=44
features=c('preciptotal','snowfall','tavg','avgspeed','rain','snow') #weather related local features (different in each store every day)

# Adding units for nonzero time series
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
    regressors_dataset=weather_series(data=weather,station=station,features = features,last_date=last_date)
    working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)
    
    working_dataset$units[which(working_dataset$date=='25/12/12')]=0
    
    actually=actually+1
    if(actually==6 || actually== 11)
    {
      x11()
      layout(matrix(1:5,5,1,byrow=T))
    }
    plot(working_dataset$date,working_dataset$units,xlab='Date',ylab='Units',main=paste('Product',item,'in store',store,": units"),col = 'green4', type='l')
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
nonzeros=actually
actually=0


# Adding station related weather features (indexed by store belonging)
for(feat in features)
{
  print(paste("Adding",feat))
  loc_actually=0
  x11()
  layout(matrix(1:5,5,1,byrow=T))
  for(i in 1:45){
    store=i
    if(wiw[item,store]=='!')
    {
      station=key$station_nbr[which(key$store_nbr==store)]
      items_dataset=unit_series(train,item=item,last_date = last_date,store=store)
      regressors_dataset=weather_series(data=weather,station=station,features = features,last_date=last_date)
      working_dataset=merge(y=items_dataset,x=regressors_dataset,by='date',all.x=TRUE)
      
      working_dataset$units[which(working_dataset$date=='25/12/12')]=0
      
      actually=actually+1
      loc_actually=loc_actually+1
      if(loc_actually==6 || loc_actually== 11)
      {
        x11()
        layout(matrix(1:5,5,1,byrow=T))
      }
      plot(working_dataset$date,working_dataset[,feat],xlab='Date',ylab='Units',main=paste('Product',item,'in store',store,":",feat),col = 'blue', type='l')

      working_full[,nonzeros+actually+1]=working_dataset[,feat]
      cn=c(cn,paste(feat,store,sep='_'))
    }
  }
  colnames(working_full)=cn
  
}

# Adding global features (that are the same in every store so only one column per feature) (ex. holidays)
# introduce weekends
sett <- c(1,0,0,0,0,0,1)
weekend <- c(rep(sett,52),1,0)
mydata <- data.frame(working_full,weekend)
# introduce holidays
holiday <- rep(0,366) 
for (i in 1:dim(mydata)[1]) {
  if (mydata$date[i]==chron(dates="01/01/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="16/01/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="14/02/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="20/02/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="08/04/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="13/05/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="28/05/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="17/06/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="04/07/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="03/09/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="08/10/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="31/10/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="11/11/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="22/11/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="24/12/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="25/12/12", format=c(dates="d/m/y")) |
      mydata$date[i]==chron(dates="31/12/12", format=c(dates="d/m/y"))) {
    holiday[i] = 1
  }
}  
mydata <- data.frame(mydata,holiday)
# Black Friday
blackfriday <- rep(0,366)  
blackfriday[which(mydata$date<chron(dates="27/11/12", format=c(dates="d/m/y")) & 
                    mydata$date>chron(dates="19/11/12", format=c(dates="d/m/y")))] = rep(1,7) 
mydata <- data.frame(mydata,blackfriday)

save(mydata,list="mydata",file="../../Dataset_pronti/mydata.RData")

# Some useful analysis
cov(working_full[,2:6])
cor(working_full[,2:6])

cor(working_full[,2:31])



