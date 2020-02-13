#The function to correctly load the "weather_temp.txt" dataset

load_weather_temp <- function() {
  data=read.table("weather_temp.txt",colClasses = "character")
  for(indx in colnames(data))
  {
    data[which(data[,indx]=='M'),indx]=NA
    data[which(data[,indx]=='  T'),indx]=0.005
    data[which(data[,indx]=='-'),indx]=NA
  }
  
  
  data$tmax=as.numeric(data$tmax)
  data$tmin=as.numeric(data$tmin)
  data$tavg=as.numeric(data$tavg)
  data$depart=as.numeric(data$depart)
  data$dewpoint=as.numeric(data$dewpoint)
  data$wetbulb=as.numeric(data$wetbulb)
  data$snowfall=as.numeric(data$snowfall)
  data$preciptotal=as.numeric(data$preciptotal)
  data$stnpressure=as.numeric(data$stnpressure)
  data$sealevel=as.numeric(data$sealevel)
  data$resultspeed=as.numeric(data$resultspeed)
  data$resultdir=as.numeric(data$resultdir)*10
  data$avgspeed=as.numeric(data$avgspeed)
  for(indx in 18:26){
    data[,indx]=as.integer(data[,indx])
  }
  
  library(chron)
  data[,2]=chron(dates=data[,2], format=c(dates="y-m-d"),out.format=c(dates="d/m/y"))
  #time=chron(times=data$sunrise, format=c(times="hm"),out.format =c(times="h:m" ))
  
  return(data)
}

weather=load_weather_temp()

save(weather,list="weather",file="Dataset_pronti/weather_temp.RData")


