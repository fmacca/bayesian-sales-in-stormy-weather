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

meteo <- meteo[which(meteo$date<chron(dates="01/01/14", format=c(dates="d/m/y")) & meteo$date>chron(dates="31/12/12", format=c(dates="d/m/y"))),]
# introduce weekends
sett <- c(0,0,0,0,1,1,0)
weekend <- c(rep(sett,52),0)
#weekend <- weekend[-360]
meteo <- data.frame(meteo,weekend)

meteo_temp <- meteo_temp[367:731,]

train <- train[366:677,]

# integrate train dataset (units sold of product 91 in store 21) with meteo 
index <- rep(0,53)
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
mydata <- mydata[,c(4,6:9,11,12,16:18,21:31)]

# introduce holidays
holiday <- rep(0,312) 
for (i in 1:dim(mydata)[1]) {
  if (mydata$date.1[i]==chron(dates="01/01/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="21/01/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="14/02/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="18/02/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="31/03/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="12/05/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="27/05/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="16/06/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="04/07/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="02/09/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="14/10/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="31/10/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="11/11/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="28/11/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="24/12/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="25/12/13", format=c(dates="d/m/y")) |
      mydata$date.1[i]==chron(dates="31/12/13", format=c(dates="d/m/y"))) {
    holiday[i] = 1
  }
}  
mydata <- data.frame(mydata,holiday)

# introduce blackfriday  
#blackfriday <- rep(0,365)  
#blackfriday[which(mydata$date.1<chron(dates="27/11/12", format=c(dates="d/m/y")) & 
#                    mydata$date.1>chron(dates="19/11/12", format=c(dates="d/m/y")))] = rep(1,7) 
#mydata <- data.frame(mydata,blackfriday)

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
# check
mydata$date[which(mydata$holiday == 1)]
mydata$date[which(holiday == 1)]
# --> OK

# remove second to last row (only NA values)
mydata <- mydata[-311,]

# is there any NA value?
sum(is.na(mydata)) # --> 6

NAind <- matrix(0,6,2)
k <- 1
for (i in 1:dim(mydata)[1]) {
  for (j in 1:dim(mydata)[2]) {
    if (is.na(mydata[i,j]) == TRUE) {
      NAind[k,1] <- i
      NAind[k,2] <- j
      k <- k + 1
    }
  }
}
NAind

# Substitute NA values in tavg with the mean of tmax and tmin
indna <- which(is.na(mydata$tavg) == TRUE)
mydata$tavg[indna] <- (mydata$tmin[indna] + mydata$tmax[indna])/2 

write.table(mydata, file = "test_9121.csv")
