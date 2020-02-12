datatest <- read.table('test.csv', header = TRUE, sep = ',')
names(table(datatest$date))
weather <- read.table('weather.csv', header = TRUE, sep = ',')
weather_temp <- read.table('weather_temp.txt', header = TRUE)

index <- rep(1,526917/111)
for (i in 1:(526917/111)){
  index[i] <- index[i] + (i-1)*111
}
datestore <- datatest[index,1:2]

key <- read.table('key.csv', header = TRUE, sep = ',')

station <- rep(0,dim(datestore)[1])
for (i in 1:dim(datestore)[1]){
  if (datestore[i,2] == 1){
    station[i] = 1
  }
  if (datestore[i,2] == 16){
    station[i] = 2
  }
  if (datestore[i,2] == 21 | datestore[i,2] == 29 | datestore[i,2] == 33){
    station[i] = 3
  }
  if (datestore[i,2] == 8){
    station[i] = 4
  }
  if (datestore[i,2] == 35){
    station[i] = 5
  }
  if (datestore[i,2] == 7 | datestore[i,2] == 13){
    station[i] = 6
  }
  if (datestore[i,2] == 3 | datestore[i,2] == 20 | datestore[i,2] == 28){
    station[i] = 7
  }
  if (datestore[i,2] == 39){
    station[i] = 8
  }
  if (datestore[i,2] == 4 | datestore[i,2] == 24){
    station[i] = 9
  }
  if (datestore[i,2] == 11 | datestore[i,2] == 22 | datestore[i,2] == 27){
    station[i] = 10
  }
  if (datestore[i,2] == 12 | datestore[i,2] == 43){
    station[i] = 11
  }
  if (datestore[i,2] == 5 | datestore[i,2] == 10 | datestore[i,2] == 41 | datestore[i,2] == 44){
    station[i] = 12
  }
  if (datestore[i,2] == 15 | datestore[i,2] == 25 | datestore[i,2] == 32 | datestore[i,2] == 37 | datestore[i,2] == 40){
    station[i] = 13
  }
  if (datestore[i,2] == 2 | datestore[i,2] == 6 | datestore[i,2] == 38 | datestore[i,2] == 42){
    station[i] = 14
  }
  if (datestore[i,2] == 19){
    station[i] = 15
  }
  if (datestore[i,2] == 14 | datestore[i,2] == 45){
    station[i] = 16
  }
  if (datestore[i,2] == 9 | datestore[i,2] == 18 | datestore[i,2] == 23 | datestore[i,2] == 26 | datestore[i,2] == 31 | datestore[i,2] == 34){
    station[i] = 17
  }
  if (datestore[i,2] == 36){
    station[i] = 18
  }
  if (datestore[i,2] == 30){
    station[i] = 19
  }
  if (datestore[i,2] == 17){
    station[i] = 20
  }
}

date_store_station <- data.frame(cbind(datestore,station))

library(dplyr)

data_ex <- weather_temp %>% filter(weather_temp$date == as.character(date_store_station[1,1]) & weather_temp$station_nbr == date_store_station[1,3])

A <- data_ex

for (i in 2:dim(date_store_station)[1]){
  data_ex <- weather_temp %>% filter(weather_temp$date == as.character(date_store_station[i,1]) & weather_temp$station_nbr == date_store_station[i,3])
  A <- rbind(A,data_ex)
}

A = A[,3:26]

weatherTEST <- data.frame(cbind(date_store_station,A))

#write.table(weatherTEST, file = "./weatherTEST.txt")



