library(dplyr)
weather <- read.table('weather_temp.txt', header = TRUE)

for (i in 1:20517){
  if (weather[i,5] == 'M'){
    weather[i,5] <- NA
  }
}

st <- matrix(0,1035,20)

for (i in 1:20){
  if (i != 5){
    st[,i] <- as.vector(as.numeric(as.character((weather %>% filter(weather$station_nbr == i))[,5])))
  }
}

na <- rep(0,20)
for (j in 1:20){
  cont = 0
  for (i in 1:1035){
    if (is.na(st[i,j])){
      cont = cont + 1
    }
  }
  na[j] <- cont
}

na 

st <- st[,-c(5,8)]

st_avg <- rowMeans(st, na.rm = TRUE)

table(max.col(st)) # 8
table(max.col(-st)) # 7 e 13

d <- c(1:1035)

matplot(d,st[,c(8,7)], col = c('red','blue'), type='l', lty=1, 
        ylab = 'Average Temperature [F]', xlab = 'Days (from 01/01/2012)', 
        main = 'Comparison Between Stations')

lines(1:1035,st_avg, col = 'green', type = 'l', lty = 1, lwd = 2)

legend(850, 10, legend = c("Hottest","Coolest","Average"), 
       col = c("red","blue","green"), lty = 1, cex = 0.8, box.lty = 0, lwd = c(1,1,2))

weatherM <- read.table('weather.csv', header = TRUE, sep = ',')

dataM7 <- weatherM %>% filter(weatherM$station_nbr == 9, weatherM$tavg == 'M')
dataM8 <- weatherM %>% filter(weatherM$station_nbr == 10, weatherM$tavg == 'M')
dataM13 <- weatherM %>% filter(weatherM$station_nbr == 15, weatherM$tavg == 'M')

st8 <- (as.vector(as.numeric(as.character(dataM8[,3])))+as.vector(as.numeric(as.character(dataM8[,4]))))/2
st13 <- (as.vector(as.numeric(as.character(dataM13[,3])))+as.vector(as.numeric(as.character(dataM13[,4]))))/2
st13 <- st13[c(1,2,4,5)]
st8 <- ceiling(st8)
st13 <- ceiling(st13)

st[c(69,94,846,943),13] <- st13
st[c(153,166,214,492,548,668,822,943),8] <- st8
st[366,13] <- (st[365,13]+st[367,13])/2

data7 <- weatherM %>% filter(weatherM$station_nbr == 9)
ind7 <- which(data7$tavg == 'M')
ind7 <- ind7[-c(4,5,8,11,13,17)]

st7 <- (as.vector(as.numeric(as.character(dataM7[,3])))+as.vector(as.numeric(as.character(dataM7[,4]))))/2
st7 <- st7[-c(4,5,8,11,13,17)]
st7 <- ceiling(st7)

st[ind7,7] <- st7

ind7M <- (which(data7$tavg == 'M'))[c(8,11,13,17)]

for (i in 1:4){
  st[ind7M[i],7] <- (st[ind7M[i]-1,7]+st[ind7M[i]+1,7])/2
}

ind7M <- (which(data7$tavg == 'M'))[c(4,5)]
st[ind7M[1],7] <- 2/3*st[ind7M[1]-1] + 1/3*st[ind7M[1]+2]
st[ind7M[2],7] <- 1/3*st[ind7M[2]-2] + 2/3*st[ind7M[2]+1]


st_avg <- rowMeans(st, na.rm = TRUE)

matplot(d,cbind(smooth(st[,8]),smooth((st[,7]))), col = c('red','blue'), type='l', lty=1, 
        ylab = 'Average Temperature [F]', xlab = 'Days (from 01/01/2012)', 
        main = 'COMPARISON BETWEEN STATIONS')

lines(1:1035,smooth(st_avg), col = 'green', type = 'l', lty = 1, lwd = 2)

legend(850, 15, legend = c("Hottest","Coolest","Average"), 
       col = c("red","blue","green"), lty = 1, cex = 0.8, box.lty = 0, lwd = c(1,1,2))


