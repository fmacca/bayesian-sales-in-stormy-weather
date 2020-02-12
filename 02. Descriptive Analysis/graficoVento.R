library(dplyr)
weather <- read.table('weather_temp.txt', header = TRUE)

for (i in 1:20517){
  if (weather[i,17] == 'M'){
    weather[i,17] <- NA
  }
}

st <- matrix(0,1035,20)

for (i in 1:20){
  if (i != 5){
    st[,i] <- as.vector(as.numeric(as.character((weather %>% filter(weather$station_nbr == i))[,17])))
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

st <- st[,-5]

st_avg <- rowMeans(st, na.rm = TRUE)

table(max.col(st)) 
table(max.col(-st)) 

d <- c(1:1035)

matplot(d,st[,c(6,14)], col = c('red','blue'), type='l', lty=1, 
        ylab = 'Average Wind Speed [mph]', xlab = 'Days (from 01/01/2012)', 
        main = 'Comparison Between Stations')

lines(1:1035,st_avg, col = 'green', type = 'l', lty = 1, lwd = 2)

legend(870, 29, legend = c("Fastest","Slowest","Average"), 
       col = c("red","blue","green"), lty = 1, cex = 0.8, box.lty = 0, lwd = c(1,1,2))

weatherM <- read.table('weather_temp.txt', header = TRUE)

data6 <- weatherM %>% filter(weatherM$station_nbr == 7)
data14 <- weatherM %>% filter(weatherM$station_nbr == 15)

miss6 <- which(data6$avgspeed == 'M')
miss14 <- which(data14$avgspeed == 'M')

for (i in 1:3){
  st[miss6[i],6] <- (st[miss6[i]-1,6] + st[miss6[i]+1,6])/2
  st[miss14[i],14] <- (st[miss14[i]-1,14] + st[miss14[i]+1,14])/2
}


st_avg <- rowMeans(st, na.rm = TRUE)

matplot(d,cbind(smooth(st[,6]),smooth((st[,14]))), col = c('purple','pink'), type='l', lty=1, 
        ylab = 'Average Wind Speed [mph]', xlab = 'Days (from 01/01/2012)', 
        main = 'COMPARISON BETWEEN STATIONS')

lines(1:1035,smooth(st_avg), col = 'light blue', type = 'l', lty = 1, lwd = 2)

legend(880, 25, legend = c("Fastest","Slowest","Average"), 
       col = c("purple","pink","light blue"), lty = 1, cex = 0.7, box.lty = 0.5, lwd = c(1,1,2))


