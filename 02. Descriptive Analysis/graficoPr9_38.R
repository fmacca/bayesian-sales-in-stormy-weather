# Run graficoTemperature before running this code, you need some of those variables in the global environment

train <- read.table('../Data/train.csv', header = TRUE, sep = ',')
trainalot <- train %>% filter(train$units > 100)
train100 <- train %>% filter(train$item_nbr == 5, train$units > 0)
train38_9 <- train %>% filter(train$store_nbr == 32, train$item_nbr == 19)
weather <- read.table('../Data/weather_temp.csv', header = TRUE)
weather1 <- weather %>% filter(weather$station_nbr == 1)

st38_pr9 <- rep(-1,1035)
j = 1
for (i in 1:1035){
  if (as.character(train38_9$date[j]) == as.character(weather1$date[i])){
    st38_pr9[i] <- train38_9$units[j]
    j = j + 1
  }
}

naind <- which(st38_pr9 == -1) 
st38_pr9[naind] <- NA
redPoints <- rep(0,length(naind))
d <- c(1:1035)

plot(d,st38_pr9, col = 'green4', type='h', lty=1, 
     ylab = 'Units Sold', xlab = 'Days (from 01/01/2012)', ylim = c(0,8),
     main = 'Product 19', sub = '* This product is sold only in store 32')

points(naind,redPoints, col = 'red', pch = 20)

#

st_avg_new = st_avg/max(st_avg)*4+3

lines(1:1035,smooth(st_avg_new), col = 'grey', type = 'l', lty = 1, lwd = 1)

legend(790, 3, legend = c("Train Days","Test Days","Scaled Temperature"), col = c("green4","red","grey"), 
       lty = c(1,NA,1), pch = c(NA,20,NA), cex = 0.7, box.lty = 0, lwd = c(1,NA,1))

