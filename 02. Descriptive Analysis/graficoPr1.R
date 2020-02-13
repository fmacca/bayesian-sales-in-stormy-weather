# Run graficoTemperature before running this code, you need some of those variables in the global environment

train <- read.table('../Data/train.csv', header = TRUE, sep = ',')
train1 <- train %>% filter(train$item_nbr == 1, train$units > 0)
train36_1 <- train %>% filter(train$store_nbr == 36, train$item_nbr == 1)
weather <- read.table('../Data/weather_temp.csv', header = TRUE)
weather1 <- weather %>% filter(weather$station_nbr == 1)

st36_pr1 <- rep(-1,1035)
j = 1
for (i in 1:1035){
  if (as.character(train36_1$date[j]) == as.character(weather1$date[i])){
    st36_pr1[i] <- train36_1$units[j]
    j = j + 1
  }
}

naind <- which(st36_pr1 == -1) 
st36_pr1[naind] <- NA
redPoints <- rep(0,length(naind))
d <- c(1:1035)

plot(d,st36_pr1, col = 'green4', type='h', lty=1, 
        ylab = 'Units Sold', xlab = 'Days (from 01/01/2012)', 
        main = 'Product 1', sub = '* This product is sold only in store 36')

points(naind,redPoints, col = 'red', pch = 20)

#

st_avg_new = st_avg/max(st_avg)*4+3

lines(1:1035,smooth(st_avg_new), col = 'grey', type = 'l', lty = 1, lwd = 1)

legend(750, 9.3, legend = c("Train Days","Test Days","Scaled Temperature"), col = c("green4","red","grey85"), 
       lty = c(1,NA,1), pch = c(NA,20,NA), cex = 0.7, box.lty = 0, lwd = c(1,NA,1))

