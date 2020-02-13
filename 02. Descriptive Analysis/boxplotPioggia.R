train <- read.table('../Data/train.csv', header = TRUE, sep = ',')
weather <- read.table('../Data/weather_temp.csv', header = TRUE)
key <- read.table('../Data/key.csv', header = TRUE, sep = ',')

library(dplyr)
#sandplace <- weather %>% filter(weather$sand == 1 | weather$smoke == 1)
#table(sandplace$station_nbr)
#store36 <- train %>% filter(train$store_nbr == 36, train$units > 0)

train38_9 <- train %>% filter(train$store_nbr == 36, train$item_nbr == 44)
weather1 <- weather %>% filter(weather$station_nbr == 18)

rainydays <- which((weather1$rain + weather1$thunder) >= 1 & weather1$snow == 0)
before <- rainydays - 3
after <- rainydays + 3

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

soldrain <- st38_pr9[rainydays]
soldNOrain <- st38_pr9[-rainydays]


#boxplot(soldrain,soldNOrain)
#before <- c(21,50,195,250,353,538,746,746,757,781,807,807,847,847)
#after <- c(23,52,197,252,355,539,749,749,759,783,810,810,850,850)

#before[1] <- 1 
after[218] <- 1035
Bsoldrain <- st38_pr9[before]
Asoldrain <- st38_pr9[after]

df <- data.frame(Bsoldrain, soldrain, Asoldrain)
colnames(df) <- c('before a storm','during a storm','after a storm')

#dfS <- data.frame(Bsoldrain,Asoldrain)
#colnames(dfS) <- c('before a snowfall','after a snowfall')
 
#par(mfrow = c(1,2))
boxplot(df, col = c('deepskyblue2','midnightblue','dodgerblue3'), main = 'Effect of a Storm')
#boxplot(dfS, col = c('lightgreen','lightcyan'), main = 'Snowy Area')


sort(Bsoldrain)

