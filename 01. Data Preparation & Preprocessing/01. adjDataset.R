library(dplyr)
data <- read.table('../Data/weather.csv', header = TRUE, sep = ',')
attach(data)
#data_ <- data %>% filter()


# VARIABILE BINARIA PIOGGIA

a <- as.numeric(grep("RA", codesum))
b <- as.numeric(grep("DZ", codesum))
c <- rep(0,length(b))
d <- as.numeric(grep("SQ", codesum))
e <- rep(0,length(d))
aux = 1
for (i in 1:length(b)){
  cont = 0
  for (j in 1:length(a)){
    if (a[j] == b[i]) {
      cont = cont + 1
    }
  }
  if (cont == 0){
    c[aux] = b[i]
    aux = aux +1
  }
}
c <- c[1:164]
A <- sort(c(a,c), decreasing = FALSE)

aux = 1
for (i in 1:length(d)){
  cont = 0
  for (j in 1:length(A)){
    if (A[j] == d[i]) {
      cont = cont + 1
    }
  }
  if (cont == 0){
    e[aux] = d[i]
    aux = aux +1
  }
}
e <- e[1:3]
rainind <- sort(c(A,e), decreasing = FALSE)

rain <- rep(0,dim(data)[1])
rain[rainind] = rep(1,length(rainind))

data <- data.frame(cbind(data,rain))


# VARIABILE BINARIA TEMPORALE

ts <- as.numeric(grep("TS", codesum))
thunder <- rep(0,dim(data)[1])
thunder[ts] = rep(1,length(ts))

data <- data.frame(cbind(data,thunder))


# VARIABILE BINARIA NEVE

sn <- as.numeric(grep("SN", codesum))
snpiu <- c(5167, 7446, 12870, 15195, 16491)
snowind <- sort(c(sn,snpiu), decreasing = FALSE)
snow <- rep(0,dim(data)[1])
snow[snowind] <- rep(1,length(snowind))

data <- data.frame(cbind(data,snow)) 


# VARIABILE BINARIA GRANDINE

gr <- as.numeric(grep("GR", codesum))
pl <- as.numeric(grep("PL", codesum))
hailind <- sort(c(gr,pl), decreasing = FALSE)
hail <- rep(0,dim(data)[1])
hail[hailind] <- rep(1,length(hailind))

data <- data.frame(cbind(data,hail)) 


# VARIABILE BINARIA NEBBIA

fg <- as.numeric(grep("FG", codesum))
fog <- rep(0,dim(data)[1])
fog[fg] = rep(1,length(fg))

data <- data.frame(cbind(data,fog))


# VARIABILE BINARIA FOSCHIA

br <- as.numeric(grep("BR", codesum))
hz <- as.numeric(grep("HZ", codesum))
fos <- rep(0,length(hz))
aux = 1
for (i in 1:length(hz)){
  cont = 0
  for (j in 1:length(br)){
    if (br[j] == hz[i]) {
      cont = cont + 1
    }
  }
  if (cont == 0){
    fos[aux] = hz[i]
    aux = aux + 1
  }
}

fos <- fos[1:521]
foschiaind <- sort(c(br,fos), decreasing = FALSE)
foschia <- rep(0,dim(data)[1])
foschia[foschiaind] <- rep(1,length(foschiaind))

data <- data.frame(cbind(data,foschia))


# VARIABILE BINARIA SABBIA

du <- as.numeric(grep("DU", codesum))
ss <- as.numeric(grep("SS", codesum))
sandind <- sort(c(du,ss), decreasing = FALSE)
sand <- rep(0,dim(data)[1])
sand[sandind] <- rep(1,length(sandind))

data <- data.frame(cbind(data,sand))


# VARIABILE BINARIA CONGELAMENTO

fz <- as.numeric(grep("FZ", codesum))
freeze <- rep(0,dim(data)[1])
freeze[fz] <- rep(1,length(fz))

data <- data.frame(cbind(data,freeze))


# VARIABILE BINARIA FUMO

fu <- as.numeric(grep("FU", codesum))
smoke <- rep(0,dim(data)[1])
smoke[fu] <- rep(1,length(fu))

data <- data.frame(cbind(data,smoke))

data <- data[,-c(9,10,13)]

write.table(data, file = "../Data/weather_temp.csv")




