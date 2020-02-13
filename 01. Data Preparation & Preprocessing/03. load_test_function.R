#The function to correctly load the "train.csv" dataset

load_test <- function() {
  data=read.table('../Data/test.csv',sep=',',header=TRUE,colClasses=c('character',rep("integer",2)))
  
  library(chron)
  data[,1]=chron(dates=data[,1], format=c(dates="y-m-d"),out.format=c(dates="d/m/y"))
  
  return(data)
}

test=load_test()

save(test,list="test",file="../Dataset_pronti/test.RData")

