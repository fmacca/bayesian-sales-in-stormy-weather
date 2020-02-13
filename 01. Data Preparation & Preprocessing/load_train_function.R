#The function to correctly load the "train.csv" dataset

load_train <- function() {
  data=read.table('train.csv',sep=',',header=TRUE,colClasses=c('character',rep("integer",3)))
  
  library(chron)
  data[,1]=chron(dates=data[,1], format=c(dates="y-m-d"),out.format=c(dates="d/m/y"))
  
  return(data)
}

train=load_train()

save(train,list="train",file="Dataset_pronti/train.RData")

