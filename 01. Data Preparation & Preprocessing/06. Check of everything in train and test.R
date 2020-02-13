library(chron)
library(tidyverse)
library(sqldf)

load('../Dataset_pronti/train.RData')
load('../Dataset_pronti/test.RData')
test$units=rep(NA,length(test))

full=rbind(train,test)

full=full[order(full$date,full$store_nbr,full$item_nbr),]

unit_series <- function(data,store,item,last_date='14/12/19',all.dates=FALSE) {
  data=data[which(data$store_nbr==store & data$item_nbr==item & data$date<chron(date=last_date,format = c(date='d/m/y'))),c(1,4)]
  
  if(all.dates)
  {
    #To be decided in the future
    #I would like to add NAs for test dates
    # and 0 for christmas
  }
  
  return(data)
}

n_stores=max(full$store_nbr)
n_items=max(full$item_nbr)

# temp=rep(1:n_stores,n_items)
# temp=temp[order(temp)]
# 
# results=data.frame(
#   store_nbr=temp,
#   item_nbr=rep(1:n_items,n_stores),
#   sold=rep(NA,n_items*n_stores),
#   stringsAsFactors=FALSE
# )

maxs=sqldf("SELECT store_nbr,item_nbr,max(units) FROM full GROUP BY store_nbr,item_nbr")
m=spread(maxs,key=item_nbr,value='max(units)')
m=as.matrix(m[,2:(n_items+1)])
m=((m>0))

for (st in 1:n_stores){
  for(it in 1:n_items){
    if((m[st,it])==F)
    {
      m[st,it]=' '
    }
    else
    {
      m[st,it]='!'
    }
    
  }
}
m=t(m)
save(m,list="m",file="../Dataset_pronti/which_in_which.RData")
write.table(m,file="../Dataset_pronti/which_in_which.csv",sep=',')

# for (st in 1:max(full$store_nbr)){
#   for(it in 1:max(full$item_nbr)){
#     
#     curr=unit_series(data=full,item=it,store=st)
#     if(max(na.omit(curr$units))>0){
#       sold=1
#     }else{
#       sold=0
#     }
#     results[which(results$store_nbr==st & results$item_nbr==it),3]=sold
#   }
# }
