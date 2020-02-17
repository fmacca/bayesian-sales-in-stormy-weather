load('../Dataset_pronti/mydata_week.RData')


ar1data_week=mydata_week[2:366,]


units.traslated=mydata_week[1:365,2:6]
transname=colnames(units.traslated)
for(i in 1:5)
{
  transname[i]=paste(transname[i],'_t1',sep='')
}
colnames(units.traslated)=transname


ar1data_week <- data.frame(ar1data_week,units.traslated)

ar1data_week=ar1data_week[1:364,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar1data_week[,17:26])) # --> 3
for(i in 1:364){
  for(j in 17:26){
    if(is.na(ar1data_week[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
ar1data_week[205,19]=(ar1data_week[204,19]+ar1data_week[206,19])/2
temp=(ar1data_week[232,17]+ar1data_week[235,17])/2
ar1data_week[233,17]=temp
ar1data_week[234,17]=temp






save(ar1data_week,list="ar1data_week",file="../Dataset_pronti/for_multivariate/ar1data_week.RData")
