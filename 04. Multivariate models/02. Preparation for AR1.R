load('../Dataset_pronti/mydata.RData')


ar1data=mydata[2:366,]


units.traslated=mydata[1:365,2:6]
transname=colnames(units.traslated)
for(i in 1:5)
{
  transname[i]=paste(transname[i],'_t1',sep='')
}
colnames(units.traslated)=transname


ar1data <- data.frame(ar1data,units.traslated)

ar1data=ar1data[1:364,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar1data[,17:26])) # --> 3
for(i in 1:364){
  for(j in 17:26){
    if(is.na(ar1data[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
ar1data[205,19]=(ar1data[204,19]+ar1data[206,19])/2
temp=(ar1data[232,17]+ar1data[235,17])/2
ar1data[233,17]=temp
ar1data[234,17]=temp






save(ar1data,list="ar1data",file="../Dataset_pronti/for_multivariate/ar1data.RData")
