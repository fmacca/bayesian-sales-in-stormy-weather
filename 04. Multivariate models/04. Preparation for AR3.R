load('../Dataset_pronti/mydata.RData')


ar3data=mydata[4:366,]

units.traslated1=mydata[1:363,2:6]
transname1=colnames(units.traslated1)
for(i in 1:5)
{
  transname1[i]=paste(transname1[i],'_t1',sep='')
}
colnames(units.traslated1)=transname1

units.traslated2=mydata[2:364,2:6]
transname2=colnames(units.traslated2)
for(i in 1:5)
{
  transname2[i]=paste(transname2[i],'_t2',sep='')
}
colnames(units.traslated2)=transname2

units.traslated3=mydata[3:365,2:6]
transname3=colnames(units.traslated3)
for(i in 1:5)
{
  transname3[i]=paste(transname3[i],'_t3',sep='')
}
colnames(units.traslated3)=transname3

ar3data <- data.frame(ar3data,units.traslated1,units.traslated2, units.traslated3)

ar3data=ar3data[1:362,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar3data[,17:26])) # --> 3
for(i in 1:362){
  for(j in 17:26){
    if(is.na(ar3data[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
k=2
ar3data[205-k,19]=(ar3data[204-k,19]+ar3data[206-k,19])/2
temp=(ar3data[232-k,17]+ar3data[235-k,17])/2
ar3data[233-k,17]=temp
ar3data[234-k,17]=temp






save(ar3data,list="ar3data",file="../Dataset_pronti/for_multivariate/ar3data.RData")
