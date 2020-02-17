load('../Dataset_pronti/mydata.RData')


ar4data=mydata[5:366,]

units.traslated1=mydata[1:362,2:6]
transname1=colnames(units.traslated1)
for(i in 1:5)
{
  transname1[i]=paste(transname1[i],'_t1',sep='')
}
colnames(units.traslated1)=transname1

units.traslated2=mydata[2:363,2:6]
transname2=colnames(units.traslated2)
for(i in 1:5)
{
  transname2[i]=paste(transname2[i],'_t2',sep='')
}
colnames(units.traslated2)=transname2

units.traslated3=mydata[3:364,2:6]
transname3=colnames(units.traslated3)
for(i in 1:5)
{
  transname3[i]=paste(transname3[i],'_t3',sep='')
}
colnames(units.traslated3)=transname3

units.traslated4=mydata[4:365,2:6]
transname4=colnames(units.traslated4)
for(i in 1:5)
{
  transname4[i]=paste(transname4[i],'_t4',sep='')
}
colnames(units.traslated4)=transname4

ar4data <- data.frame(ar4data,units.traslated1,units.traslated2, units.traslated3, units.traslated4)

ar4data=ar4data[1:361,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar4data[,17:26])) # --> 3
for(i in 1:361){
  for(j in 17:26){
    if(is.na(ar4data[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
k=3
ar4data[205-k,19]=(ar4data[204-k,19]+ar4data[206-k,19])/2
temp=(ar4data[232-k,17]+ar4data[235-k,17])/2
ar4data[233-k,17]=temp
ar4data[234-k,17]=temp






save(ar4data,list="ar4data",file="../Dataset_pronti/for_multivariate/ar4data.RData")
