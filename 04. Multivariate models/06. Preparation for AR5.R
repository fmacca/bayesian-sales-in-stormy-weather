load('../Dataset_pronti/mydata.RData')


ar5data=mydata[6:366,]

units.traslated1=mydata[1:361,2:6]
transname1=colnames(units.traslated1)
for(i in 1:5)
{
  transname1[i]=paste(transname1[i],'_t1',sep='')
}
colnames(units.traslated1)=transname1

units.traslated2=mydata[2:362,2:6]
transname2=colnames(units.traslated2)
for(i in 1:5)
{
  transname2[i]=paste(transname2[i],'_t2',sep='')
}
colnames(units.traslated2)=transname2

units.traslated3=mydata[3:363,2:6]
transname3=colnames(units.traslated3)
for(i in 1:5)
{
  transname3[i]=paste(transname3[i],'_t3',sep='')
}
colnames(units.traslated3)=transname3

units.traslated4=mydata[4:364,2:6]
transname4=colnames(units.traslated4)
for(i in 1:5)
{
  transname4[i]=paste(transname4[i],'_t4',sep='')
}
colnames(units.traslated4)=transname4

units.traslated5=mydata[5:365,2:6]
transname5=colnames(units.traslated5)
for(i in 1:5)
{
  transname5[i]=paste(transname5[i],'_t5',sep='')
}
colnames(units.traslated5)=transname5

ar5data <- data.frame(ar5data,units.traslated1,units.traslated2, units.traslated3, units.traslated4, units.traslated5)

ar5data=ar5data[1:360,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar5data[,17:26])) # --> 3
for(i in 1:360){
  for(j in 17:26){
    if(is.na(ar5data[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
k=4
ar5data[205-k,19]=(ar5data[204-k,19]+ar5data[206-k,19])/2
temp=(ar5data[232-k,17]+ar5data[235-k,17])/2
ar5data[233-k,17]=temp
ar5data[234-k,17]=temp







save(ar5data,list="ar5data",file="../Dataset_pronti/for_multivariate/ar5data.RData")
