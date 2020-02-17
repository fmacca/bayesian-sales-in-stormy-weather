load('../Dataset_pronti/mydata.RData')


ar2data=mydata[3:366,]

units.traslated1=mydata[1:364,2:6]
transname1=colnames(units.traslated1)
for(i in 1:5)
{
  transname1[i]=paste(transname1[i],'_t1',sep='')
}
colnames(units.traslated1)=transname1

units.traslated2=mydata[2:365,2:6]
transname2=colnames(units.traslated2)
for(i in 1:5)
{
  transname2[i]=paste(transname2[i],'_t2',sep='')
}
colnames(units.traslated2)=transname2


ar2data <- data.frame(ar2data,units.traslated1,units.traslated2)

ar2data=ar2data[1:363,]



# FIXING NAs
# is there any NA value?
sum(is.na(ar2data[,17:26])) # --> 3
for(i in 1:363){
  for(j in 17:26){
    if(is.na(ar2data[i,j])){
      print(paste(i,j,sep=','))
    }
  }
}

# Substitute NA values in tavg with the mean between day before and day after
k=1
ar2data[205-k,19]=(ar2data[204-k,19]+ar2data[206-k,19])/2
temp=(ar2data[232-k,17]+ar2data[235-k,17])/2
ar2data[233-k,17]=temp
ar2data[234-k,17]=temp






save(ar2data,list="ar2data",file="../Dataset_pronti/for_multivariate/ar2data.RData")
