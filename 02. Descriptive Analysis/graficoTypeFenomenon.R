library(dplyr)
library(ggplot2)
weather <- read.table('weather_temp.txt', header = TRUE)

pioggia <- as.numeric(length(which(weather$rain == 1 & 
                                     weather$snow == 0 & 
                                     weather$hail == 0 &
                                     weather$thunder == 0 & 
                                     weather$smoke == 0 & 
                                     weather$sand ==0)))
thunder <- as.numeric(length(which(weather$snow == 0 & 
                                     weather$hail == 0 &
                                     weather$thunder == 1 & 
                                     weather$smoke == 0 & 
                                     weather$sand ==0)))

hail <- as.numeric(length(which(weather$snow == 0 & 
                                  weather$hail == 1 &
                                  weather$smoke == 0 & 
                                  weather$sand ==0)))

snow <- as.numeric(length(which(weather$snow == 1 & weather$smoke == 0 & 
                                  weather$sand ==0)))

fog <- as.numeric(length(which(weather$rain == 0 & 
                                 weather$snow == 0 & 
                                 weather$thunder == 0 & 
                                 weather$hail == 0 &
                                 (weather$fog + weather$foschia >= 1) & 
                                 weather$smoke == 0 & 
                                 weather$sand ==0)))

altro <- as.numeric(length(which(weather$smoke == 1 | weather$sand ==1)))
 
fenomeni <- as.numeric(length(which(weather$rain + weather$thunder + weather$snow + weather$hail + 
                           weather$fog + weather$foschia + weather$sand + weather$freeze + 
                           weather$smoke >= 1)))
 
pioggia + thunder + hail + snow + fog + altro == fenomeni    # TRUE

# inglobiamo hail in altro

perc_pioggia <- round(pioggia/fenomeni*100, digits = 1)
perc_thunder <- round(thunder/fenomeni*100, digits = 1)
perc_snow <- round(snow/fenomeni*100, digits = 1)
perc_fog <- round(fog/fenomeni*100, digits = 1)
perc_altro <- round(100 - (perc_pioggia + perc_thunder + perc_snow + perc_fog), digits = 1)

df <- data.frame(fasce=c("fog/mist","rain","thunderstorm","snow","others"),
                 perc=c(perc_fog, perc_pioggia, perc_thunder, perc_snow, perc_altro))

df$fasce <- factor(df$fasce, levels = c("fog/mist","rain","thunderstorm","snow","others"), ordered = TRUE)

head(df)            

ggplot(data=df, aes(x=fasce, y=perc)) +
  labs(x="Type of Phenomenon", y = "Percentage of Surveys [%]") +
  geom_bar(width = 0.65, stat="identity", fill=c('ivory3','cornflowerblue','midnightblue','light blue','lightgoldenrod3'))+
  geom_text(aes(label=perc), vjust=-1, color=c('ivory3','cornflowerblue','midnightblue','light blue','lightgoldenrod3'), size=10)+
  theme_bw() + theme(axis.text=element_text(size=20),
           axis.title=element_text(size=25,face="bold")) + ylim(0,45)




