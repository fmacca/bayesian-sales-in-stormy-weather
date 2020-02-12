library(dplyr)
library(ggplot2)
weather <- read.table('weather_temp.txt', header = TRUE)

fenomeni <- length(which(weather$rain + weather$thunder + weather$snow + weather$hail + 
                    weather$fog + weather$foschia + weather$sand + weather$freeze + 
                    weather$smoke >= 1))
fenomeni <- as.numeric(fenomeni)

perc_feno <- fenomeni/dim(weather)[1]*100
perc_feno <- round(perc_feno, digits = 2)
perc_nofeno <- 100-perc_feno

df <- data.frame(fasce=c("at least one","none"),
                 perc=c(perc_feno, perc_nofeno))
head(df)            

ggplot(data=df, aes(x=fasce, y=perc)) +
  labs(x="Meteorological Phenomena", y = "Percentage of Surveys [%]") +
  geom_bar(width = 0.5, stat="identity", fill=c('orange','grey'))+
  geom_text(aes(label=perc), vjust=-1, color=c('orange','grey'), size=5)+
  theme_minimal() + ylim(0,60)


