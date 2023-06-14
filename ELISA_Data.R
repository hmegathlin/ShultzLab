library(tidyverse)
setwd("~/Desktop/R Working Directory/ELISA")


elisa <- elisa %>% drop_na()
elisa$Dilution <- as.numeric(elisa$Dilution)
elisa <- elisa %>% drop_na() 
elisa <- select(elisa, Sample, AdjConc)
elisa <- separate_wider_delim(elisa, Sample, delim="_", names=c("TimePoint", "Mouse"))

for(i in 1:9){
  elisa$Mouse[elisa$Mouse==paste0("Mus0", i)] <- paste0("Mus", i)
}


elisa$AdjConc <- as.numeric(elisa$AdjConc)
elisa$AdjConc <- replace_na(elisa$AdjConc, 0)
elisa_wider <- pivot_wider(elisa, names_from=TimePoint, values_from=AdjConc, id_cols=Mouse)

elisa$TimePoint[elisa$TimePoint==T1] <- "24hours"

elisa2000 <- elisa

elisa200$TimePoint <- gsub("TP", "T", elisa200$TimePoint)

write_csv(elisa, "Longer_Elisa.csv")


write_csv(elisa_wider, "Avg_AdjConc.csv")



##### Standard Curves ####
Standard200 <- read_csv("Standard200.csv")
Standard200 <- Standard200 %>% drop_na() 
Standard200 <- filter(Standard200, Sample<=8)

ggplot() + geom_line(data=Standard200, aes(`Concentration ng/mL`, MeanValue))


Standard2000 <- read_csv("Standard2000.csv")
Standard2000 <- Standard2000 %>% drop_na() 
Standard2000 <- filter(Standard2000, Sample<=12)

ggplot(Standard2000) + geom_line(aes(`Standard Value ng/mL`, AvgOD))


elisa200 <- read_csv("IgG Clearance.csv")
elisa200 <- elisa200 %>% 
  select(Sample, OD, AvgConc) %>% 
  fill(c(Sample, AvgConc), .direction="down")
elisa200$OD <- as.numeric(elisa200$OD)
elisa200$AvgConc <- as.numeric(elisa200$AvgConc)
elisa200 <- elisa200 %>% drop_na()
elisa200 <- elisa200 %>%  
  group_by(Sample) %>% 
  summarise(MeanOD=mean(OD), AvgConc=mean(AvgConc))

elisa2000 <- read_csv("IgG Clearance 2000 Dilution.csv")
elisa2000 <- elisa2000 %>% 
  select(Sample, OD, AvgConc) %>% 
  fill(c(Sample, AvgConc), .direction="down")
elisa2000$OD <- as.numeric(elisa2000$OD)
elisa2000$AvgConc <- as.numeric(elisa2000$AvgConc)
elisa2000 <- elisa2000 %>% drop_na()
elisa2000 <- elisa2000 %>%  
  group_by(Sample) %>% 
  summarise(MeanOD=mean(OD), AvgConc=mean(AvgConc))

d200 <- ggplot() + geom_line(data=Standard200, aes(log(`Concentration ng/mL`), MeanValue)) + geom_point(data=elisa200, aes(log(AvgConc), MeanOD))
d200
d2000 <- ggplot() + geom_line(data=Standard2000, aes(log(`Standard Value ng/mL`), AvgOD)) + geom_point(data=elisa2000, aes(log(AvgConc), MeanOD))
d2000


