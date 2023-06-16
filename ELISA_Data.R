library(tidyverse)
setwd("~/Desktop/R Working Directory/ELISA")

elisa200 <- read_csv("LDS 7194 1 to 200 ELISA IgG Clearance.csv")
elisa200 <- mutate(elisa200, across(2:12, .fns=as.numeric))
elisa200 <- elisa200 %>% pivot_longer(cols=2:12, names_to="TimePoint")


elisa<- read_csv("IgG Clearance 2000 Dilution.csv")
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

elisa2000 <- elisa

elisa_wider <- pivot_wider(elisa, names_from=TimePoint, values_from=AdjConc, id_cols=Mouse)




elisa200$TimePoint <- gsub("TP", "T", elisa200$TimePoint)

elisas <- full_join(elisa200, elisa2000, by=c("TimePoint", "Mouse"), suffix=c(".200", ".2000"))
elisas$Mouse <- gsub("Mus", "", elisas$Mouse)
elisas$TimePoint <- gsub("T", "", elisas$TimePoint)
write_csv(elisas, "CompareDilutions.csv")

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


elisa200 <- read_csv("LDS 7194 1 to 200 ELISA IgG Clearance.csv")
elisa200 <- mutate(elisa200, across(2:12, .fns=as.numeric))
elisa200 <- elisa200 %>% pivot_longer(cols=2:12, names_to="TimePoint")


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

##### Look for 100 #####

cd <- read_csv("CompareDilutions.csv")


tp_mean <- cd %>% group_by(TimePoint) %>% summarise(mean(`Concentration  to use`))
max(tp_mean$`mean(\`Concentration  to use\`)`)
#t2

total <- filter(cd, TimePoint==2)
total <- rename(total, Total=`Concentration  to use`)
total <- select(total, Mouse, Total)
cd <- full_join(cd, total, by="Mouse")
cd <- filter(cd, TimePoint!=1)
cd <- rename(cd, Concentration=`Concentration  to use`)
cd <- cd %>% mutate(Percent_Left=((Concentration)/(Total)*100))

write_csv(cd, "Figure.csv")
