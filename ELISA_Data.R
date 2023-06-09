library(tidyverse)

elisa <- read_delim("~/Desktop/IgG Clearance 2000 Dilution.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)

elisa <- elisa %>% drop_na()           
colnames(elisa) <- elisa[1,]
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



write_csv(elisa_wider, "Avg_AdjConc.csv")


ggplot(elisa, aes(x=TimePoint, y=AdjConc)) + geom_point()
