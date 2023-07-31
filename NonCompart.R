install.packages("NonCompart")
library(NonCompart)


PKData <- HerceptinPK
PKData <- PKData %>% rename(Time=`Timepoint`)
PKData <- filter(PKData, Time!=0 & Mouse !=10 & Mouse !=4)

nca <- tblNCA(PKData, 
       key="Mouse", 
       colTime="Time", 
       colConc="Concentration", 
       dose=200000,
       doseUnit="ng",
       timeUnit="days",
       down="log",
       concUnit = "ng/mL",
       MW=148)

nca <- bind_cols(data.frame(Strain=c(rep("NSG", times=4), rep("FcgRI_KO", times=4))), nca)
sum.nca <- nca %>% group_by(Strain) %>% summarise(across(3:37, mean))
