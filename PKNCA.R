library(PKNCA)
library(tidyverse)

PKNCA.options(default=TRUE)
PKNCA.options(auc.method="linear")

PKData <- read_csv("PKData.csv")


PKData <- mutate(PKData, Concentration=Concentration/1000)


Dose <- PKData %>% filter(Time==0)

for(i in 1:nrow(PKData)){
  if(PKData$Time[i]==0){
    PKData$Concentration[i] <- 0
  } else {
    PKData$Concentration[i] <- PKData$Concentration[i]
  }
}

ggplot(PKData, aes(Time, Concentration)) + geom_point(aes(color=Strain)) + geom_line(aes(color=Strain))


PKData <- arrange(PKData, Mouse, Time)

conc <- PKNCAconc(PKData, formula=Concentration~Time|Strain * Mouse, subject= "Mouse")
dose <- PKNCAdose(data=Dose, formula=Concentration~Time|Strain * Mouse)
units <- pknca_units_table(concu="ug/mL", doseu="ug", timeu="days", amountu="ug")

data <- PKNCAdata(conc, dose, units=units)
results <- pk.nca(data)

view <- as.data.frame(results[["result"]])


view <- filter(view, PPTESTCD== "aucinf.obs" | PPTESTCD== "lambda.z" |PPTESTCD== "half.life")

view <- pivot_wider(view, names_from=c(PPTESTCD, PPORRESU), values_from=PPORRES)

view <- view %>% select(-exclude) %>% drop_na()
view <- mutate(view, Vd = (200/(`aucinf.obs_days*ug/mL` * `lambda.z_1/days`)), CL=(200/(`aucinf.obs_days*ug/mL`)))


colnames(view)
nca <- select(view, Strain,"aucinf.obs_days*ug/mL", "lambda.z_1/days", "half.life_days", "Vd", "CL")

nca <- nca %>% group_by(Strain) %>% summarise(across(1:5, .fns= list(mean= mean, sd= sd), .))



###### C0 Calculations #############

c0.test <- arrange(PKData, Time)

mice <- unique(c0.test$Mouse)
C0 <- data.frame()
for(i in 1:length(mice)){
  data <- filter(c0.test, Mouse==mice[i])
  temp <- pk.calc.c0(conc=data$Concentration, 
                     time=data$Time, 
                     time.dose=0, 
                     method = c("c0", "logslope", "c1", "cmin", "set0"), check=TRUE)
  info <- data.frame(Mouse=data$Mouse[1], Strain=data$Strain[1])
  X <- bind_cols(info, temp)
  C0 <- bind_rows(C0, X)
}
C0 <- rename(C0, C0=3)
C0 %>% group_by(Strain) %>% summarise(mean(C0))
Vd <- mutate(C0, Vd=200/C0)





