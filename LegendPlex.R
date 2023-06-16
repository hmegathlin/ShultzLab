setwd("~/Desktop/R Working Directory/IL34/LEGENDPlex")
library(tidyverse)

data <- data %>% filter(sample_type!="Standard")

data <- select(data, sample, `IL-34 (A10)`)

data$`IL-34 (A10)` <- as.numeric(data$`IL-34 (A10)`)
data <- drop_na(data)

data <- data %>% group_by( sample) %>% summarise( Concentration=mean(`IL-34 (A10)`))
