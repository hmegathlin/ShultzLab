library(tidyverse)
library(scales)
library(lubridate)
library(vistime)

inj <- mdy("07/20/23")  #Put anticipated date of injection
fb <- inj + weeks(3)  #Anticipated date of founders being born
fs  <- fb + weeks(4) #Anticipated date of founders being sequenced
bc <- fs + weeks(1)  #Backcross to NSG (Guess about 1 week after sequencing, but this can be set manually)
f1b <- bc + weeks(3)  #F1 birth
f1s <- f1b + weeks(4) #F1 sequencing

tl <- tibble(Event=c("Injection", "Founders Born", "Founders Sequencing", "Back Cross to NSG", "F1 Born", "F1 Sequencing"), Dates=c(inj, fb, fs, bc, f1b, f1s))
tl$formDate <- format(tl$Dates, "%D")

ggplot(tl, aes(x=Dates, y=0)) + geom_point() + 
  geom_text(aes(label=Event), hjust="outward", vjust=-3) + 
  geom_text(aes(label=formDate), hjust="outward", vjust=3) +
  geom_hline(yintercept=0) +
  theme(
    plot.background=element_blank(), 
    panel.background = element_blank(),
    panel.border=element_blank(),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y = element_blank(), 
    axis.title= element_blank(),
    axis.text.x= element_text(color="black", size=12)
  ) +
  scale_x_date(date_breaks="1 month", date_labels="%B %Y", limits=c(tl$Dates[1]-7, tl$Dates[6]+10))

ggsave("timeline.jpg", width=10, height=3)


