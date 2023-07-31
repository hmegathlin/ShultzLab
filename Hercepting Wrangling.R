
patterns <- c("m\\d|m\\d\\d|M\\d", "t\\d|T\\d", "d.|D.")
names(patterns) <- c("Mouse", "Timepoint", "Dilution")
Herceptin <- Herceptin %>% separate_wider_regex(2, patterns=patterns)

Herceptin$Mouse <- gsub("M", "m", Herceptin$Mouse)
Herceptin$Timepoint <- gsub("T", "t", Herceptin$Timepoint)
Herceptin$Dilution <- gsub("D", "d", Herceptin$Dilution)

d200 <- filter(Herceptin, Dilution=="d1")

d2000 <- filter(Herceptin, Dilution=="d2")

hpkdata <- d2000

PKData <- d2000

PKData <- rename(PKData, Concentration=AdjResult, )

write_csv(Herceptin, "HerceptinPK.csv")
