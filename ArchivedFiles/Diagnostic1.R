JEC1 <- read.csv("JEC-A-2012-20km")
JEC2 <- read.csv("JEC-E-2012-20km")

JSC1 <- read.csv("JSC-A-2012-20km")
JSC2 <- read.csv("JSC-E-2012-20km")

TCG1 <- read.csv("TCG-A-2012-20km")
TCG2 <- read.csv("TCG-E-2012-20km")

JECdata <- data.frame()
JSCdata <- data.frame()
TCGdata <- data.frame()

for(i in 2:366) {
  
  dayJEC1 <- subset(JEC1, Day == i)
  dayJEC2 <- subset(JEC2, Day == i)
  
  dayJSC1 <- subset(JSC1, Day == i)
  dayJSC2 <- subset(JSC2, Day == i)
  
  dayTCG1 <- subset(TCG1, Day == i)
  dayTCG2 <- subset(TCG2, Day == i)
  
  
  tempJEC <- cbind(i, max(dayJEC1$Lat), min(dayJEC1$Lat), max(dayJEC1$Lon), min(dayJEC1$Lon), max(dayJEC1$Conc), min(dayJEC1$Conc),
                   max(dayJEC2$Lat), min(dayJEC2$Lat), max(dayJEC2$Lon), min(dayJEC2$Lon), max(dayJEC2$Conc), min(dayJEC2$Conc))
  
  tempJSC <- cbind(i, max(dayJSC1$Lat), min(dayJSC1$Lat), max(dayJSC1$Lon), min(dayJSC1$Lon), max(dayJSC1$Conc), min(dayJSC1$Conc),
                   max(dayJSC2$Lat), min(dayJSC2$Lat), max(dayJSC2$Lon), min(dayJSC2$Lon), max(dayJSC2$Conc), min(dayJSC2$Conc))
  
  tempTCG <- cbind(i, max(dayTCG1$Lat), min(dayTCG1$Lat), max(dayTCG1$Lon), min(dayTCG1$Lon), max(dayTCG1$Conc), min(dayTCG1$Conc),
                   max(dayTCG2$Lat), min(dayTCG2$Lat), max(dayTCG2$Lon), min(dayTCG2$Lon), max(dayTCG2$Conc), min(dayTCG2$Conc))
  
  JECdata <- rbind(JECdata, tempJEC)
  JSCdata <- rbind(JSCdata, tempJSC)
  TCGdata <- rbind(TCGdata, tempTCG)
  
}

names(JECdata) <- c("Day", "LatMax1", "LatMin1", "LonMax1", "LonMin1", "MaxConc1", "MinConc1", "LatMax2", "LatMin2", "LonMax2", "LonMin2", "MaxConc2", "MinConc2")
names(JSCdata) <- c("Day", "LatMax1", "LatMin1", "LonMax1", "LonMin1", "MaxConc1", "MinConc1", "LatMax2", "LatMin2", "LonMax2", "LonMin2", "MaxConc2", "MinConc2")
names(TCGdata) <- c("Day", "LatMax1", "LatMin1", "LonMax1", "LonMin1", "MaxConc1", "MinConc1", "LatMax2", "LatMin2", "LonMax2", "LonMin2", "MaxConc2", "MinConc2")