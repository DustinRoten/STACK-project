library(reshape2)
library(ggplot2)
library(ggmap)

##### Model B #####

JECModel1 <- read.csv("JEC-B-2012-20km")[2:6]
JECModel2 <- read.csv("JEC-E-2012-20km")[2:6]

JSCModel1 <- read.csv("JSC-B-2012-20km")[2:6]
JSCModel2 <- read.csv("JSC-E-2012-20km")[2:6]

TCGModel1 <- read.csv("TCG-B-2012-20km")[2:6]
TCGModel2 <- read.csv("TCG-E-2012-20km")[2:6]


JECStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
    c(39.28684, -96.11821),
    c(39.28681, -96.11721),
    c(39.28681, -96.11618)
  )

JSCStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
    c(36.99781, -84.59239)
  )

TCGStackLoc <- rbind(
    c(46.75575, -122.85820),
    c(46.75513, -122.85810)
  )

JECeGRIDLoc <- c(39.2865, -96.1172)
JSCeGRIDLoc <- c(36.9981, -84.5919)
TCGeGRIDLoc <- c(46.7559, -122.8598)

StartDay <- 2
EndDay <- 366

R <- 0.1

Metric1 = NULL
Metric2 = NULL
Metric3 = NULL



##### JEC Power Plant #####

for (i in StartDay:EndDay) {
  
  JECDayModel1 <- subset(JECModel1, Day == i)
  JECDayModel2 <- subset(JECModel2, Day == i)
  
  x_range <- max(max(JECDayModel1$Lon), max(JECDayModel2$Lon)) - min(min(JECDayModel1$Lon), min(JECDayModel2$Lon)) + 1
  y_range <- max(max(JECDayModel1$Lat), max(JECDayModel2$Lat)) - min(min(JECDayModel1$Lat), min(JECDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JECmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JECmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JECDayModel1$Conc[JECDayModel1$Lon >= min(JECDayModel1$Lon) + R*(k-1) &
                                    JECDayModel1$Lon < min(JECDayModel1$Lon) + R*k &
                                    JECDayModel1$Lat >= min(JECDayModel1$Lat) + R*(j-1) &
                                    JECDayModel1$Lat < min(JECDayModel1$Lat) + R*j])
      
      JECmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JECDayModel2$Conc[JECDayModel2$Lon >= min(JECDayModel2$Lon) + R*(k-1) &
                                    JECDayModel2$Lon < min(JECDayModel2$Lon) + R*k &
                                    JECDayModel2$Lat >= min(JECDayModel2$Lat) + R*(j-1) &
                                    JECDayModel2$Lat < min(JECDayModel2$Lat) + R*j])
      
      JECmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric1[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JECmatrix2 - JECmatrix1))
  
}




##### JSC Power Plant #####

for (i in StartDay:EndDay) {
  
  JSCDayModel1 <- subset(JSCModel1, Day == i)
  JSCDayModel2 <- subset(JSCModel2, Day == i)
  
  x_range <- max(max(JSCDayModel1$Lon), max(JSCDayModel2$Lon)) - min(min(JSCDayModel1$Lon), min(JSCDayModel2$Lon)) + 1
  y_range <- max(max(JSCDayModel1$Lat), max(JSCDayModel2$Lat)) - min(min(JSCDayModel1$Lat), min(JSCDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JSCmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JSCmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JSCDayModel1$Conc[JSCDayModel1$Lon >= min(JSCDayModel1$Lon) + R*(k-1) &
                                       JSCDayModel1$Lon < min(JSCDayModel1$Lon) + R*k &
                                       JSCDayModel1$Lat >= min(JSCDayModel1$Lat) + R*(j-1) &
                                       JSCDayModel1$Lat < min(JSCDayModel1$Lat) + R*j])
      
      JSCmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JSCDayModel2$Conc[JSCDayModel2$Lon >= min(JSCDayModel2$Lon) + R*(k-1) &
                                       JSCDayModel2$Lon < min(JSCDayModel2$Lon) + R*k &
                                       JSCDayModel2$Lat >= min(JSCDayModel2$Lat) + R*(j-1) &
                                       JSCDayModel2$Lat < min(JSCDayModel2$Lat) + R*j])
      
      JSCmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric2[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JSCmatrix2 - JSCmatrix1))
  
}



##### TCG Power Plant #####

for (i in StartDay:EndDay) {
  
  TCGDayModel1 <- subset(TCGModel1, Day == i)
  TCGDayModel2 <- subset(TCGModel2, Day == i)
  
  x_range <- max(max(TCGDayModel1$Lon), max(TCGDayModel2$Lon)) - min(min(TCGDayModel1$Lon), min(TCGDayModel2$Lon)) + 1
  y_range <- max(max(TCGDayModel1$Lat), max(TCGDayModel2$Lat)) - min(min(TCGDayModel1$Lat), min(TCGDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  TCGmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  TCGmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(TCGDayModel1$Conc[TCGDayModel1$Lon >= min(TCGDayModel1$Lon) + R*(k-1) &
                                       TCGDayModel1$Lon < min(TCGDayModel1$Lon) + R*k &
                                       TCGDayModel1$Lat >= min(TCGDayModel1$Lat) + R*(j-1) &
                                       TCGDayModel1$Lat < min(TCGDayModel1$Lat) + R*j])
      
      TCGmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(TCGDayModel2$Conc[TCGDayModel2$Lon >= min(TCGDayModel2$Lon) + R*(k-1) &
                                       TCGDayModel2$Lon < min(TCGDayModel2$Lon) + R*k &
                                       TCGDayModel2$Lat >= min(TCGDayModel2$Lat) + R*(j-1) &
                                       TCGDayModel2$Lat < min(TCGDayModel2$Lat) + R*j])
      
      TCGmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric3[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(TCGmatrix2 - TCGmatrix1))
  
}

###########

##### Save Data Frame #####
Output <- data.frame(Metric1, Metric2, Metric3)
write.csv(Output, "MRSmeasureB")



##### Model C



JECModel1 <- read.csv("JEC-C-2012-20km")[2:6]
JECModel2 <- read.csv("JEC-E-2012-20km")[2:6]

JSCModel1 <- read.csv("JSC-C-2012-20km")[2:6]
JSCModel2 <- read.csv("JSC-E-2012-20km")[2:6]

TCGModel1 <- read.csv("TCG-C-2012-20km")[2:6]
TCGModel2 <- read.csv("TCG-E-2012-20km")[2:6]


JECStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

JSCStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(36.99781, -84.59239)
)

TCGStackLoc <- rbind(
  c(46.75575, -122.85820),
  c(46.75513, -122.85810)
)

JECeGRIDLoc <- c(39.2865, -96.1172)
JSCeGRIDLoc <- c(36.9981, -84.5919)
TCGeGRIDLoc <- c(46.7559, -122.8598)

StartDay <- 2
EndDay <- 366

R <- 0.1

Metric1 = NULL
Metric2 = NULL
Metric3 = NULL



##### JEC Power Plant #####

for (i in StartDay:EndDay) {
  
  JECDayModel1 <- subset(JECModel1, Day == i)
  JECDayModel2 <- subset(JECModel2, Day == i)
  
  x_range <- max(max(JECDayModel1$Lon), max(JECDayModel2$Lon)) - min(min(JECDayModel1$Lon), min(JECDayModel2$Lon)) + 1
  y_range <- max(max(JECDayModel1$Lat), max(JECDayModel2$Lat)) - min(min(JECDayModel1$Lat), min(JECDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JECmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JECmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JECDayModel1$Conc[JECDayModel1$Lon >= min(JECDayModel1$Lon) + R*(k-1) &
                                       JECDayModel1$Lon < min(JECDayModel1$Lon) + R*k &
                                       JECDayModel1$Lat >= min(JECDayModel1$Lat) + R*(j-1) &
                                       JECDayModel1$Lat < min(JECDayModel1$Lat) + R*j])
      
      JECmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JECDayModel2$Conc[JECDayModel2$Lon >= min(JECDayModel2$Lon) + R*(k-1) &
                                       JECDayModel2$Lon < min(JECDayModel2$Lon) + R*k &
                                       JECDayModel2$Lat >= min(JECDayModel2$Lat) + R*(j-1) &
                                       JECDayModel2$Lat < min(JECDayModel2$Lat) + R*j])
      
      JECmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric1[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JECmatrix2 - JECmatrix1))
  
}




##### JSC Power Plant #####

for (i in StartDay:EndDay) {
  
  JSCDayModel1 <- subset(JSCModel1, Day == i)
  JSCDayModel2 <- subset(JSCModel2, Day == i)
  
  x_range <- max(max(JSCDayModel1$Lon), max(JSCDayModel2$Lon)) - min(min(JSCDayModel1$Lon), min(JSCDayModel2$Lon)) + 1
  y_range <- max(max(JSCDayModel1$Lat), max(JSCDayModel2$Lat)) - min(min(JSCDayModel1$Lat), min(JSCDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JSCmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JSCmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JSCDayModel1$Conc[JSCDayModel1$Lon >= min(JSCDayModel1$Lon) + R*(k-1) &
                                       JSCDayModel1$Lon < min(JSCDayModel1$Lon) + R*k &
                                       JSCDayModel1$Lat >= min(JSCDayModel1$Lat) + R*(j-1) &
                                       JSCDayModel1$Lat < min(JSCDayModel1$Lat) + R*j])
      
      JSCmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JSCDayModel2$Conc[JSCDayModel2$Lon >= min(JSCDayModel2$Lon) + R*(k-1) &
                                       JSCDayModel2$Lon < min(JSCDayModel2$Lon) + R*k &
                                       JSCDayModel2$Lat >= min(JSCDayModel2$Lat) + R*(j-1) &
                                       JSCDayModel2$Lat < min(JSCDayModel2$Lat) + R*j])
      
      JSCmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric2[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JSCmatrix2 - JSCmatrix1))
  
}



##### TCG Power Plant #####

for (i in StartDay:EndDay) {
  
  TCGDayModel1 <- subset(TCGModel1, Day == i)
  TCGDayModel2 <- subset(TCGModel2, Day == i)
  
  x_range <- max(max(TCGDayModel1$Lon), max(TCGDayModel2$Lon)) - min(min(TCGDayModel1$Lon), min(TCGDayModel2$Lon)) + 1
  y_range <- max(max(TCGDayModel1$Lat), max(TCGDayModel2$Lat)) - min(min(TCGDayModel1$Lat), min(TCGDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  TCGmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  TCGmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(TCGDayModel1$Conc[TCGDayModel1$Lon >= min(TCGDayModel1$Lon) + R*(k-1) &
                                       TCGDayModel1$Lon < min(TCGDayModel1$Lon) + R*k &
                                       TCGDayModel1$Lat >= min(TCGDayModel1$Lat) + R*(j-1) &
                                       TCGDayModel1$Lat < min(TCGDayModel1$Lat) + R*j])
      
      TCGmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(TCGDayModel2$Conc[TCGDayModel2$Lon >= min(TCGDayModel2$Lon) + R*(k-1) &
                                       TCGDayModel2$Lon < min(TCGDayModel2$Lon) + R*k &
                                       TCGDayModel2$Lat >= min(TCGDayModel2$Lat) + R*(j-1) &
                                       TCGDayModel2$Lat < min(TCGDayModel2$Lat) + R*j])
      
      TCGmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric3[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(TCGmatrix2 - TCGmatrix1))
  
}

###########

##### Save Data Frame #####
Output <- data.frame(Metric1, Metric2, Metric3)
write.csv(Output, "MRSmeasureC")




##### Model D #####




JECModel1 <- read.csv("JEC-D-2012-20km")[2:6]
JECModel2 <- read.csv("JEC-E-2012-20km")[2:6]

JSCModel1 <- read.csv("JSC-D-2012-20km")[2:6]
JSCModel2 <- read.csv("JSC-E-2012-20km")[2:6]

TCGModel1 <- read.csv("TCG-D-2012-20km")[2:6]
TCGModel2 <- read.csv("TCG-E-2012-20km")[2:6]


JECStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

JSCStackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(36.99781, -84.59239)
)

TCGStackLoc <- rbind(
  c(46.75575, -122.85820),
  c(46.75513, -122.85810)
)

JECeGRIDLoc <- c(39.2865, -96.1172)
JSCeGRIDLoc <- c(36.9981, -84.5919)
TCGeGRIDLoc <- c(46.7559, -122.8598)

StartDay <- 2
EndDay <- 366

R <- 0.1

Metric1 = NULL
Metric2 = NULL
Metric3 = NULL



##### JEC Power Plant #####

for (i in StartDay:EndDay) {
  
  JECDayModel1 <- subset(JECModel1, Day == i)
  JECDayModel2 <- subset(JECModel2, Day == i)
  
  x_range <- max(max(JECDayModel1$Lon), max(JECDayModel2$Lon)) - min(min(JECDayModel1$Lon), min(JECDayModel2$Lon)) + 1
  y_range <- max(max(JECDayModel1$Lat), max(JECDayModel2$Lat)) - min(min(JECDayModel1$Lat), min(JECDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JECmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JECmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JECDayModel1$Conc[JECDayModel1$Lon >= min(JECDayModel1$Lon) + R*(k-1) &
                                       JECDayModel1$Lon < min(JECDayModel1$Lon) + R*k &
                                       JECDayModel1$Lat >= min(JECDayModel1$Lat) + R*(j-1) &
                                       JECDayModel1$Lat < min(JECDayModel1$Lat) + R*j])
      
      JECmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JECDayModel2$Conc[JECDayModel2$Lon >= min(JECDayModel2$Lon) + R*(k-1) &
                                       JECDayModel2$Lon < min(JECDayModel2$Lon) + R*k &
                                       JECDayModel2$Lat >= min(JECDayModel2$Lat) + R*(j-1) &
                                       JECDayModel2$Lat < min(JECDayModel2$Lat) + R*j])
      
      JECmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric1[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JECmatrix2 - JECmatrix1))
  
}




##### JSC Power Plant #####

for (i in StartDay:EndDay) {
  
  JSCDayModel1 <- subset(JSCModel1, Day == i)
  JSCDayModel2 <- subset(JSCModel2, Day == i)
  
  x_range <- max(max(JSCDayModel1$Lon), max(JSCDayModel2$Lon)) - min(min(JSCDayModel1$Lon), min(JSCDayModel2$Lon)) + 1
  y_range <- max(max(JSCDayModel1$Lat), max(JSCDayModel2$Lat)) - min(min(JSCDayModel1$Lat), min(JSCDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  JSCmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  JSCmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(JSCDayModel1$Conc[JSCDayModel1$Lon >= min(JSCDayModel1$Lon) + R*(k-1) &
                                       JSCDayModel1$Lon < min(JSCDayModel1$Lon) + R*k &
                                       JSCDayModel1$Lat >= min(JSCDayModel1$Lat) + R*(j-1) &
                                       JSCDayModel1$Lat < min(JSCDayModel1$Lat) + R*j])
      
      JSCmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(JSCDayModel2$Conc[JSCDayModel2$Lon >= min(JSCDayModel2$Lon) + R*(k-1) &
                                       JSCDayModel2$Lon < min(JSCDayModel2$Lon) + R*k &
                                       JSCDayModel2$Lat >= min(JSCDayModel2$Lat) + R*(j-1) &
                                       JSCDayModel2$Lat < min(JSCDayModel2$Lat) + R*j])
      
      JSCmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric2[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(JSCmatrix2 - JSCmatrix1))
  
}



##### TCG Power Plant #####

for (i in StartDay:EndDay) {
  
  TCGDayModel1 <- subset(TCGModel1, Day == i)
  TCGDayModel2 <- subset(TCGModel2, Day == i)
  
  x_range <- max(max(TCGDayModel1$Lon), max(TCGDayModel2$Lon)) - min(min(TCGDayModel1$Lon), min(TCGDayModel2$Lon)) + 1
  y_range <- max(max(TCGDayModel1$Lat), max(TCGDayModel2$Lat)) - min(min(TCGDayModel1$Lat), min(TCGDayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R, 0)
  y_steps <- round(y_range/R, 0)
  
  TCGmatrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  TCGmatrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(TCGDayModel1$Conc[TCGDayModel1$Lon >= min(TCGDayModel1$Lon) + R*(k-1) &
                                       TCGDayModel1$Lon < min(TCGDayModel1$Lon) + R*k &
                                       TCGDayModel1$Lat >= min(TCGDayModel1$Lat) + R*(j-1) &
                                       TCGDayModel1$Lat < min(TCGDayModel1$Lat) + R*j])
      
      TCGmatrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(TCGDayModel2$Conc[TCGDayModel2$Lon >= min(TCGDayModel2$Lon) + R*(k-1) &
                                       TCGDayModel2$Lon < min(TCGDayModel2$Lon) + R*k &
                                       TCGDayModel2$Lat >= min(TCGDayModel2$Lat) + R*(j-1) &
                                       TCGDayModel2$Lat < min(TCGDayModel2$Lat) + R*j])
      
      TCGmatrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric3[i] <- 0.5*(2.4642e+14/1.2592e+10)*sum(abs(TCGmatrix2 - TCGmatrix1))
  
}

###########

##### Save Data Frame #####
Output <- data.frame(Metric1, Metric2, Metric3)
write.csv(Output, "MRSmeasureD")
