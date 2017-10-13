### Grid Dispersion ###
# Load required libraries
library(ggplot2)
library(reshape2)
library(scales)
library(ggmap)
library(geosphere)
#library(mailR)
source("DEMOFunctions.R")
source("EmailFunction.R")

# Read in file
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
PlantLAT <- 39.28682
PlantLON <- -96.1172
Emissions <- 12591532084.8523

DispersionAtOrigin <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)

#########################################################################################################################
########## Shifted Dispersion ###########################################################################################
#########################################################################################################################

ShiftedMetricValues <- data.frame()
for(i in 0:50) {
  
    Model1 <- DispersionAtOrigin
  
    ShiftedDispersion <- ShiftDispersion(DispersionAtOrigin, i)
    
    Model2 <- ShiftedDispersion
    
    x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
    y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
    
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
    
    Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    LAT1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    LON1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    
    minLON <- min(min(Model1$LON), min(Model2$LON))
    minLAT <- min(min(Model1$LAT), min(Model2$LAT))
    
    for (g in 1:y_steps) {
      
      for(h in 1:x_steps) {
        
        CellAveragedPollutant_1 <- mean(Model1[,7][Model1$LON >= minLON + Resolution*(h-1) & Model1$LON < minLON + Resolution*h &
                                                     Model1$LAT >= minLAT + Resolution*(g-1) & Model1$LAT < minLAT + Resolution*g])
        
        CellAveragedPollutant_2 <- mean(Model2[,7][Model2$LON >= minLON + Resolution*(h-1) & Model2$LON < minLON + Resolution*h &
                                                     Model2$LAT >= minLAT + Resolution*(g-1) & Model2$LAT < minLAT + Resolution*g])
        
        Model1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
        Model2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
        
      }
    }
    
    Gridded_Model1 <- melt(Model1_Matrix)
    Gridded_Model2 <- melt(Model2_Matrix)
    
    names(Gridded_Model1) <- c("LAT", "LON", "CO2")
    names(Gridded_Model2) <- c("LAT", "LON", "CO2")
    
    Gridded_Model1$LAT <- minLAT + Resolution*(Gridded_Model1$LAT + 0.05)
    Gridded_Model1$LON <- minLON + Resolution*(Gridded_Model1$LON + 0.05)
    Gridded_Model2$LAT <- minLAT + Resolution*(Gridded_Model2$LAT + 0.05)
    Gridded_Model2$LON <- minLON + Resolution*(Gridded_Model2$LON + 0.05)
    
    Gridded_Model1$LAT[Gridded_Model1$CO2 == 0] <- NA
    Gridded_Model1$LON[Gridded_Model1$CO2 == 0] <- NA
    Gridded_Model2$LAT[Gridded_Model2$CO2 == 0] <- NA
    Gridded_Model2$LON[Gridded_Model2$CO2 == 0] <- NA
    
    Gridded_Model2$CO2 <- (sum(na.omit(Gridded_Model1$CO2))/sum(na.omit(Gridded_Model2$CO2)))*Gridded_Model2$CO2
    
    ##### Metric Calculations Here #####
    
    ShiftedMetricValues[i+1, 1] <- i/10
    
    ShiftedMetricValues[i+1, 2] <- 0.5*(1/sum(na.omit(Gridded_Model1$CO2)))*sum(abs(Gridded_Model1$CO2 - Gridded_Model2$CO2))*100
    
    Angles1 <- (180/pi)*atan2(Gridded_Model1$LAT, Gridded_Model1$LON)
    MeanAngle1 <- sum(na.omit(Angles1*Gridded_Model1$CO2))/sum(na.omit(Gridded_Model1$CO2))
    Angles2 <- (180/pi)*atan2(Gridded_Model2$LAT, Gridded_Model2$LON)
    MeanAngle2 <- sum(na.omit(Angles2*Gridded_Model2$CO2))/sum(na.omit(Gridded_Model2$CO2))
    ShiftedMetricValues[i+1, 3] <- abs(MeanAngle1 - MeanAngle2)
    
    StdAngle1 <- sqrt(sum(na.omit(Gridded_Model1$CO2*(Angles1 - MeanAngle1)^2))/sum(Gridded_Model1$CO2))*sqrt(nrow(na.omit(Gridded_Model1))/(nrow(na.omit(Gridded_Model1)) - 1))
    StdAngle2 <- sqrt(sum(na.omit(Gridded_Model2$CO2*(Angles2 - MeanAngle2)^2))/sum(Gridded_Model2$CO2))*sqrt(nrow(na.omit(Gridded_Model2))/(nrow(na.omit(Gridded_Model2)) - 1))
    ShiftedMetricValues[i+1, 4] <- abs(StdAngle1 - StdAngle2)

    x1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LON))/sum(Gridded_Model1$CO2)
    y1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LAT))/sum(Gridded_Model1$CO2)
    x2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LON))/sum(Gridded_Model2$CO2)
    y2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LAT))/sum(Gridded_Model2$CO2)
    ShiftedMetricValues[i+1, 5] <- 111*abs(sqrt((x1-x2)^2 + (y1-y2)^2))
}

names(ShiftedMetricValues) <- c("ShiftFactor", "MetricValue", "MeanAngleValue", "StdAngleValue", "COM")

plot(ShiftedMetricValues$MetricValue ~ ShiftedMetricValues$ShiftFactor)
plot(ShiftedMetricValues$MeanAngleValue ~ ShiftedMetricValues$ShiftFactor)
plot(ShiftedMetricValues$StdAngleValue ~ ShiftedMetricValues$ShiftFactor)
plot(ShiftedMetricValues$COM ~ ShiftedMetricValues$ShiftFactor)



#########################################################################################################################
########## Rotated Dispersion ###########################################################################################
#########################################################################################################################

RotatedMetricValues <- data.frame()
for(i in 0:200) {
  
  Model1 <- DispersionAtOrigin
  
  ShiftedDispersion <- RotateDispersion(DispersionAtOrigin, i)
  
  Model2 <- ShiftedDispersion
  
  x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
  y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
  
  x_steps <- round(x_range/Resolution, 0)
  y_steps <- round(y_range/Resolution, 0)
  
  Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  LAT1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  LON1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  minLON <- min(min(Model1$LON), min(Model2$LON))
  minLAT <- min(min(Model1$LAT), min(Model2$LAT))
  
  for (g in 1:y_steps) {
    
    for(h in 1:x_steps) {
      
      CellAveragedPollutant_1 <- mean(Model1[,7][Model1$LON >= minLON + Resolution*(h-1) & Model1$LON < minLON + Resolution*h &
                                                   Model1$LAT >= minLAT + Resolution*(g-1) & Model1$LAT < minLAT + Resolution*g])
      
      CellAveragedPollutant_2 <- mean(Model2[,7][Model2$LON >= minLON + Resolution*(h-1) & Model2$LON < minLON + Resolution*h &
                                                   Model2$LAT >= minLAT + Resolution*(g-1) & Model2$LAT < minLAT + Resolution*g])
      
      Model1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
      Model2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
      
    }
  }
  
  Gridded_Model1 <- melt(Model1_Matrix)
  Gridded_Model2 <- melt(Model2_Matrix)
  
  names(Gridded_Model1) <- c("LAT", "LON", "CO2")
  names(Gridded_Model2) <- c("LAT", "LON", "CO2")
  
  Gridded_Model1$LAT <- minLAT + Resolution*(Gridded_Model1$LAT + 0.05)
  Gridded_Model1$LON <- minLON + Resolution*(Gridded_Model1$LON + 0.05)
  Gridded_Model2$LAT <- minLAT + Resolution*(Gridded_Model2$LAT + 0.05)
  Gridded_Model2$LON <- minLON + Resolution*(Gridded_Model2$LON + 0.05)
  
  Gridded_Model1$LAT[Gridded_Model1$CO2 == 0] <- NA
  Gridded_Model1$LON[Gridded_Model1$CO2 == 0] <- NA
  Gridded_Model2$LAT[Gridded_Model2$CO2 == 0] <- NA
  Gridded_Model2$LON[Gridded_Model2$CO2 == 0] <- NA
  
  Gridded_Model2$CO2 <- (sum(na.omit(Gridded_Model1$CO2))/sum(na.omit(Gridded_Model2$CO2)))*Gridded_Model2$CO2
  
  ##### Metric Calculations Here #####
  
  RotatedMetricValues[i+1, 1] <- i/10
  
  RotatedMetricValues[i+1, 2] <- 0.5*(1/sum(na.omit(Gridded_Model1$CO2)))*sum(abs(Gridded_Model1$CO2 - Gridded_Model2$CO2))*100
  
  Angles1 <- (180/pi)*atan2(Gridded_Model1$LAT, Gridded_Model1$LON)
  MeanAngle1 <- sum(na.omit(Angles1*Gridded_Model1$CO2))/sum(na.omit(Gridded_Model1$CO2))
  Angles2 <- (180/pi)*atan2(Gridded_Model2$LAT, Gridded_Model2$LON)
  MeanAngle2 <- sum(na.omit(Angles2*Gridded_Model2$CO2))/sum(na.omit(Gridded_Model2$CO2))
  RotatedMetricValues[i+1, 3] <- abs(MeanAngle1 - MeanAngle2)
  
  StdAngle1 <- sqrt(sum(na.omit(Gridded_Model1$CO2*(Angles1 - MeanAngle1)^2))/sum(Gridded_Model1$CO2))*sqrt(nrow(na.omit(Gridded_Model1))/(nrow(na.omit(Gridded_Model1)) - 1))
  StdAngle2 <- sqrt(sum(na.omit(Gridded_Model2$CO2*(Angles2 - MeanAngle2)^2))/sum(Gridded_Model2$CO2))*sqrt(nrow(na.omit(Gridded_Model2))/(nrow(na.omit(Gridded_Model2)) - 1))
  RotatedMetricValues[i+1, 4] <- abs(StdAngle1 - StdAngle2)
  
  x1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LON))/sum(Gridded_Model1$CO2)
  y1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LAT))/sum(Gridded_Model1$CO2)
  x2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LON))/sum(Gridded_Model2$CO2)
  y2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LAT))/sum(Gridded_Model2$CO2)
  RotatedMetricValues[i+1, 5] <- 111*abs(sqrt((x1-x2)^2 + (y1-y2)^2))
}

names(RotatedMetricValues) <- c("ShiftFactor", "MetricValue", "MeanAngleValue", "StdAngleValue", "COM")

plot(RotatedMetricValues$MetricValue ~ RotatedMetricValues$ShiftFactor)
plot(RotatedMetricValues$MeanAngleValue ~ RotatedMetricValues$ShiftFactor)
plot(RotatedMetricValues$StdAngleValue ~ RotatedMetricValues$ShiftFactor)
plot(RotatedMetricValues$COM ~ RotatedMetricValues$ShiftFactor)
