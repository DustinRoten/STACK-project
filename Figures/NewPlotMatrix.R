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
Resolution <- 0.1

DispersionAtOrigin <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)

#########################################################################################################################
########## Shifted Dispersion ###########################################################################################
#########################################################################################################################

ShiftedMetricValues <- data.frame()
for(i in 0:50) {
  
    Model1 <- DispersionAtOrigin
  
    ShiftedDispersion <- ShiftDispersion(DispersionAtOrigin, i)
    
    Model2 <- ShiftedDispersion
    
    # Determine the size of the collective area of both dispersions
    x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
    y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
    
    # Translate the area into the number of grid cells based on the Resolution required.
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
    
    # Set up 2 matrices to store values in
    Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    
    # These values are going to be used in interations to define the bounds of each grid cell
    minLON <- min(min(Model1$LON), min(Model2$LON))
    minLAT <- min(min(Model1$LAT), min(Model2$LAT))
    
    # Grid the dispersions
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
    
    # Melt the matrices for easier plotting
    Gridded_Model1 <- melt(Model1_Matrix)
    Gridded_Model2 <- melt(Model2_Matrix)
    names(Gridded_Model1) <- c("LAT", "LON", "CO2")
    names(Gridded_Model2) <- c("LAT", "LON", "CO2")
    
    # Calibrate cell numbers to LAT/LON
    Gridded_Model1$LAT <- minLAT + Resolution*(Gridded_Model1$LAT + 0.05)
    Gridded_Model1$LON <- minLON + Resolution*(Gridded_Model1$LON + 0.05)
    Gridded_Model2$LAT <- minLAT + Resolution*(Gridded_Model2$LAT + 0.05)
    Gridded_Model2$LON <- minLON + Resolution*(Gridded_Model2$LON + 0.05)
    
    # Place NA's in each cell containing 0 so they are not graphed
    Gridded_Model1$LAT[Gridded_Model1$CO2 == 0] <- NA
    Gridded_Model1$LON[Gridded_Model1$CO2 == 0] <- NA
    Gridded_Model2$LAT[Gridded_Model2$CO2 == 0] <- NA
    Gridded_Model2$LON[Gridded_Model2$CO2 == 0] <- NA
    
    Gridded_Model2$CO2 <- (sum(na.omit(Gridded_Model1$CO2))/sum(na.omit(Gridded_Model2$CO2)))*Gridded_Model2$CO2
    
    ##### Metric Calculations Here #####
    
    ShiftedMetricValues[i+1, 1] <- i/10 # Shifts for translation
    
    ShiftedMetricValues[i+1, 2] <- 0.5*(1/sum(na.omit(Gridded_Model1$CO2)))*sum(abs(Gridded_Model1$CO2 - Gridded_Model2$CO2))*100 # MRS Measure
    
    Angles1 <- (180/pi)*atan2(Gridded_Model1$LAT, Gridded_Model1$LON)
    MeanAngle1 <- sum(na.omit(Angles1*Gridded_Model1$CO2))/sum(na.omit(Gridded_Model1$CO2))
    Angles2 <- (180/pi)*atan2(Gridded_Model2$LAT, Gridded_Model2$LON)
    MeanAngle2 <- sum(na.omit(Angles2*Gridded_Model2$CO2))/sum(na.omit(Gridded_Model2$CO2))
    ShiftedMetricValues[i+1, 3] <- abs(MeanAngle1 - MeanAngle2) # Mean Angle Measure
    
    StdAngle1 <- sqrt(sum(na.omit(Gridded_Model1$CO2*(Angles1 - MeanAngle1)^2))/sum(Gridded_Model1$CO2))*sqrt(nrow(na.omit(Gridded_Model1))/(nrow(na.omit(Gridded_Model1)) - 1))
    StdAngle2 <- sqrt(sum(na.omit(Gridded_Model2$CO2*(Angles2 - MeanAngle2)^2))/sum(Gridded_Model2$CO2))*sqrt(nrow(na.omit(Gridded_Model2))/(nrow(na.omit(Gridded_Model2)) - 1))
    ShiftedMetricValues[i+1, 4] <- abs(StdAngle1 - StdAngle2) # Std Dev Measure

    x1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LON))/sum(Gridded_Model1$CO2)
    y1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LAT))/sum(Gridded_Model1$CO2)
    x2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LON))/sum(Gridded_Model2$CO2)
    y2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LAT))/sum(Gridded_Model2$CO2)
    ShiftedMetricValues[i+1, 5] <- 111*abs(sqrt((x1-x2)^2 + (y1-y2)^2)) # COM Measure
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
  
  RotatedMetricValues[i+1, 1] <- i/100
  
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

names(RotatedMetricValues) <- c("RotationFactor", "MetricValue", "MeanAngleValue", "StdAngleValue", "COM")

plot(RotatedMetricValues$MetricValue ~ RotatedMetricValues$RotationFactor)
plot(RotatedMetricValues$MeanAngleValue ~ RotatedMetricValues$RotationFactor)
plot(RotatedMetricValues$StdAngleValue ~ RotatedMetricValues$RotationFactor)
plot(RotatedMetricValues$COM ~ RotatedMetricValues$RotationFactor)



#########################################################################################################################
########## Radially Stretched Dispersion ################################################################################
#########################################################################################################################

RadialMetricValues <- data.frame()
for(i in 0:99) {
  
  Model1 <- DispersionAtOrigin
  
  ShiftedDispersion <- RadialDilation(DispersionAtOrigin, i)
  
  Model2 <- ShiftedDispersion
  
  x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
  y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
  
  x_steps <- round(x_range/Resolution, 0)
  y_steps <- round(y_range/Resolution, 0)
  
  Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
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
  
  RadialMetricValues[i+1, 1] <- 1 + i/100
  
  RadialMetricValues[i+1, 2] <- 0.5*(1/sum(na.omit(Gridded_Model1$CO2)))*sum(abs(Gridded_Model1$CO2 - Gridded_Model2$CO2))*100
  
  Angles1 <- (180/pi)*atan2(Gridded_Model1$LAT, Gridded_Model1$LON)
  MeanAngle1 <- sum(na.omit(Angles1*Gridded_Model1$CO2))/sum(na.omit(Gridded_Model1$CO2))
  Angles2 <- (180/pi)*atan2(Gridded_Model2$LAT, Gridded_Model2$LON)
  MeanAngle2 <- sum(na.omit(Angles2*Gridded_Model2$CO2))/sum(na.omit(Gridded_Model2$CO2))
  RadialMetricValues[i+1, 3] <- abs(MeanAngle1 - MeanAngle2)
  
  StdAngle1 <- sqrt(sum(na.omit(Gridded_Model1$CO2*(Angles1 - MeanAngle1)^2))/sum(Gridded_Model1$CO2))*sqrt(nrow(na.omit(Gridded_Model1))/(nrow(na.omit(Gridded_Model1)) - 1))
  StdAngle2 <- sqrt(sum(na.omit(Gridded_Model2$CO2*(Angles2 - MeanAngle2)^2))/sum(Gridded_Model2$CO2))*sqrt(nrow(na.omit(Gridded_Model2))/(nrow(na.omit(Gridded_Model2)) - 1))
  RadialMetricValues[i+1, 4] <- abs(StdAngle1 - StdAngle2)
  
  x1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LON))/sum(Gridded_Model1$CO2)
  y1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LAT))/sum(Gridded_Model1$CO2)
  x2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LON))/sum(Gridded_Model2$CO2)
  y2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LAT))/sum(Gridded_Model2$CO2)
  RadialMetricValues[i+1, 5] <- 111*abs(sqrt((x1-x2)^2 + (y1-y2)^2))
}

names(RadialMetricValues) <- c("DilationFactor", "MetricValue", "MeanAngleValue", "StdAngleValue", "COM")

plot(RadialMetricValues$MetricValue ~ RadialMetricValues$DilationFactor)
plot(RadialMetricValues$MeanAngleValue ~ RadialMetricValues$DilationFactor)
plot(RadialMetricValues$StdAngleValue ~ RadialMetricValues$DilationFactor)
plot(RadialMetricValues$COM ~ RadialMetricValues$DilationFactor)



#########################################################################################################################
########## Angularly Stretched Dispersion ###############################################################################
#########################################################################################################################

AngularMetricValues <- data.frame()
for(i in 0:99) {
  
  Model1 <- DispersionAtOrigin
  
  ShiftedDispersion <- AngularStretch(DispersionAtOrigin, i)
  
  Model2 <- ShiftedDispersion
  
  x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
  y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
  
  x_steps <- round(x_range/Resolution, 0)
  y_steps <- round(y_range/Resolution, 0)
  
  Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
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
  
  AngularMetricValues[i+1, 1] <- 1 + i/100
  
  AngularMetricValues[i+1, 2] <- 0.5*(1/sum(na.omit(Gridded_Model1$CO2)))*sum(abs(Gridded_Model1$CO2 - Gridded_Model2$CO2))*100
  
  Angles1 <- (180/pi)*atan2(Gridded_Model1$LAT, Gridded_Model1$LON)
  MeanAngle1 <- sum(na.omit(Angles1*Gridded_Model1$CO2))/sum(na.omit(Gridded_Model1$CO2))
  Angles2 <- (180/pi)*atan2(Gridded_Model2$LAT, Gridded_Model2$LON)
  MeanAngle2 <- sum(na.omit(Angles2*Gridded_Model2$CO2))/sum(na.omit(Gridded_Model2$CO2))
  AngularMetricValues[i+1, 3] <- abs(MeanAngle1 - MeanAngle2)
  
  StdAngle1 <- sqrt(sum(na.omit(Gridded_Model1$CO2*(Angles1 - MeanAngle1)^2))/sum(Gridded_Model1$CO2))*sqrt(nrow(na.omit(Gridded_Model1))/(nrow(na.omit(Gridded_Model1)) - 1))
  StdAngle2 <- sqrt(sum(na.omit(Gridded_Model2$CO2*(Angles2 - MeanAngle2)^2))/sum(Gridded_Model2$CO2))*sqrt(nrow(na.omit(Gridded_Model2))/(nrow(na.omit(Gridded_Model2)) - 1))
  AngularMetricValues[i+1, 4] <- abs(StdAngle1 - StdAngle2)
  
  x1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LON))/sum(Gridded_Model1$CO2)
  y1 <- sum(na.omit(Gridded_Model1$CO2*Gridded_Model1$LAT))/sum(Gridded_Model1$CO2)
  x2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LON))/sum(Gridded_Model2$CO2)
  y2 <- sum(na.omit(Gridded_Model2$CO2*Gridded_Model2$LAT))/sum(Gridded_Model2$CO2)
  AngularMetricValues[i+1, 5] <- 111*abs(sqrt((x1-x2)^2 + (y1-y2)^2))
}

names(AngularMetricValues) <- c("DilationFactor", "MetricValue", "MeanAngleValue", "StdAngleValue", "COM")

plot(AngularMetricValues$MetricValue ~ AngularMetricValues$DilationFactor)
plot(AngularMetricValues$MeanAngleValue ~ AngularMetricValues$DilationFactor)
plot(AngularMetricValues$StdAngleValue ~ AngularMetricValues$DilationFactor)
plot(AngularMetricValues$COM ~ AngularMetricValues$DilationFactor)


###################################################################################################
########## All Plots are Generated Below ##########################################################
###################################################################################################

### Plot 1: Shift Metric, MRS ###
p <- ggplot(data = ShiftedMetricValues, aes(x = ShiftFactor, y = MetricValue)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0,max(max(1.01*ShiftedMetricValues$MetricValue), max(1.01*RotatedMetricValues$MetricValue), max(1.01*RadialMetricValues$MetricValue), max(1.01*AngularMetricValues$MetricValue) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Shift Metric, MeanAngle ###
p <- ggplot(data = ShiftedMetricValues, aes(x = ShiftFactor, y = MeanAngleValue)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*ShiftedMetricValues$MeanAngleValue), max(1.01*RotatedMetricValues$MeanAngleValue), max(1.01*RadialMetricValues$MeanAngleValue), max(1.01*AngularMetricValues$MeanAngleValue) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Shift Metric, STDAngle ###
p <- ggplot(data = ShiftedMetricValues, aes(x = ShiftFactor, y = StdAngleValue)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*ShiftedMetricValues$StdAngleValue), max(1.01*RotatedMetricValues$StdAngleValue), max(1.01*RadialMetricValues$StdAngleValue), max(1.01*AngularMetricValues$StdAngleValue) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Shift Metric, COM ###
p <- ggplot(data = ShiftedMetricValues, aes(x = ShiftFactor, y = COM)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*ShiftedMetricValues$COM), max(1.01*RotatedMetricValues$COM), max(1.01*RadialMetricValues$COM), max(1.01*AngularMetricValues$COM) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

####################################################################################################################
####################################################################################################################
####################################################################################################################

### Plot 1: Rotation Metric, MRS ###
p <- ggplot(data = RotatedMetricValues, aes(x = RotationFactor, y = MetricValue)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotation-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Rotation Metric, MeanAngle ###
p <- ggplot(data = RotatedMetricValues, aes(x = RotationFactor, y = MeanAngleValue)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotation-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Rotation Metric, STDAngle ###
p <- ggplot(data = RotatedMetricValues, aes(x = RotationFactor, y = StdAngleValue)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotation-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Rotation Metric, COM ###
p <- ggplot(data = RotatedMetricValues, aes(x = RotationFactor, y = COM)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotation-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

####################################################################################################################
####################################################################################################################
####################################################################################################################

### Plot 1: Radial Stretch Metric, MRS ###
p <- ggplot(data = RadialMetricValues, aes(x = DilationFactor, y = MetricValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Radial Stretch Metric, MeanAngle ###
p <- ggplot(data = RadialMetricValues, aes(x = DilationFactor, y = MeanAngleValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Radial Stretch Metric, STDAngle ###
p <- ggplot(data = RadialMetricValues, aes(x = DilationFactor, y = StdAngleValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Radial Stretch Metric, COM ###
p <- ggplot(data = RadialMetricValues, aes(x = DilationFactor, y = COM)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

####################################################################################################################
####################################################################################################################
####################################################################################################################

### Plot 1: Angular Stretch Metric, MRS ###
p <- ggplot(data = AngularMetricValues, aes(x = DilationFactor, y = MetricValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("AngStretch-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Angular Stretch Metric, MeanAngle ###
p <- ggplot(data = AngularMetricValues, aes(x = DilationFactor, y = MeanAngleValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("AngStretch-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Angular Stretch Metric, STDAngle ###
p <- ggplot(data = AngularMetricValues, aes(x = DilationFactor, y = StdAngleValue)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("AngStretch-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Angular Stretch Metric, COM ###
p <- ggplot(data = AngularMetricValues, aes(x = DilationFactor, y = COM)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("AngStretch-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")



##### Moving files #####
NAME <- c("Shift-COM-Calibration.jpg",
          "Shift-MRS-Calibration.jpg",
          "Shift-MeanAngle-Calibration.jpg",
          "Shift-STDAngle-Calibration.jpg",
          "RadStretch-COM-Calibration.jpg",
          "RadStretch-MRS-Calibration.jpg",
          "RadStretch-MeanAngle-Calibration.jpg",
          "RadStretch-STDAngle-Calibration.jpg",
          "Rotation-COM-Calibration.jpg",
          "Rotation-MRS-Calibration.jpg",
          "Rotation-MeanAngle-Calibration.jpg",
          "Rotation-STDAngle-Calibration.jpg",
          "AngStretch-COM-Calibration.jpg",
          "AngStretch-MRS-Calibration.jpg",
          "AngStretch-MeanAngle-Calibration.jpg",
          "AngStretch-STDAngle-Calibration.jpg")

TEST = NULL
for(i in 1:length(NAME)) {
  
  TEST[i] <- file.exists(paste(NAME[i]))
  file.copy(from = paste(NAME[i]), to = paste("~/Google Drive/NASA/HysplitPaper1/images/",NAME[i], sep = ""), overwrite = TRUE)
  file.remove(paste(NAME[i]))
  TEST
}