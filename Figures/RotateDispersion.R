# Dustin Roten 11/05/2017
# This script is responsible for metrics for the shifted dispersion scenario ONLY.

# Load required libraries
library(ggplot2)
library(reshape)
source("TEST-DEMOFunctions.R")

# The required information for the functions used includes the (Lat, Lon) of the plant
# as well as the desired gridding resolution.
PlantLAT <- 39.28682
PlantLON <- -96.1172

# Read in the file and translate the coordinates to the origin.
# It has been renamed to a generic "x" variable since the same section
# of code will be used in other scripts.
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")
Origin_Dispersion <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)
x <- Origin_Dispersion

##### Shift Dispersion Below #####
MRSMeasure = NULL
COMMeasure = NULL
AngleMeasure = NULL
STDAngleMeasure = NULL

for(i in 0:200) {

    Rotated_Dispersion <- RotateDispersion(Origin_Dispersion, i)
    
    Matrix_Origin_Dispersion <- GridDispersions2(Origin_Dispersion, Rotated_Dispersion, Resolution, 1)
    Matrix_Rotated_Dispersion <- GridDispersions2(Origin_Dispersion, Rotated_Dispersion, Resolution, 2)
    Origin <- GridDispersions2(Origin_Dispersion, Rotated_Dispersion, Resolution, "O")
    
    Matrix_Rotated_Dispersion <- Matrix_Rotated_Dispersion*(sum(Matrix_Origin_Dispersion)/sum(Matrix_Rotated_Dispersion))
    
    MRSMeasure[i+1] <- (1/(2*sum(Matrix_Origin_Dispersion)))*sum(abs(Matrix_Origin_Dispersion - Matrix_Rotated_Dispersion))*100
    
    # "Spatial" Matrices
    Melted_Origin_Dispersion <- melt(Matrix_Origin_Dispersion)
    Melted_Rotated_Dispersion <- melt(Matrix_Rotated_Dispersion)
    
    Melted_Origin_Dispersion <- subset(Melted_Origin_Dispersion, Melted_Origin_Dispersion$value != 0)
    Melted_Rotated_Dispersion <- subset(Melted_Rotated_Dispersion, Melted_Rotated_Dispersion$value != 0)
    
    Melted_Origin_Dispersion$X1 <- Melted_Origin_Dispersion$X1 - Origin[1]
    Melted_Origin_Dispersion$X2 <- Melted_Origin_Dispersion$X2 - Origin[2]
    Melted_Rotated_Dispersion$X1 <- Melted_Rotated_Dispersion$X1 - Origin[1]
    Melted_Rotated_Dispersion$X2 <- Melted_Rotated_Dispersion$X2 - Origin[2]
    
    names(Melted_Origin_Dispersion) <- c("Y", "X", "CO2")
    names(Melted_Rotated_Dispersion) <- c("Y", "X", "CO2")
    
    # Center of Mass Calculation
    x1 <- sum(Melted_Origin_Dispersion$X * Melted_Origin_Dispersion$CO2)/sum(Melted_Origin_Dispersion$CO2)
    y1 <- sum(Melted_Origin_Dispersion$Y * Melted_Origin_Dispersion$CO2)/sum(Melted_Origin_Dispersion$CO2)
    x2 <- sum(Melted_Rotated_Dispersion$X * Melted_Rotated_Dispersion$CO2)/sum(Melted_Rotated_Dispersion$CO2)
    y2 <- sum(Melted_Rotated_Dispersion$Y * Melted_Rotated_Dispersion$CO2)/sum(Melted_Rotated_Dispersion$CO2)
    
    COMMeasure[i+1] <- 111*Resolution*sqrt((x2 - x1)^2 + (y2 - y1)^2)
    
    # Mean Angle Calculation
    Angle1 <- if( (180/pi)*atan2(y1, x1) < 0 ) {360 + (180/pi)*atan2(y1, x1)} else {(180/pi)*atan2(y1, x1)}
    Angle2 <- if( (180/pi)*atan2(y2, x2) < 0 ) {360 + (180/pi)*atan2(y2, x2)} else {(180/pi)*atan2(y2, x2)}
    
    AngleMeasure[i+1] <- if(abs(Angle1 - Angle2) > 180) {360 - abs(Angle1 - Angle2)} else {abs(Angle1 - Angle2)}
    
    # Standard Deviation Calculation
    NormalizedAxis_Melted_Origin_Dispersion <- data.frame(
        Melted_Origin_Dispersion$X*sin(-Angle1*pi/180) + Melted_Origin_Dispersion$Y*cos(-Angle1*pi/180),
        Melted_Origin_Dispersion$X*cos(-Angle1*pi/180) - Melted_Origin_Dispersion$Y*sin(-Angle1*pi/180),
        Melted_Origin_Dispersion$CO2
        )
    names(NormalizedAxis_Melted_Origin_Dispersion) <- c("Y", "X", "CO2")
    
    NormalizedAxis_Melted_Rotated_Dispersion <- data.frame(
      Melted_Rotated_Dispersion$X*sin(-Angle2*pi/180) + Melted_Rotated_Dispersion$Y*cos(-Angle2*pi/180),
      Melted_Rotated_Dispersion$X*cos(-Angle2*pi/180) - Melted_Rotated_Dispersion$Y*sin(-Angle2*pi/180),
      Melted_Rotated_Dispersion$CO2
    )
    names(NormalizedAxis_Melted_Rotated_Dispersion) <- c("Y", "X", "CO2")
    
    STDAngle1 <- sd((180/pi)*atan2(NormalizedAxis_Melted_Origin_Dispersion$Y, NormalizedAxis_Melted_Origin_Dispersion$X))
    STDAngle2 <- sd((180/pi)*atan2(NormalizedAxis_Melted_Rotated_Dispersion$Y, NormalizedAxis_Melted_Rotated_Dispersion$X))
    
    STDAngleMeasure[i+1] <- abs(STDAngle1 - STDAngle2)
      
}

Metrics_Rotate <- data.frame(c(0:200)/100, MRSMeasure, COMMeasure, AngleMeasure, STDAngleMeasure)
names(Metrics_Rotate) <- c("ShiftDegree", "MRS_Measure", "COM_Measure", "MeanAngle_Measure", "STDAngle_Measure")