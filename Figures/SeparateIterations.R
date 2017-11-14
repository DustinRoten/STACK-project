library(ggplot2)
library(reshape)
library(ggmap)
source("TEST-DEMOFunctions.R")

### Radial Dispersion ###

# The required information for the functions used includes the (Lat, Lon) of the plant
# as well as the desired gridding resolution.
PlantLAT <- 39.28682
PlantLON <- -96.1172
Resolution <- 0.1

# Read in the file and translate the coordinates to the origin.
# It has been renamed to a generic "x" variable since the same section
# of code will be used in other scripts.
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")
Origin_Dispersion <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)
x <- Origin_Dispersion

MRSMeasure = NULL
COMMeasure = NULL
AngleMeasure = NULL
STDAngleMeasure = NULL

COMMeasure_Mod1 = NULL
COMMeasure_Mod2 = NULL

AngleMeasure_Mod1 = NULL
AngleMeasure_Mod2 = NULL

STDAngleMeasure_Mod1 = NULL
STDAngleMeasure_Mod2 = NULL

Steps <- c(0, 10, 20, 30, 40, 50, 60)

j = 1

map <- get_map(location = c(PlantLON + 2, PlantLAT + 4), zoom =5, maptype = "terrain", color = "bw")

for(i in 0:6) {
  
    Radial_Dispersion <- AngularStretch(Origin_Dispersion, Steps[i+1])
  
    Matrix_Origin_Dispersion <- GridDispersions2(Origin_Dispersion, Radial_Dispersion, 0.1, 1)
    Matrix_Radial_Dispersion <- GridDispersions2(Origin_Dispersion, Radial_Dispersion, 0.1, 2)
    Origin <- GridDispersions2(Origin_Dispersion, Radial_Dispersion, 0.1, "O")
  
    Matrix_Radial_Dispersion <- Matrix_Radial_Dispersion*(sum(Matrix_Origin_Dispersion)/sum(Matrix_Radial_Dispersion))
  
    MRSMeasure[j] <- (1/(2*sum(Matrix_Origin_Dispersion)))*sum(abs(Matrix_Origin_Dispersion - Matrix_Radial_Dispersion))*100
  
    # "Spatial" Matrices
    Melted_Origin_Dispersion <- melt(Matrix_Origin_Dispersion)
    Melted_Radial_Dispersion <- melt(Matrix_Radial_Dispersion)
  
    Melted_Origin_Dispersion <- subset(Melted_Origin_Dispersion, Melted_Origin_Dispersion$value != 0)
    Melted_Radial_Dispersion <- subset(Melted_Radial_Dispersion, Melted_Radial_Dispersion$value != 0)
  
    Melted_Origin_Dispersion$X1 <- Melted_Origin_Dispersion$X1 - Origin[1]
    Melted_Origin_Dispersion$X2 <- Melted_Origin_Dispersion$X2 - Origin[2]
    Melted_Radial_Dispersion$X1 <- Melted_Radial_Dispersion$X1 - Origin[1]
    Melted_Radial_Dispersion$X2 <- Melted_Radial_Dispersion$X2 - Origin[2]
  
    names(Melted_Origin_Dispersion) <- c("Y", "X", "CO2")
    names(Melted_Radial_Dispersion) <- c("Y", "X", "CO2")
  
    # Center of Mass Calculation
    x1 <- sum(Melted_Origin_Dispersion$X * Melted_Origin_Dispersion$CO2)/sum(Melted_Origin_Dispersion$CO2)
    y1 <- sum(Melted_Origin_Dispersion$Y * Melted_Origin_Dispersion$CO2)/sum(Melted_Origin_Dispersion$CO2)
    x2 <- sum(Melted_Radial_Dispersion$X * Melted_Radial_Dispersion$CO2)/sum(Melted_Radial_Dispersion$CO2)
    y2 <- sum(Melted_Radial_Dispersion$Y * Melted_Radial_Dispersion$CO2)/sum(Melted_Radial_Dispersion$CO2)
  
    COMMeasure_Mod1[i+1] <- 111*0.1*sqrt(x1^2 + y1^2)
    COMMeasure_Mod2[i+1] <- 111*0.1*sqrt(x2^2 + y2^2)
    
    COMMeasure[i+1] <- 111*0.1*sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
    # Mean Angle Calculation
    Angle1 <- if( (180/pi)*atan2(y1, x1) < 0 ) {360 + (180/pi)*atan2(y1, x1)} else {(180/pi)*atan2(y1, x1)}
    Angle2 <- if( (180/pi)*atan2(y2, x2) < 0 ) {360 + (180/pi)*atan2(y2, x2)} else {(180/pi)*atan2(y2, x2)}
  
    AngleMeasure_Mod1[i+1] <- Angle1
    AngleMeasure_Mod2[i+1] <- Angle2
    
    AngleMeasure[i+1] <- if(abs(Angle1 - Angle2) > 180) {360 - abs(Angle1 - Angle2)} else {abs(Angle1 - Angle2)}
  
    # Standard Deviation Calculation
    NormalizedAxis_Melted_Origin_Dispersion <- data.frame(
    Melted_Origin_Dispersion$X*sin(-Angle1*pi/180) + Melted_Origin_Dispersion$Y*cos(-Angle1*pi/180),
    Melted_Origin_Dispersion$X*cos(-Angle1*pi/180) - Melted_Origin_Dispersion$Y*sin(-Angle1*pi/180),
    Melted_Origin_Dispersion$CO2
    )
    names(NormalizedAxis_Melted_Origin_Dispersion) <- c("Y", "X", "CO2")
  
    NormalizedAxis_Melted_Radial_Dispersion <- data.frame(
    Melted_Radial_Dispersion$X*sin(-Angle2*pi/180) + Melted_Radial_Dispersion$Y*cos(-Angle2*pi/180),
    Melted_Radial_Dispersion$X*cos(-Angle2*pi/180) - Melted_Radial_Dispersion$Y*sin(-Angle2*pi/180),
    Melted_Radial_Dispersion$CO2
    )
    names(NormalizedAxis_Melted_Radial_Dispersion) <- c("Y", "X", "CO2")
  
    STDAngle1 <- sd((180/pi)*atan2(NormalizedAxis_Melted_Origin_Dispersion$Y, NormalizedAxis_Melted_Origin_Dispersion$X))
    STDAngle2 <- sd((180/pi)*atan2(NormalizedAxis_Melted_Radial_Dispersion$Y, NormalizedAxis_Melted_Radial_Dispersion$X))
  
    STDAngleMeasure_Mod1[i+1] <- STDAngle1
    STDAngleMeasure_Mod2[i+1] <- STDAngle2
    
    STDAngleMeasure[j] <- abs(STDAngle1 - STDAngle2)
    
    j <- j+1
    
    Radial_Dispersion <- ShiftToOrigin("U", AngularStretch(Origin_Dispersion, Steps[i+1]), PlantLAT, PlantLON)
    
    Ex1 <- ggmap(map) +
      geom_point(data = Radial_Dispersion, aes(x = LON, y = LAT, color = CO2), alpha = 0.3) +
      geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
      coord_cartesian() +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      scale_colour_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                             values = c(0, 0.0012, 0.002, 0.004, 0.009, 1))
    
    eval(parse(text = paste("ggsave('AngularExample", i, ".jpg', Ex1, device = 'jpg', width = 10, height = 10, units = 'in')", sep = "")))
  
}

Metrics_Angular <- data.frame(Steps, MRSMeasure, COMMeasure, AngleMeasure, STDAngleMeasure)
names(Metrics_Radial) <- c("Dilation", "MRS_Measure", "COM_Measure", "MeanAngle_Measure", "STDAngle_Measure")