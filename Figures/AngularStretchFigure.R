library(geosphere)
load("TEST_Values.RData")

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,5:7]
names(Dispersion) <- c("LAT", "LON", "CO2")
Dispersion$LAT <- Dispersion$LAT - 39.2865
Dispersion$LON <- Dispersion$LON - (-96.1172)

AngDispersion <- data.frame(atan(Dispersion$LAT/Dispersion$LON), Dispersion$CO2)
names(AngDispersion) <- c("Angle", "Conc")

MeanAngle <- mean(AngDispersion$Angle)

Metric <- data.frame()
for(i in 0:25) {

    AngleStretch <- 1 + i/100
    
    temp_angle = NULL
    for(j in 1:nrow(AngDispersion)) {
    
         if(AngDispersion[j,1] > MeanAngle)
            {temp_angle[j] <- AngDispersion[j,1]*AngleStretch}
          else if(AngDispersion[j,1] < MeanAngle)
            {temp_angle[j] <- AngDispersion[j,1]*(-AngleStretch)}
          else if(AngDispersion[j,1] == MeanAngle)
            {temp_angle[j] <- MeanAngle}
          else{print("ERROR")}
        
    }
    
        AngDispersionLAT <- sin(temp_angle) + 39.2865
        AngDispersionLON <- cos(temp_angle) - 96.1172
    
    AngDispersion <- data.frame(AngDispersionLAT, AngDispersionLON, Dispersion$CO2)
    names(AngDispersion) <- c("LAT", "LON", "CO2")
    
    DayModel1 <- AngDispersion
    DayModel2 <- Dispersion
    
    x_range <- max( max(DayModel1$LON), max(DayModel2$LON)) - min(min(DayModel1$LON), min(DayModel2$LON)) + 1
    y_range <- max( max(DayModel1$LAT), max(DayModel2$LAT)) - min(min(DayModel1$LAT), min(DayModel2$LAT)) + 1
    
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
    
    DayModel1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    DayModel2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    
    
    # The metric is calculated here
    minLON <- min(min(DayModel1$LON), min(DayModel2$LON))
    minLAT <- min(min(DayModel1$LAT), min(DayModel2$LAT))
    
    for (g in 1:y_steps) {
      
      for(h in 1:x_steps) {
        
        CellAveragedPollutant_1 <- mean(DayModel1[,3][DayModel1$LON >= minLON + Resolution*(h-1) & DayModel1$LON < minLON + Resolution*h &
                                                        DayModel1$LAT >= minLAT + Resolution*(g-1) & DayModel1$LAT < minLAT + Resolution*g])
        
        CellAveragedPollutant_2 <- mean(DayModel2[,3][DayModel2$LON >= minLON + Resolution*(h-1) & DayModel2$LON < minLON + Resolution*h &
                                                        DayModel2$LAT >= minLAT + Resolution*(g-1) & DayModel2$LAT < minLAT + Resolution*g])
        
        DayModel1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
        DayModel2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
        
      }
    }
    
    # Paige's Metrics Begin Here
    MeanLat <- eval(parse(text = paste("mean(", LocationInformation[1,1], "_", "StackParams", "[,1]", ")", sep = "")))
    MeanLon<- eval(parse(text = paste("mean(", LocationInformation[1,1], "_", "StackParams", "[,2]", ")", sep = "")))
    
    plant <- c(MeanLon, MeanLat) # Location Latitude, Longitude
    plant2 <- plant
    
    # Model - 1  
    lat <- DayModel1[,1]
    long <- DayModel1[,2]
    conc <- DayModel1[,3]
    emit <- cbind(long, lat, conc)
    
    ex.angle <- bearing(plant, emit[,1:2])
    mean.angle1 <- sum(ex.angle*emit[,3])/sum(emit[,3])
    var.angle1 <- sqrt((sum(na.omit(emit[,3])*(ex.angle - mean.angle1)))^2)/sum(na.omit(emit[,3]))
    
    # Model - 2        
    lat2 <- DayModel2[,1]
    long2 <- DayModel2[,2]
    conc2 <- DayModel2[,3]
    emit2 <- cbind(long2, lat2, conc2)
    
    ex.angle2 <- bearing(plant2, emit2[,1:2])
    mean.angle2 <- sum(ex.angle2*emit2[,3])/sum(emit2[,3])
    var.angle2 <- sqrt((sum(na.omit(emit2[,3])*(ex.angle2 - mean.angle2)))^2)/sum(na.omit(emit2[,3]))
    
    ### Difference of E - A
    mean.angleX <- mean.angle2 - mean.angle1     # Metrics[,3]
    var.angleX <- var.angle2 - var.angle1        # Metrics[,4]
    
    ### Center of Mass (Goes in Metrics[,5])
    DayModel1_x <- sum( (DayModel1[,2] - LocationInformation[1,5])*DayModel1[,3] )/sum(DayModel1[,3])
    DayModel1_y <- sum( (DayModel1[,1] - LocationInformation[1,4])*DayModel1[,3] )/sum(DayModel1[,3])
    
    DayModel2_x <- sum( (DayModel2[,2] - LocationInformation[1,5])*DayModel2[,3] )/sum(DayModel2[,3])
    DayModel2_y <- sum( (DayModel2[,1] - LocationInformation[1,4])*DayModel2[,3] )/sum(DayModel2[,3])
    
    # Metric calculation is performed here (as a percentage %)
    Metric[i+1,1] <- "Angular Stretch"
    Metric[i+1,2] <- AngleStretch
    Metric[i+1,3] <- ((100*20000*(Resolution*111000)^2)/(2*(LocationInformation[1,2]/(366))))*sum(abs(DayModel2_Matrix - DayModel1_Matrix))
    Metric[i+1,4] <- mean.angleX
    Metric[i+1,5] <- var.angleX
    Metric[i+1,6] <- sqrt( (DayModel2_x - DayModel1_x)^2 + (DayModel2_y - DayModel1_y)^2)
}


# PLOT FIGURE HERE

# Testing

TEST = NULL
for(i in 1:100) {
  
    testing <- 1 + i/100
    
    TEST[i] <- sin(testing)
  
}

library(ggplot2)
ggplot(data = Metric, aes(x = V2, y = V3)) +
  geom_line() +
  theme_bw()
