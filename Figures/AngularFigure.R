library(ggplot2)
library(scales)
library(ggmap)
library(reshape2)
library(geosphere)
load("TEST_Values.RData")

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
Dispersion$LAT <- Dispersion$LAT - LocationInformation[1,4]
Dispersion$LON <- Dispersion$LON - LocationInformation[1,5]

Metric <- data.frame()

for (i in 0:200) {
  
    theta <- i/100
  
    RotDispersion <- as.data.frame(cbind(Dispersion[,1:4],
                                          Dispersion$LON*sinpi(theta) + Dispersion$LAT*cospi(theta),
                                          Dispersion$LON*cospi(theta) - Dispersion$LAT*sinpi(theta),
                                          Dispersion[,7]))
  
    names(RotDispersion) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    # Metric
    DayModel1 <- RotDispersion
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
        
        CellAveragedPollutant_1 <- mean(DayModel1[,5][DayModel1$LON >= minLON + Resolution*(h-1) & DayModel1$LON < minLON + Resolution*h &
                                                        DayModel1$LAT >= minLAT + Resolution*(g-1) & DayModel1$LAT < minLAT + Resolution*g])
        
        CellAveragedPollutant_2 <- mean(DayModel2[,5][DayModel2$LON >= minLON + Resolution*(h-1) & DayModel2$LON < minLON + Resolution*h &
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
    lat <- DayModel1[,5]
    long <- DayModel1[,6]
    conc <- DayModel1[,7]
    emit <- cbind(long, lat, conc)
    
    ex.angle <- bearing(plant, emit[,1:2])
    mean.angle1 <- sum(ex.angle*emit[,3])/sum(emit[,3])
    var.angle1 <- sqrt((sum(na.omit(emit[,3])*(ex.angle - mean.angle1)))^2)/sum(na.omit(emit[,3]))
    
    # Model - 2        
    lat2 <- DayModel2[,5]
    long2 <- DayModel2[,6]
    conc2 <- DayModel2[,7]
    emit2 <- cbind(long2, lat2, conc2)
    
    ex.angle2 <- bearing(plant2, emit2[,1:2])
    mean.angle2 <- sum(ex.angle2*emit2[,3])/sum(emit2[,3])
    var.angle2 <- sqrt((sum(emit[,3])*(sum(ex.angle2 - mean.angle2)^2))/sum(emit2[,3]))
    
    ### Difference of E - A
    mean.angleX <- mean.angle2 - mean.angle1     # Metrics[,3]
    var.angleX <- var.angle2 - var.angle1        # Metrics[,4]
    
    ### Center of Mass (Goes in Metrics[,5])
    DayModel1_x <- sum( (DayModel1[,4] - LocationInformation[1,5])*DayModel1[,5] )/sum(DayModel1[,5])
    DayModel1_y <- sum( (DayModel1[,3] - LocationInformation[1,4])*DayModel1[,5] )/sum(DayModel1[,5])
    
    DayModel2_x <- sum( (DayModel2[,4] - LocationInformation[1,5])*DayModel2[,5] )/sum(DayModel2[,5])
    DayModel2_y <- sum( (DayModel2[,3] - LocationInformation[1,4])*DayModel2[,5] )/sum(DayModel2[,5])
    
    # Metric calculation is performed here (as a percentage %)
    Metric[i+1,1] <- "Angular"
    Metric[i+1,2] <- theta
    Metric[i+1,3] <- ((100*20000*(Resolution*111000)^2)/(2*(LocationInformation[1,2]/(366))))*sum(abs(DayModel2_Matrix - DayModel1_Matrix))
    Metric[i+1,4] <- mean.angleX
    Metric[i+1,5] <- var.angleX
    Metric[i+1,6] <- sqrt( (DayModel2_x - DayModel1_x)^2 + (DayModel2_y - DayModel1_y)^2)
}

names(Metric) <- c("Day", "theta", "MRS", "MeanAngle", "StdAngle", "COM")
write.csv(Metric, "AngularMetrics")



# Plot the figure
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
Dispersion$LAT <- Dispersion$LAT - LocationInformation[1,4]
Dispersion$LON <- Dispersion$LON - LocationInformation[1,5]

theta <- 0.25

RotDispersion <- as.data.frame(cbind(Dispersion[,1:4],
                              round(Dispersion$LON*sinpi(theta) + Dispersion$LAT*cospi(theta), 4),
                              round(Dispersion$LON*cospi(theta) - Dispersion$LAT*sinpi(theta), 4),
                              Dispersion[,7]))

names(RotDispersion) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")

Dispersion$LAT <- Dispersion$LAT + LocationInformation[1,4]
Dispersion$LON <- Dispersion$LON + LocationInformation[1,5]

RotDispersion$LAT <- RotDispersion$LAT + LocationInformation[1,4]
RotDispersion$LON <- RotDispersion$LON + LocationInformation[1,5]

write.csv(RotDispersion, "RotatedDispersion")

RotatedDispersion <- read.csv("RotatedDispersion", header = TRUE)

#Rotated Dispersion
Quantiles <- quantile(RotatedDispersion$CO2, c(0.1, 0.955))
qn01 <- rescale(c(Quantiles, range(RotatedDispersion$CO2)))

map <- get_map(location = c(lon = -95, lat = 43), zoom = 6, maptype = "terrain", colo = "bw")

ggmap(map) +
  geom_raster(data = Dispersion, aes(x = LON, y = LAT, fill = CO2), interpolate = TRUE) +
  geom_raster(data = RotatedDispersion, aes(x = LON, y = LAT, fill = CO2), interpolate = TRUE) +
  scale_fill_gradientn(colours = colorRampPalette(c("limegreen", "yellow", "orange", "red4"))(50),
                       values = c(0, seq(qn01[1], qn01[2], length.out = 2000), 1), 
                       limits = c(min(RotatedDispersion$CO2), max(RotatedDispersion$CO2)),
                       name = "Concentration (kg/cbm)",
                       guide = FALSE) +
  coord_cartesian() +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))
