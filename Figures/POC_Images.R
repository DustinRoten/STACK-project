# Dustin Roten 11/05/2017
# This script is responsible for metrics for the shifted dispersion scenario ONLY.

SetAlpha <- 0.3

# Load required libraries
library(ggplot2)
library(reshape)
library(ggmap)
source("TEST-DEMOFunctions.R")

# The required information for the functions used includes the (Lat, Lon) of the plant
# as well as the desired gridding resolution.
PlantLAT <- 39.28682
PlantLON <- -96.1172
Resolution <- 0.1

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")
Origin_Dispersion <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)

Shifted_Dispersion <- ShiftToOrigin("U", ShiftDispersion(Origin_Dispersion, 10), PlantLAT, PlantLON)
Rotated_Dispersion <- ShiftToOrigin("U", RotateDispersion(Origin_Dispersion, 15), PlantLAT, PlantLON)
Radial_Dispersion <- ShiftToOrigin("U", RadialDilation(Origin_Dispersion, 24), PlantLAT, PlantLON)
Angular_Dispersion <- ShiftToOrigin("U", AngularStretch(Origin_Dispersion, 55), PlantLAT, PlantLON)

map <- get_map(location = c(PlantLON + 2, PlantLAT + 4), zoom =6, maptype = "terrain", color = "bw")

Ex1 <- ggmap(map) +
          geom_point(data = Shifted_Dispersion, aes(x = LON, y = LAT, color = CO2), alpha = SetAlpha) +
          geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
          coord_cartesian() +
          theme_bw() +
          theme(legend.position="none") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
          scale_colour_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                                 values = c(0, 0.0012, 0.002, 0.004, 0.009, 1))

ggsave("ShiftExample.jpg", Ex1, device = "jpg", width = 10, height = 10, units = "in")


Ex2 <- ggmap(map) +
          geom_point(data = Angular_Dispersion, aes(x = LON, y = LAT, color = CO2), alpha = SetAlpha) +
          geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
          coord_cartesian() +
          theme_bw() +
          theme(legend.position="none") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
          scale_colour_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                                 values = c(0, 0.0012, 0.002, 0.004, 0.009, 1))

ggsave("AngularExample.jpg", Ex2, device = "jpg", width = 10, height = 10, units = "in")


Ex3 <- ggmap(map) +
          geom_point(data = Radial_Dispersion, aes(x = LON, y = LAT, color = CO2), alpha = SetAlpha) +
          geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
          coord_cartesian() +
          theme_bw() +
          theme(legend.position="none") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
          scale_colour_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                                 values = c(0, 0.0012, 0.002, 0.004, 0.009, 1))

  
  ggsave("RadialExample.jpg", Ex3, device = "jpg", width = 10, height = 10, units = "in")


Ex4 <- ggmap(map) +
          geom_point(data = Rotated_Dispersion, aes(x = LON, y = LAT, color = CO2), alpha = SetAlpha) +
          geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
          coord_cartesian() +
          theme_bw() +
          theme(legend.position="none") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
          scale_colour_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                                 values = c(0, 0.0012, 0.002, 0.004, 0.009, 1))

ggsave("RotatedExample.jpg", Ex4, device = "jpg", width = 10, height = 10, units = "in")


##### Moving files #####
NAME <- c("ShiftExample.jpg", "AngularExample.jpg", "RadialExample.jpg", "RotatedExample.jpg")

TEST = NULL
for(i in 1:length(NAME)) {
  
  TEST[i] <- file.exists(paste(NAME[i]))
  file.copy(from = paste(NAME[i]), to = paste("C:/Users/dusti/Google Drive/NASA/HysplitPaper1/images/", NAME[i], sep = ""), overwrite = TRUE)
  file.remove(paste(NAME[i]))
  TEST
}
