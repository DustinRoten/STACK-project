# Dustin Roten 11/05/2017
# This script is responsible for metrics for the shifted dispersion scenario ONLY.

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
Angular_Dispersion <- ShiftToOrigin("U", AngularStretch(Origin_Dispersion, 40), PlantLAT, PlantLON)

map <- get_map(location = c(PlantLON + 2, PlantLAT + 4), zoom =6, maptype = "terrain", color = "bw")

ggmap(map) +
  geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
  geom_point(data = Shifted_Dispersion, aes(x = LON, y = LAT, color = CO2, alpha = 0.2)) +
  coord_cartesian() +
  theme_bw()

ggmap(map) +
  geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
  geom_point(data = Angular_Dispersion, aes(x = LON, y = LAT, color = CO2, alpha = 0.2)) +
  coord_cartesian() +
  theme_bw()

ggmap(map) +
  geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
  geom_point(data = Radial_Dispersion, aes(x = LON, y = LAT, color = CO2, alpha = 0.2)) +
  coord_cartesian() +
  theme_bw()

ggmap(map) +
  geom_point(data = Dispersion, aes(x = LON, y = LAT, color = CO2)) +
  geom_point(data = Rotated_Dispersion, aes(x = LON, y = LAT, color = CO2, alpha = 0.2)) +
  coord_cartesian() +
  theme_bw()
