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
