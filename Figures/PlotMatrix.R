# Dustin Roten - Generates Metric Sensitivity Plots (2017)

# Load required libraries
library(ggplot2)
library(reshape2)
library(scales)
library(ggmap)
library(geosphere)
source("Functions.R")

# Read in file
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
PlantLAT <- 39.2865
PlantLON <- -96.1172
Emissions <- 12591532084.8523

##### Horizontal Shift #####
ShiftMetricValues <- data.frame()
for(i in 0:50) {
    ShiftedDispersion <- ShiftDispersion(Dispersion, i)
    ShiftMetricValues[i+1, 1] <- MRSMeasure(Dispersion, ShiftedDispersion, Emissions, 0.1)
    ShiftMetricValues[i+1, 2] <- MeanAngleMeasure(Dispersion, ShiftedDispersion, PlantLAT, PlantLON)
    ShiftMetricValues[i+1, 3] <- STDAngleMeasure(Dispersion, ShiftedDispersion, PlantLAT, PlantLON)
    ShiftMetricValues[i+1, 4] <- COMMeasure(Dispersion, ShiftedDispersion, PlantLAT, PlantLON)
}

ShiftMetricSteps <- c(1:nrow(ShiftMetricValues))/10
names(ShiftMetricValues) <- c("MRS", "MeanAngle", "STDAngle", "COM")
MetricList <- c("MRS", "MeanAngle", "STDAngle", "COM")

### Plot 1: Shift Metric, MRS ###
p <- ggplot(data = abs(ShiftMetricValues), aes(x = ShiftMetricSteps, y = MRS)) +
       geom_line() +
       xlab("Horizontal Shift (Degrees)") +
       ylab(expression(Phi*" (%)")) +
       theme_bw() +
       theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
       theme(plot.title = element_text(size = 30, face = "bold")) +
       theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold")) +
       theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))) +
       theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
  
ggsave("Shift-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Shift Metric, MeanAngle ###
p <- ggplot(data = abs(ShiftMetricValues), aes(x = ShiftMetricSteps, y = MeanAngle)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab(expression(Delta*bar(theta)*" (Degrees)")) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))

ggsave("Shift-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Shift Metric, STDAngle ###
p <- ggplot(data = abs(ShiftMetricValues), aes(x = ShiftMetricSteps, y = STDAngle)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab(expression(Delta*sigma[bar(theta)]*" (Degrees)")) +
  ylim(c(0,1)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))

ggsave("Shift-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: ShiftMetric, COM ###
p <- ggplot(data = abs(ShiftMetricValues), aes(x = ShiftMetricSteps, y = COM)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab(expression(Delta*"|r|"*" (km)")) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))

ggsave("Shift-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")
