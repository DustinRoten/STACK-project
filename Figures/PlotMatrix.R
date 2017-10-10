# Dustin Roten - Generates Metric Sensitivity Plots (2017)

# Load required libraries
library(ggplot2)
library(reshape2)
library(scales)
library(ggmap)
library(geosphere)
library(mailR)
source("DEMOFunctions.R")
source("EmailFunction.R")
source("TEST-DemoFunctions.R")

# Read in file
Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
PlantLAT <- 39.28682
PlantLON <- -96.1172
Emissions <- 12591532084.8523

DispersionAtOrigin <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)

BossStatus("3366202544@cwwsms.com", "Sensitivity Graphs", "Your sensitivity plotting script is 0% complete... Beginning horizontal shift now.")

##### Horizontal Shift #####
ShiftMetricValues <- data.frame()

for(i in 0:50) {
    ShiftedDispersion <- ShiftDispersion(DispersionAtOrigin, i)

    ShiftMetricValues[i+1, 1] <- i/10
    ShiftMetricValues[i+1, 2] <- TESTMRSMeasure(ShiftedDispersion, DispersionAtOrigin, Emissions, 0.1)
    ShiftMetricValues[i+1, 3] <- MeanAngleMeasure(ShiftedDispersion, DispersionAtOrigin, 0, 0)
    ShiftMetricValues[i+1, 4] <- STDAngleMeasure(ShiftedDispersion, DispersionAtOrigin, 0, 0)
    ShiftMetricValues[i+1, 5] <- COMMeasure("F", ShiftedDispersion, DispersionAtOrigin, 0, 0)
}

names(ShiftMetricValues) <- c("Factor", "MRS", "MeanAngle", "STDAngle", "COM")


### Plot 1: Shift Metric, MRS ###
p <- ggplot(data = ShiftMetricValues, aes(x = Factor, y = MRS)) +
       geom_line() +
       xlab("Horizontal Shift (Degrees)") +
       ylab("") +
       theme_bw() +
       theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
       theme(plot.title = element_text(size = 40, face = "bold")) +
       theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
       theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
       theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))
  
ggsave("Shift-MRS-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Shift Metric, MeanAngle ###
p <- ggplot(data = ShiftMetricValues, aes(x = Factor, y = MeanAngle)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Shift Metric, STDAngle ###
p <- ggplot(data = ShiftMetricValues, aes(x = Factor, y = STDAngle)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(c(-1,1)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Shift Metric, COM ###
p <- ggplot(data = ShiftMetricValues, aes(x = Factor, y = COM)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-COM-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

###################################################################################################################

BossStatus("3366202544@cwwsms.com", "Sensitivity Graphs", "Your sensitivity plotting script is 25% complete... Beginning angular rotation now.")

### Angular Rotation ###
RotationMetricValues <- data.frame()

for(i in 0:200) {
  
    RotatedDispersion <- RotateDispersion(DispersionAtOrigin, i)
    RotationMetricValues[i+1, 1] <- i/100
    RotationMetricValues[i+1, 2] <- TESTMRSMeasure(RotatedDispersion, DispersionAtOrigin, Emissions, 0.1)
    RotationMetricValues[i+1, 3] <- MeanAngleMeasure(RotatedDispersion, DispersionAtOrigin, 0, 0)
    RotationMetricValues[i+1, 4] <- STDAngleMeasure(RotatedDispersion, DispersionAtOrigin, 0, 0)
    RotationMetricValues[i+1, 5] <- COMMeasure("T", RotatedDispersion, DispersionAtOrigin, 0, 0)
  
}

names(RotationMetricValues) <- c("Factor", "MRS", "MeanAngle", "STDAngle", "COM")

### Plot 1: Rotation Metric, MRS ###
p <- ggplot(data = RotationMetricValues, aes(x = Factor, y = MRS)) +
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
p <- ggplot(data = RotationMetricValues, aes(x = Factor, y = MeanAngle)) +
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
p <- ggplot(data = RotationMetricValues, aes(x = Factor, y = STDAngle)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  ylim(-1,1) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotation-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Rotation Metric, COM ###
p <- ggplot(data = RotationMetricValues, aes(x = Factor, y = COM)) +
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

BossStatus("3366202544@cwwsms.com", "Sensitivity Graphs", "Your sensitivity plotting script is 50% complete... Beginning radial dilation now.")

###################################################################################################################

##### Dilation (Radial?) #####
RadialStretchMetricValues <- data.frame()
for(i in 0:200) {
  
    RadialStretchDispersion <- TESTRadialDilation(DispersionAtOrigin, i)
    RadialStretchMetricValues[i+1, 1] <- 1 + i/100
    RadialStretchMetricValues[i+1, 2] <- TESTMRSMeasure(RadialStretchDispersion, DispersionAtOrigin, Emissions, 0.1)
    RadialStretchMetricValues[i+1, 3] <- MeanAngleMeasure(RadialStretchDispersion, DispersionAtOrigin, 0, 0)
    RadialStretchMetricValues[i+1, 4] <- STDAngleMeasure(RadialStretchDispersion, DispersionAtOrigin, 0, 0)
    RadialStretchMetricValues[i+1, 5] <- COMMeasure("T", RadialStretchDispersion, DispersionAtOrigin, 0, 0)
  
}

names(RadialStretchMetricValues) <- c("Factor", "MRS", "MeanAngle", "STDAngle", "COM")

### Plot 1: Radial Stretch Metric, MRS ###
p <- ggplot(data = RadialStretchMetricValues, aes(x = Factor, y = MRS)) +
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
p <- ggplot(data = RadialStretchMetricValues, aes(x = Factor, y = MeanAngle)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  ylim(c(-1,1)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-MeanAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Radial Stretch Metric, STDAngle ###
p <- ggplot(data = RadialStretchMetricValues, aes(x = Factor, y = STDAngle)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  ylim(c(-1,1)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("RadStretch-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Radial Stretch Metric, COM ###
p <- ggplot(data = RadialStretchMetricValues, aes(x = Factor, y = COM)) +
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

BossStatus("3366202544@cwwsms.com", "Sensitivity Graphs", "Your sensitivity plotting script is 75% complete... Beginning angular stretch now.")

###############################################################################################################

### Angular Stretch ###

AngularStretchMetricValues <- data.frame()

for (i in 0:100) {

    AngularStretchDispersion <- TESTAngularStretch(DispersionAtOrigin, i)
    AngularStretchMetricValues[i+1, 1] <- i/100
    AngularStretchMetricValues[i+1, 2] <- TESTMRSMeasure(AngularStretchDispersion, DispersionAtOrigin, Emissions, 0.1)
    AngularStretchMetricValues[i+1, 3] <- MeanAngleMeasure(AngularStretchDispersion, DispersionAtOrigin, 0, 0)
    AngularStretchMetricValues[i+1, 4] <- STDAngleMeasure(AngularStretchDispersion, DispersionAtOrigin, 0, 0)
    AngularStretchMetricValues[i+1, 5] <- COMMeasure("T", AngularStretchDispersion, DispersionAtOrigin, 0, 0)
        
}

names(AngularStretchMetricValues) <- c("Factor", "MRS", "MeanAngle", "STDAngle", "COM")



### Plot 1: Angular Stretch Metric, MRS ###
p <- ggplot(data = AngularStretchMetricValues, aes(x = Factor, y = MRS)) +
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
p <- ggplot(data = AngularStretchMetricValues, aes(x = Factor, y = MeanAngle)) +
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
p <- ggplot(data = AngularStretchMetricValues, aes(x = Factor, y = STDAngle)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  ylim(c(-1,1)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("AngStretch-STDAngle-Calibration.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Angular Stretch Metric, COM ###
p <- ggplot(data = AngularStretchMetricValues, aes(x = Factor, y = COM)) +
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

BossStatus("3366202544@cwwsms.com", "Sensitivity Graphs", "Your sensitivity plotting script is 100% complete... Beginning file transfer now.")

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
