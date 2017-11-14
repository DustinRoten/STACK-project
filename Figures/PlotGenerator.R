source("TEST-DEMOFunctions.R")
source("RotateDispersion.R")
source("ShiftDispersion.R")
source("RadialDispersion.R")
source("AngularDispersion.R")

Resolution <- 0.01

#####################################
##### Plots are Generated Below #####
#####################################

### Plot 1: Shift Metric, MRS ###
p <- ggplot(data = Metrics_Shift, aes(x = ShiftDegree, y = MRS_Measure)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$MRS_Measure), max(1.01*Metrics_Rotate$MRS_Measure), max(1.01*Metrics_Angular$MRS_Measure), max(1.01*Metrics_Radial$MRS_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-MRS-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Shift Metric, MeanAngle ###
p <- ggplot(data = Metrics_Shift, aes(x = ShiftDegree, y = MeanAngle_Measure)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$MeanAngle_Measure), max(1.01*Metrics_Rotate$MeanAngle_Measure), max(1.01*Metrics_Angular$MeanAngle_Measure), max(1.01*Metrics_Radial$MeanAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-MeanAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Shift Metric, STDAngle ###
p <- ggplot(data = Metrics_Shift, aes(x = ShiftDegree, y = STDAngle_Measure)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$STDAngle_Measure), max(1.01*Metrics_Rotate$STDAngle_Measure), max(1.01*Metrics_Angular$STDAngle_Measure), max(1.01*Metrics_Radial$STDAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-STDAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Shift Metric, COM ###
p <- ggplot(data = Metrics_Shift, aes(x = ShiftDegree, y = COM_Measure)) +
  geom_line() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$COM_Measure), max(1.01*Metrics_Rotate$COM_Measure), max(1.01*Metrics_Angular$COM_Measure), max(1.01*Metrics_Radial$COM_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Shift-COM-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

######################################################################################

### Plot 1: Rotate Metric, MRS ###
p <- ggplot(data = Metrics_Rotate, aes(x = ShiftDegree, y = MRS_Measure)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$MRS_Measure), max(1.01*Metrics_Rotate$MRS_Measure), max(1.01*Metrics_Radial$MRS_Measure), max(1.01*Metrics_Angular$MRS_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotate-MRS-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Rotate Metric, MeanAngle ###
p <- ggplot(data = Metrics_Rotate, aes(x = ShiftDegree, y = MeanAngle_Measure)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$MeanAngle_Measure), max(1.01*Metrics_Rotate$MeanAngle_Measure), max(1.01*Metrics_Angular$MeanAngle_Measure), max(1.01*Metrics_Radial$MeanAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotate-MeanAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Rotate Metric, STDAngle ###
p <- ggplot(data = Metrics_Rotate, aes(x = ShiftDegree, y = STDAngle_Measure)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$STDAngle_Measure), max(1.01*Metrics_Rotate$STDAngle_Measure), max(1.01*Metrics_Angular$STDAngle_Measure), max(1.01*Metrics_Radial$STDAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotate-STDAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Rotate Metric, COM ###
p <- ggplot(data = Metrics_Rotate, aes(x = ShiftDegree, y = COM_Measure)) +
  geom_line() +
  xlab("Rotation (Degrees)") +
  ylab("") +
  ylim(0, max(max(1.01*Metrics_Shift$COM_Measure), max(1.01*Metrics_Rotate$COM_Measure), max(1.01*Metrics_Angular$COM_Measure), max(1.01*Metrics_Radial$COM_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Rotate-COM-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

#########################################################################################

### Plot 1: Radial Metric, MRS ###
p <- ggplot(data = Metrics_Radial, aes(x = ShiftDegree, y = MRS_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$MRS_Measure), max(1.01*Metrics_Rotate$MRS_Measure), max(1.01*Metrics_Angular$MRS_Measure), max(1.01*Metrics_Radial$MRS_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Radial-MRS-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Radial Metric, MeanAngle ###
p <- ggplot(data = Metrics_Radial, aes(x = ShiftDegree, y = MeanAngle_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$MeanAngle_Measure), max(1.01*Metrics_Rotate$MeanAngle_Measure), max(1.01*Metrics_Angular$MeanAngle_Measure), max(1.01*Metrics_Radial$MeanAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Radial-MeanAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Radial Metric, STDAngle ###
p <- ggplot(data = Metrics_Radial, aes(x = ShiftDegree, y = STDAngle_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$STDAngle_Measure), max(1.01*Metrics_Rotate$STDAngle_Measure), max(1.01*Metrics_Angular$STDAngle_Measure), max(1.01*Metrics_Radial$STDAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Radial-STDAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Radial Metric, COM ###
p <- ggplot(data = Metrics_Radial, aes(x = ShiftDegree, y = COM_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$COM_Measure), max(1.01*Metrics_Rotate$COM_Measure), max(1.01*Metrics_Angular$COM_Measure), max(1.01*Metrics_Radial$COM_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Radial-COM-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")

################################################################################################

### Plot 1: Angular Metric, MRS ###
p <- ggplot(data = Metrics_Angular, aes(x = ShiftDegree, y = MRS_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$MRS_Measure), max(1.01*Metrics_Rotate$MRS_Measure), max(1.01*Metrics_Angular$MRS_Measure), max(1.01*Metrics_Radial$MRS_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Angular-MRS-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 2: Angular Metric, MeanAngle ###
p <- ggplot(data = Metrics_Angular, aes(x = ShiftDegree, y = MeanAngle_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$MeanAngle_Measure), max(1.01*Metrics_Rotate$MeanAngle_Measure), max(1.01*Metrics_Angular$MeanAngle_Measure), max(1.01*Metrics_Radial$MeanAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Angular-MeanAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 3: Angular Metric, STDAngle ###
p <- ggplot(data = Metrics_Angular, aes(x = ShiftDegree, y = STDAngle_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$STDAngle_Measure), max(1.01*Metrics_Rotate$STDAngle_Measure), max(1.01*Metrics_Angular$STDAngle_Measure), max(1.01*Metrics_Radial$STDAngle_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Angular-STDAngle-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")


### Plot 4: Angular Metric, COM ###
p <- ggplot(data = Metrics_Angular, aes(x = ShiftDegree, y = COM_Measure)) +
  geom_line() +
  xlab("Dilation Factor") +
  ylab("") +
  xlim(1,2) +
  ylim(0, max(max(1.01*Metrics_Shift$COM_Measure), max(1.01*Metrics_Rotate$COM_Measure), max(1.01*Metrics_Angular$COM_Measure), max(1.01*Metrics_Radial$COM_Measure) )) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 40, face = "bold")) +
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
  theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

ggsave("Angular-COM-Calibration01.jpg", p, device = "jpg", width = 10, height = 8, units = "in")



##### Moving files #####
NAME <- c("Shift-COM-Calibration01.jpg",
          "Shift-MRS-Calibration01.jpg",
          "Shift-MeanAngle-Calibration01.jpg",
          "Shift-STDAngle-Calibration01.jpg",
          "Radial-COM-Calibration01.jpg",
          "Radial-MRS-Calibration01.jpg",
          "Radial-MeanAngle-Calibration01.jpg",
          "Radial-STDAngle-Calibration01.jpg",
          "Rotate-COM-Calibration01.jpg",
          "Rotate-MRS-Calibration01.jpg",
          "Rotate-MeanAngle-Calibration01.jpg",
          "Rotate-STDAngle-Calibration01.jpg",
          "Angular-COM-Calibration01.jpg",
          "Angular-MRS-Calibration01.jpg",
          "Angular-MeanAngle-Calibration01.jpg",
          "Angular-STDAngle-Calibration01.jpg")

TEST = NULL
for(i in 1:length(NAME)) {
  
  TEST[i] <- file.exists(paste(NAME[i]))
  file.copy(from = paste(NAME[i]), to = paste("~/Google Drive/NASA/HysplitPaper1/images/",NAME[i], sep = ""), overwrite = TRUE)
  file.remove(paste(NAME[i]))
  TEST
}