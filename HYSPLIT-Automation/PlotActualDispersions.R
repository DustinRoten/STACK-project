library(ggmap)
library(ggplot2)
library(gridExtra)

f = 5
zoomval <-6 #0623 - 7 ... 0210 - 6

Model1 <- read.delim("TCG_A", header = TRUE, sep = "")
Model2 <- read.delim("TCG_E", header = TRUE, sep = "")

if(min(Model1$DA) != 0 & min(Model2$DA) != 0) {

    Model1$DA <- Model1$DA - 1
    Model2$DA <- Model2$DA - 1

} else {}

Plume1 <- subset(Model1, DA == f)
Plume2 <- subset(Model2, DA == f)
  
PlantLAT <- 39.2865
PlantLON <- -96.1172

map <- get_map(location = c(PlantLON, PlantLAT), zoom = zoomval, maptype = "terrain", color = "bw")

DeltaP <- range(log(Plume1$CO2, 10))[2] - range(log(Plume1$CO2, 10))[1]

############################# Normal View ################################

Plot1 <- ggmap(map) +
  geom_tile(data = Plume1, aes(x = LON, y = LAT, fill = log(CO2, 10))) +
  ggtitle("Scenario 1") +
  coord_cartesian() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = expression("Concentration"[(log)])) +
  scale_fill_gradientn(colors = c("green", "green2", "green4",
                                  "yellow", "gold", "darkorange", 
                                  "darkorange3", "red", "firebrick3",
                                  "red4", "purple", "darkmagenta"),
                       breaks = c(min(log(Plume1$CO2, 10)),
                                  min(log(Plume1$CO2, 10)) + DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 2*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 3*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 4*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 5*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 6*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 7*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 8*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 9*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 10*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 11*DeltaP/12,
                                  max(log(Plume1$CO2), 10)),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
                       limits = c(log(min( min(Plume1$CO2), min(Plume2$CO2) ), 10),
                                  log(max( max(Plume1$CO2), max(Plume2$CO2) ), 10))) +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 35, face = "bold")) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=25,face="bold")) +
  theme(legend.text=element_text(size=20), legend.key.size = unit(2, "cm")) +
  theme(legend.title = element_text(size=20, face = "bold"))
  
 

Plot2 <- ggmap(map) +
  geom_tile(data = Plume2, aes(x = LON, y = LAT, fill = log(CO2, 10))) +
  ggtitle("Scenario 5") +
  coord_cartesian() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = expression("Concentration"[(log)])) +
  scale_fill_gradientn(colors = c("green", "green2", "green4",
                                  "yellow", "gold", "darkorange", 
                                  "darkorange3", "red", "firebrick3",
                                  "red4", "purple", "darkmagenta"),
                       breaks = c(min(log(Plume1$CO2, 10)),
                                  min(log(Plume1$CO2, 10)) + DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 2*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 3*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 4*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 5*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 6*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 7*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 8*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 9*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 10*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 11*DeltaP/12,
                                  max(log(Plume1$CO2), 10)),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
                       limits = c(log(min( min(Plume1$CO2), min(Plume2$CO2) ), 10),
                                  log(max( max(Plume1$CO2), max(Plume2$CO2) ), 10))) +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 35, face = "bold")) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=25,face="bold")) +
  theme(legend.text=element_text(size=20), legend.key.size = unit(2, "cm")) +
  theme(legend.title = element_text(size=20, face = "bold"))

Plot1Max <- Plot1
Plot2Max <- Plot2

################################################# Zoom View ########################
zoomval <- 8
map <- get_map(location = c(PlantLON, PlantLAT), zoom = zoomval, maptype = "terrain", color = "bw")

Plot3 <- ggmap(map) +
  geom_tile(data = Plume1, aes(x = LON, y = LAT, fill = log(CO2, 10))) +
  coord_cartesian() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = expression("Concentration"[(log)])) +
  scale_fill_gradientn(colors = c("green", "green2", "green4",
                                  "yellow", "gold", "darkorange", 
                                  "darkorange3", "red", "firebrick3",
                                  "red4", "purple", "darkmagenta"),
                       breaks = c(min(log(Plume1$CO2, 10)),
                                  min(log(Plume1$CO2, 10)) + DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 2*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 3*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 4*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 5*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 6*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 7*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 8*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 9*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 10*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 11*DeltaP/12,
                                  max(log(Plume1$CO2), 10)),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
                       limits = c(log(min( min(Plume1$CO2), min(Plume2$CO2) ), 10),
                                  log(max( max(Plume1$CO2), max(Plume2$CO2) ), 10))) +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 35, face = "bold")) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=25,face="bold")) +
  theme(legend.text=element_text(size=20), legend.key.size = unit(2, "cm")) +
  theme(legend.title = element_text(size=20, face = "bold"))

Plot4 <- ggmap(map) +
  geom_tile(data = Plume2, aes(x = LON, y = LAT, fill = log(CO2, 10))) +
  coord_cartesian() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = expression("Concentration"[(log)])) +
  scale_fill_gradientn(colors = c("green", "green2", "green4",
                                  "yellow", "gold", "darkorange", 
                                  "darkorange3", "red", "firebrick3",
                                  "red4", "purple", "darkmagenta"),
                       breaks = c(min(log(Plume1$CO2, 10)),
                                  min(log(Plume1$CO2, 10)) + DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 2*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 3*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 4*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 5*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 6*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 7*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 8*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 9*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 10*DeltaP/12,
                                  min(log(Plume1$CO2, 10)) + 11*DeltaP/12,
                                  max(log(Plume1$CO2, 10))),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
                       limits = c(log(min( min(Plume1$CO2), min(Plume2$CO2) ), 10),
                                  log(max( max(Plume1$CO2), max(Plume2$CO2) ), 10))) +
  theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 35, face = "bold")) +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=25,face="bold")) +
  theme(legend.text=element_text(size=20), legend.key.size = unit(2, "cm")) +
  theme(legend.title = element_text(size=20, face = "bold"))

Plot3
Plot4

Plot3Max <- Plot3
Plot4Max <- Plot4

grid.arrange(Plot1Max, Plot2Max, Plot3Max, Plot4Max, ncol = 2)
