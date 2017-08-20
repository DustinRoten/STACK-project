library(ggplot2)
library(scales)
library(ggmap)
library(reshape2)
load("TEST_Values.RData")

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")[,1:7]
Dispersion$LAT <- Dispersion$LAT - LocationInformation[1,4]
Dispersion$LON <- Dispersion$LON - LocationInformation[1,5]

Metric <- data.frame()

for (i in 0:400) {
  
    stretch <- 1 + i/100
  
    StrDispersion <- as.data.frame(cbind(Dispersion[,1:4],
                                          stretch*Dispersion$LAT,
                                          stretch*Dispersion$LON,
                                          Dispersion[,7]))
  
    names(StrDispersion) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    # Metric
  
    DayModel1 <- StrDispersion
    DayModel2 <- Dispersion
  
    x_range <- max( max(DayModel1$LON), max(DayModel2$LON) ) - min( min(DayModel1$LON), min(DayModel2$LON) ) + 1
    y_range <- max( max(DayModel1$LAT), max(DayModel2$LAT) ) - min( min(DayModel1$LAT), min(DayModel2$LAT) ) + 1
  
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
  
    DayModel1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    DayModel2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
    # This section of code executes the MRS measure
    minLON <- min(min(DayModel1$LON), min(DayModel2$LON))
    minLAT <- min(min(DayModel1$LAT), min(DayModel2$LAT))
  
    for (g in 1:y_steps) {
    
        for(h in 1:x_steps) {
      
            CellAveragedPollutant_1 <- mean(DayModel1[,7][DayModel1$LON >= minLON + Resolution*(h-1) & DayModel1$LON < minLON + Resolution*h &
                                                      DayModel1$LAT >= minLAT + Resolution*(g-1) & DayModel1$LAT < minLAT + Resolution*g])
      
            CellAveragedPollutant_2 <- mean(DayModel2[,7][DayModel2$LON >= minLON + Resolution*(h-1) & DayModel2$LON < minLON + Resolution*h &
                                                      DayModel2$LAT >= minLAT + Resolution*(g-1) & DayModel2$LAT < minLAT + Resolution*g])
      
            DayModel1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
            DayModel2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
      
        }
    
    }
  
      longData<-melt(DayModel1_Matrix)
      longData<-longData[longData$value!=0,]
  
      longData2<-melt(DayModel2_Matrix)
      longData2<-longData2[longData2$value!=0,]
  
      longData3<-melt(sign(DayModel2_Matrix - DayModel1_Matrix)*abs(DayModel2_Matrix - DayModel1_Matrix))
      longData3<-longData3[longData3$value!=0,]
  
      SaveFile <- ggplot(longData3, aes(x = Var2, y = Var1)) + 
        geom_raster(aes(fill=value)) + 
        scale_fill_gradient(low="grey90", high="red") +
        labs(x="letters", y="LETTERS", title="Matrix") +
        theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                          axis.text.y=element_text(size=9),
                          plot.title=element_text(size=11))
  
  # Metric calculation is performed here (as a percentage %)
  Metric[i+1,1] <- stretch
  Metric[i+1,2] <- ((100*20000*(Resolution*111000)^2)/(2*(12591532084.8523/366)))*sum(abs(DayModel2_Matrix - DayModel1_Matrix))
  
}



## Plot the Metric Values
#vec.breaks <- seq(from = pi/2, to = 2*pi, by = pi/2)
#pi.halfs <- c(paste(expression(pi), "/2"),paste(seq(from = 3, to = 21, by = 2), "*" , expression(pi), "/2"))
#pi.fulls <- c(paste(expression(pi)), paste(seq(from = 2, to = 11, by = 1), "*" , expression(pi)))
#vec.expr <- parse(text = c(rbind(pi.halfs, pi.fulls)))[1:7]

ggplot(data = as.data.frame(Metric), aes(x = Metric[1], y = Metric[2])) +
  geom_line() +
  ggtitle("Metric Sensitivity to Radial Displacement") +
  xlab("Dilation Factor") +
  ylab("Metric Value (%)") +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))



# Plot the figure
stretch <- 1 + 25/100

StrDispersion <- as.data.frame(cbind(Dispersion[,1:4],
                                     stretch*Dispersion$LAT,
                                     stretch*Dispersion$LON,
                                     Dispersion[,7]))

names(StrDispersion) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")

Dispersion$LAT <- Dispersion$LAT + LocationInformation[1,4]
Dispersion$LON <- Dispersion$LON + LocationInformation[1,5]

StrDispersion$LAT <- StrDispersion$LAT + LocationInformation[1,4]
StrDispersion$LON <- StrDispersion$LON + LocationInformation[1,5]

write.csv(StrDispersion, "StrDispersion")

StrDispersion <- read.csv("StrDispersion", header = TRUE)

#Stretched Dispersion
Quantiles <- quantile(StrDispersion$CO2, c(0.1, 0.955))
qn01 <- rescale(c(Quantiles, range(StrDispersion$CO2)))

map <- get_map(location = c(lon = -94, lat = 44), zoom = 6, maptype = "terrain", colo = "bw")

ggmap(map) +
  geom_raster(data = Dispersion, aes(x = LON, y = LAT, fill = CO2), interpolate = TRUE) +
  geom_raster(data = StrDispersion, aes(x = LON, y = LAT, fill = CO2), interpolate = TRUE, alpha = 0.45) +
  scale_fill_gradientn(colours = colorRampPalette(c("limegreen", "yellow", "orange", "red4"))(50),
                       values = c(0, seq(qn01[1], qn01[2], length.out = 2000), 1), 
                       limits = c(min(StrDispersion$CO2), max(StrDispersion$CO2)),
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

