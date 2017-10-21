library(ggplot2)
library(reshape)
source("TEST-DEMOFunctions.R")

PlantLAT <- 39.28682
PlantLON <- -96.1172
Emissions <- 12591532084.8523
Resolution <- 0.1

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")
Origin_Dispersion <- ShiftToOrigin("S", Dispersion, PlantLAT, PlantLON)

Scenarios <- c("ShiftDispersion", "RotateDispersion", "RadialDilation", "AngularStretch")
Names_Scenarios <- c("ShiftedDispersion", "RotatedDispersion", "RadialStretchDispersion", "AngularStretchDispersion")
NumOfRuns <- c(50, 200, 100, 100)

Metrics <- c("MRSMeasure", "MeanAngleMeasure", "STDAngleMeasure", "COMMeasure")

for (i in 1:4) {
  
    for (j in 1:4) {
  
        for (k in 1:NumOfRuns[i]) { # Begin individual iterations here
          
            eval(parse(text = Names_Scenarios[i], " <- ", Scenarios[i], "(Origin_Dispersion,", k, ")"))
            
            Matrix_Model1 <- GridDispersions(Origin_Dispersion, cat(Names_Scenarios[i]), 1)
            Matrix_Model2 <- GridDispersions(Origin_Dispersion, cat(Names_Scenarios[i]), 2)
          
        } # End individual iterations here
    }
}
