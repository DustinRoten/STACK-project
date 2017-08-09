#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618))

eGRIDLoc <- c(mean(StackLoc[,1]), mean(StackLoc[,2]))

Model1 <- "JEC-A-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "JEC-E-2012"    #Name of dataset for Model 2; MUST BE CSV

#####################################
##### Initialize All Libraries ######
#####################################
library(ggplot2)
library(dplyr)
library(geosphere)
library(foreach)
library(doParallel)
library(ggmap)
library(akima)
#####################################

#####################################
##### Initialize Parallel Cores #####
#####################################

if (IfPar == 1)
{cl <- detectCores()-FreeCores
registerDoParallel(cl)
} else {print("Serial Computation")}

#####################################

#############################################
##### Analysis of Maximum Concentration #####
#############################################

Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(Model2)[2:6]

Comp1PercentDifference = NULL
Comp2PercentDifference = NULL
Comp3PercentDifference = NULL
Area1 = NULL
Area2 = NULL

SS <- 0.05

for (i in 2:366) {
  #Max Concentration Comparison
  tempModel1 <- subset(Model1, Model1$Day == i)
  tempModel2 <- subset(Model2, Model2$Day == i)
  
  ############################################
  ##### Maximum Concentration Comparison #####
  ############################################
  Comp1PercentDifference[i] <- (max(tempModel2$Conc) - max(tempModel1$Conc))/mean(c(max(tempModel1$Conc), max(tempModel2$Conc)))
  
  ############################################
  ##### Unweighted Center of Concentration #####
  ############################################
  AvgLat1 <- mean(tempModel1$Lat-mean(StackLoc[,1]))
  AvgLon1 <- mean(tempModel1$Lon-mean(StackLoc[,2]))
  CentModel1 <- sqrt(AvgLat1^2 + AvgLon1^2)
  
  AvgLat2 <- mean(tempModel2$Lat-mean(StackLoc[,1]))
  AvgLon2 <- mean(tempModel2$Lon-mean(StackLoc[,2]))
  CentModel2 <- sqrt(AvgLat2^2 + AvgLon2^2)
  
  Comp2PercentDifference[i] <- (CentModel2 - CentModel1)/mean(c(CentModel2, CentModel1))
  
  ############################################
  ##### Comparison of Dispersion Area ########
  ############################################
  Mod1 <- matrix(ncol = ((max(tempModel1$Lon)-min(tempModel1$Lon))/SS) + 1, nrow = ((max(tempModel1$Lat)-min(tempModel1$Lat))/SS) + 1)
  Mod2 <- matrix(ncol = ((max(tempModel2$Lon)-min(tempModel2$Lon))/SS) + 1, nrow = ((max(tempModel2$Lat)-min(tempModel2$Lat))/SS) + 1)
  
  tempModel1$Lon <- (tempModel1$Lon - min(tempModel1$Lon))/SS
  tempModel1$Lat <- (tempModel1$Lat - min(tempModel1$Lat))/SS
  
  tempModel2$Lon <- (tempModel2$Lon - min(tempModel2$Lon))/SS
  tempModel2$Lat <- (tempModel2$Lat - min(tempModel2$Lat))/SS
  
  for (j in 1:nrow(Mod1)) {
    
    for (k in 1:ncol(Mod1)) {
      
      temp <- mean(tempModel1$Conc[tempModel1$Lon >= k & tempModel1$Lon >= k + 1 & tempModel1$Lat >= j & tempModel1$Lat >= j + 1])
      
      if(is.nan(temp)) {Mod1[j,k] <- 0} else {Mod1[j,k] <- 1}
      
    }
  }
  
  
  
  for (j in 1:nrow(Mod2)) {
    
    for (k in 1:ncol(Mod2)) {
      
      temp <- mean(tempModel2$Conc[tempModel2$Lon >= k & tempModel2$Lon >= k + 1 & tempModel2$Lat >= j & tempModel2$Lat >= j + 1])
      
      if(is.nan(temp)) {Mod2[j,k] <- 0} else {Mod2[j,k] <- 1}
      
    }
    
  }
  
  Area1[i] <- sum(Mod1)
  Area2[i] <- sum(Mod2)
  
}

Comp3PercentDifference <- round((Area2 - Area1)/(Area2 + Area1), 2)

##############################################
##### Form the Data Frame ####################
##############################################
PercentDifferenceData <- 100*data.frame(Comp1PercentDifference, Comp2PercentDifference, Comp3PercentDifference,
                                        sqrt(Comp1PercentDifference^2 + Comp2PercentDifference^2 + Comp3PercentDifference^2))
PercentDifferenceData <- PercentDifferenceData[2:366,]
names(PercentDifferenceData) <- c("Metric 1", "Metric 2", "Metric 3", "Overall")

MaxDay <- which(PercentDifferenceData$Overall == max(PercentDifferenceData$Overall))

##############################################
##### Plots ##################################
##############################################
ggplot(data = PercentDifferenceData, aes(x = c(2:366), y = Overall)) +
  geom_point() +
  xlab("Day") +
  ylab("Percent Difference (%)") +
  geom_smooth(se = FALSE) +
  theme_bw()

##########

PlotModel1 <- subset(Model1, Day == MaxDay)
PlotModel2 <- subset(Model2, Day == MaxDay)

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 06, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 

##########

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 06, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 

PercentDifferenceData[MaxDay,]
