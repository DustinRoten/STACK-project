### Master DEMOFunction.R File ###

### currently in development ###

#########################################################################
##### Shift a dispersion to (0,0).                                  #####
##### ShiftToOrigin(DISPERSION, SOURCE LATITUDE, SOURCE LONGITUDE). #####
##### "Shift" or "Undo" is used as the first argument               #####
#########################################################################
ShiftToOrigin <- function(w, x, y, z) {
  
    if(w == "Shift" | w == "SHIFT" | w == "S") {
    
        x[,5] <- x[,5] - y
        x[,6] <- x[,6] - z
    
    }
  
    else if(w == "Undo" | w == "UNDO" | w == "U") {
    
        x[,5] <- x[,5] + y
        x[,6] <- x[,6] + z 
    
    } else {print("ERROR!")}
  
    return(x)
  
}
#########################################################################
#########################################################################



#############################################################################
##### Continually shift (horizontally) a dispersion in 0.1 degree steps #####
##### ShiftDispersion(DISPERSION, AMOUNT (0.1 degree increments))       #####
#############################################################################
ShiftDispersion <- function(x, y) {
  
    shift <- y/10
  
    temp <- as.data.frame(cbind(x[,1:4], x$LAT, x$LON - shift, x[,7]))
  
    names(temp) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    return(temp)
  
}
#############################################################################
#############################################################################



########################################################################
# Rotate Dispersion to angle theta (0.01 resolution)               #####
# RotateDispersion(DISPERSION, AMOUNT (0.01 resolution/step size)) #####
########################################################################
RotateDispersion <- function(x, y) {
  
    theta <- y/100
  
    RotatedDispersion <- as.data.frame(cbind(x[,1:4],
                                           x$LON*sinpi(theta) + x$LAT*cospi(theta),
                                           x$LON*cospi(theta) - x$LAT*sinpi(theta),
                                           x[,7]))
  
    names(RotatedDispersion) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    return(RotatedDispersion)
  
}
########################################################################
########################################################################



###############################################################
##### "Radially" dilate a dispersion (0.01 step size)     #####
##### RadialDilation(DISPERSION, AMOUNT (0.01 step size)) #####
###############################################################
TESTRadialDilation <- function(x, y) {
  
    stretch <- 1 + y/100
  
    RadDilation <- as.data.frame(cbind(x[,1:4],
                                     stretch*x$LAT,
                                     stretch*x$LON,
                                     x[,7]))
    
    names(RadDilation) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
    
    Dist1 <- sqrt(x$LAT^2 + x$LON^2)
    Dist2 <- sqrt(RadDilation$LAT^2 + RadDilation$LON^2)
    
    MeanDist1 <- mean(sqrt(x$LAT^2 + x$LON^2))
    MeanDist2 <- mean(sqrt(RadDilation$LAT^2 + RadDilation$LON^2))
    
    StandDivRadial1 <- sd(atan(x$LAT/x$LON))
    StandDivRadial2 <- sd(atan(RadDilation$LAT/RadDilation$LON))
    
    ScaleFactor <- 1 - ((MeanDist1*(StandDivRadial1^2))/(MeanDist2*(StandDivRadial2^2)))*
      exp(0.5*(((StandDivRadial2*Dist1)^2 - (StandDivRadial1*Dist2)^2)/(StandDivRadial1*StandDivRadial2)^2))
    
    RadDilation <- as.data.frame(cbind(x[,1:4],
                                       stretch*x$LAT,
                                       stretch*x$LON,
                                       ScaleFactor*x[,7]))
  
    names(RadDilation) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    return(RadDilation)
  
}
###############################################################
###############################################################



#####################################################################################
##### Angularly(?) stretch a dispersion from the mean angle (step size is 0.01) #####
##### AngularStretch(DISPERSION, AMOUNT (0.01 step size))                       #####
#####################################################################################
TESTAngularStretch <- function(x, y) {
  
    x$theta <- atan(x$LAT/x$LON)
  
    MeanAngle <- mean(x[["theta"]])
  
    AngleStretch <- y/100
  
    StretchedAngles = NULL
    AngStretchLAT = NULL
    AngStretchLON = NULL
  
    for(i in 1:nrow(x)) {
    
        if(x$theta[i] > MeanAngle) {
      
            StretchedAngles[i] <- AngleStretch*abs(x$theta[i] - MeanAngle)
      
        } else if(x$theta[i] < MeanAngle) {
      
            StretchedAngles[i] <- -1*AngleStretch*abs(x$theta[i] - MeanAngle)
      
        } else {StretchedAngles[i] <- x$theta[i]}
    
            AngStretchLAT[i] <- x$LON[i]*sin(StretchedAngles[i]) + x$LAT[i]*cos(StretchedAngles[i])
            AngStretchLON[i] <- x$LON[i]*cos(StretchedAngles[i]) - x$LAT[i]*sin(StretchedAngles[i])
    
    }
  
    AngStretch <- as.data.frame(cbind(x[,1:4],
                                    AngStretchLAT,
                                    AngStretchLON,
                                    x[,7]))
    
    names(AngStretch) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
    
    Dist1 <- sqrt(x$LAT^2 + x$LON^2)
    Dist2 <- sqrt(AngStretch$LAT^2 + AngStretch$LON^2)
  
    MeanDist1 <- mean(sqrt(x$LAT^2 + x$LON^2))
    MeanDist2 <- mean(sqrt(AngStretch$LAT^2 + AngStretch$LON^2))
  
    StandDivRadial1 <- sd(atan(x$LAT/x$LON))
    StandDivRadial2 <- sd(atan(AngStretch$LAT/AngStretch$LON))
  
    ScaleFactor <- 1 - ((MeanDist1*(StandDivRadial1^2))/(MeanDist2*(StandDivRadial2^2)))*
      exp(0.5*(((StandDivRadial2*Dist1)^2 - (StandDivRadial1*Dist2)^2)/(StandDivRadial1*StandDivRadial2)^2))
  
    AngStretch <- as.data.frame(cbind(x[,1:4],
                                    AngStretchLAT,
                                    AngStretchLON,
                                    ScaleFactor*x[,7]))
  
    names(AngStretch) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
  
    return(AngStretch)
  
}
#####################################################################################
#####################################################################################


##### TESTING MRS Measure #####
TESTMRSMeasure <- function(w, x, y, z) {
  
  Resolution <- z
  Emissions <- y
  Model1 <- w
  Model2 <- x
  
  x_range <- max( max(Model1$LON), max(Model2$LON) ) - min( min(Model1$LON), min(Model2$LON) ) + 1
  y_range <- max( max(Model1$LAT), max(Model2$LAT) ) - min( min(Model1$LAT), min(Model2$LAT) ) + 1
  
  x_steps <- round(x_range/Resolution, 0)
  y_steps <- round(y_range/Resolution, 0)
  
  Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  minLON <- min(min(Model1$LON), min(Model2$LON))
  minLAT <- min(min(Model1$LAT), min(Model2$LAT))
  
  for (g in 1:y_steps) {
    
    for(h in 1:x_steps) {
      
      CellAveragedPollutant_1 <- mean(Model1[,7][Model1$LON >= minLON + Resolution*(h-1) & Model1$LON < minLON + Resolution*h &
                                                   Model1$LAT >= minLAT + Resolution*(g-1) & Model1$LAT < minLAT + Resolution*g])
      
      CellAveragedPollutant_2 <- mean(Model2[,7][Model2$LON >= minLON + Resolution*(h-1) & Model2$LON < minLON + Resolution*h &
                                                   Model2$LAT >= minLAT + Resolution*(g-1) & Model2$LAT < minLAT + Resolution*g])
      
      Model1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
      Model2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
      
    }
  }
  
  Model1_Matrix <- Model1_Matrix*(sum(Model1_Matrix)/sum(Model2_Matrix))
  
  if(Dispersion[1,1] %% 4 == 0) {Day <- 366} else {Day <- 365}
  
  MRSMeasureValue <- (((11100^2)*10000)/(Emissions/Day))*sum(abs(Model2_Matrix - Model1_Matrix))*100
  
  return(MRSMeasureValue)
  
}
