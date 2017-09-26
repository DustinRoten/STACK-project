# Dustin Roten - 2017
# Various functions for the HYSPLIT - STACK project

##### User-defined functions are included below. #####

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
RadialDilation <- function(x, y) {
  
    stretch <- 1 + y/100
    
    RadDilation <- as.data.frame(cbind(x[,1:4],
                                      stretch*x$LAT,
                                      stretch*x$LON,
                                      x[,7]))
    
    names(RadDilation) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
    
    return(RadDilation)
  
}
###############################################################
###############################################################


#####################################################################################
##### Angularly(?) stretch a dispersion from the mean angle (step size is 0.01) #####
##### AngularStretch(DISPERSION, AMOUNT (0.01 step size))                       #####
#####################################################################################
AngularStretch <- function(x, y) {
    
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
          
            StretchedAngles[i] <- AngleStretch*abs(x$theta[i] - MeanAngle)
          
        } else {StretchedAngles[i] <- x$theta[i]}
            
    AngStretchLAT[i] <- x$LON[i]*sin(StretchedAngles[i]) + x$LAT[i]*cos(StretchedAngles[i])
    AngStretchLON[i] <- x$LON[i]*cos(StretchedAngles[i]) - x$LAT[i]*sin(StretchedAngles[i])
        
    }
    
    AngStretch <- as.data.frame(cbind(x[,1:4],
                                AngStretchLAT,
                                AngStretchLON,
                                x[,7]))
    
    names(AngStretch) <- c("YEAR", "MO", "DA", "HR", "LAT", "LON", "CO2")
    
    return(AngStretch)
    
}
#####################################################################################
#####################################################################################



# MRS Measure.
# MRSMeasure(DISPERION1, DISPERION2, EPA EMISSIONS, RESOLUTION)
MRSMeasure <- function(w, x, y, z) {
  
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
    
    # Scaling
    size1 <- length(which(Model1_Matrix != 0))
    size2 <- length(which(Model2_Matrix != 0))
    
    Ratio[i+1] <- (size1 - size2)/mean(c(size1, size2))
    
    for (g in 1:y_steps) {
      
        for(h in 1:x_steps) {
        
            CellAveragedPollutant_1 <- mean(Model1[,7][Model1$LON >= minLON + Resolution*(h-1) & Model1$LON < minLON + Resolution*h &
                                                     Model1$LAT >= minLAT + Resolution*(g-1) & Model1$LAT < minLAT + Resolution*g])
        
            Model1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, (1+Ratio)*CellAveragedPollutant_1)
        
        }
    }

    if(Dispersion[1,1] %% 4 == 0) {Day <- 366} else {Day <- 365}
          
    MRSMeasureValue <- ((100*20000*(Resolution*111000)^2)/(2*(Emissions/Day)))*sum(abs(Model2_Matrix - Model1_Matrix))

    return(MRSMeasureValue)
  
}



# Spell Measures

# Mean Angle
MeanAngleMeasure <- function(x, y, a, b) {
  
    Model1 <- x
    Model2 <- y
    LAT <- a
    LON <- b
  
    emit1 <- data.frame(Model1[,6], Model1[,5], Model1[,7])
    emit2 <- data.frame(Model2[,6], Model2[,5], Model2[,7])
    
    ex.angle1 <- bearing(c(LON, LAT), emit1[, 1:2])
    ex.angle2 <- bearing(c(LON, LAT), emit2[, 1:2])
    
    mean.angle1 <- sum(ex.angle1*emit1[,3])/sum(emit1[,3])
    mean.angle2 <- sum(ex.angle2*emit2[,3])/sum(emit2[,3])
    
    return(mean.angle2 - mean.angle1)
  
}



# STD Angle
STDAngleMeasure <- function(x, y, a, b) {
  
    Model1 <- x
    Model2 <- y
    LAT <- a
    LON <- b
  
    emit1 <- data.frame(Model1[,6], Model1[,5], Model1[,7])
    emit2 <- data.frame(Model2[,6], Model2[,5], Model2[,7])
  
    ex.angle1 <- bearing(c(LON, LAT), emit1[, 1:2])
    ex.angle2 <- bearing(c(LON, LAT), emit2[, 1:2])
  
    mean.angle1 <- sum(ex.angle1*emit1[,3])/sum(emit1[,3])
    mean.angle2 <- sum(ex.angle2*emit2[,3])/sum(emit2[,3])
  
    std.angle1 <- (sqrt((sum(emit1[,3]*(ex.angle1 - mean.angle1))^2)/sum(emit1[,3])))*(nrow(emit1)/(nrow(emit1) - 1))
    std.angle2 <- (sqrt((sum(emit2[,3]*(ex.angle2 - mean.angle2))^2)/sum(emit2[,3])))*(nrow(emit2)/(nrow(emit2) - 1))
    
    return(std.angle2 - std.angle1)
    
}



# Center of Mass Measurement
COMMeasure <- function(w, x, y, a, b) {
      
    AtOrigin <- w
    Model1 <- x
    Model2 <- y
    LAT <- a
    LON <- b
    
    if(w == "T" | w == "t" | w == "TRUE" | w == "True" | w == "true" | w == TRUE) {
        
        xx <- sum((111*Model1[,6])*Model1[,7])/sum(Model1[,7])
        xy <- sum((111*Model1[,5])*Model1[,7])/sum(Model1[,7])
        
        yx <- sum((111*Model2[,6])*Model2[,7])/sum(Model2[,7])
        yy <- sum((111*Model2[,5])*Model2[,7])/sum(Model2[,7])
        
    } else {
    
        xx <- sum((111*Model1[,6] - 111*b)*Model1[,7])/sum(Model1[,7])
        xy <- sum((111*Model1[,5] - 111*a)*Model1[,7])/sum(Model1[,7])
    
        yx <- sum((111*Model2[,6] - 111*b)*Model2[,7])/sum(Model2[,7])
        yy <- sum((111*Model2[,5] - 111*a)*Model2[,7])/sum(Model2[,7])
    
    }
          
    return(sqrt((xx - yx)^2 + (yx - yy)^2))
  
}
