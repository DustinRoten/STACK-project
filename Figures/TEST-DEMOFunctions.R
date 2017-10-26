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
  
        x$LAT <- x$LAT - y
        x$LON <- x$LON - z

    }
  
    else if(w == "Undo" | w == "UNDO" | w == "U") {
    
        x$LAT <- x$LAT + y
        x$LON <- x$LON + z 
    
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
    
    Model1_Matrix <- Model1_Matrix*(sum(Model1_Matrix)/sum(Model2_Matrix))

    if(Dispersion[1,1] %% 4 == 0) {Day <- 366} else {Day <- 365}
          
    MRSMeasureValue <- (1/sum(Model1_Matrix))*sum(abs(Model2_Matrix - Model1_Matrix))*100

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



# Weighted Center of Mass Measurement. Requires LAT, LON, CO2 columns.
COMMeasure <- function(x, y) {
        
    x1 <- sum((x$LON)*x$CO2)/sum(x$LON)
    y1 <- sum((x$LAT)*x$CO2)/sum(x$LAT)
    x2 <- sum((y$LON)*y$CO2)/sum(y$LON)
    y2 <- sum((y$LAT)*y$CO2)/sum(y$LAT)
          
    return(111000*sqrt((x1 - x2)^2 + (y1 - y2)^2))
  
}

# Weighted angle to COM
COMAngle <- function(x) {
  
    x1 <- sum((x$LON)*x$CO2)/sum(x$LON)
    y1 <- sum((x$LAT)*x$CO2)/sum(x$LAT)
    
    angle <- if( (180/pi)*atan2(y1, x1) >= 0) {(180/pi)*atan2(y1, x1)} else {(180/pi)*atan2(y1, x1) + 360}
  
    return(angle)
}

GridDispersions <- function(x, y, z) {
  
    x_range <- max( max(x$LON), max(y$LON) ) - min( min(x$LON), min(y$LON) ) + 1
    y_range <- max( max(x$LAT), max(y$LAT) ) - min( min(x$LAT), min(y$LAT) ) + 1
  
    # Translate the area into the number of grid cells based on the Resolution required.
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
  
    # Set up 2 matrices to store values in
    Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
    # These values are going to be used in interations to define the bounds of each grid cell
    minLON <- min(min(x$LON), min(y$LON))
    minLAT <- min(min(x$LAT), min(y$LAT))
  
    # Grid the dispersions
    for (g in 1:y_steps) {
    
        for(h in 1:x_steps) {
      
            CellAveragedPollutant_1 <- mean(x[,7][x$LON >= minLON + Resolution*(h-1) & x$LON < minLON + Resolution*h &
                                                   x$LAT >= minLAT + Resolution*(g-1) & x$LAT < minLAT + Resolution*g])
      
            CellAveragedPollutant_2 <- mean(y[,7][y$LON >= minLON + Resolution*(h-1) & y$LON < minLON + Resolution*h &
                                                   y$LAT >= minLAT + Resolution*(g-1) & y$LAT < minLAT + Resolution*g])
      
            Model1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
            Model2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
      
        }
    }
  
  # Melt the matrices for easier plotting
  # Gridded_Model1 <- melt(Model1_Matrix)
  # Gridded_Model2 <- melt(Model2_Matrix)
  
  # names(Gridded_Model1) <- c("LAT", "LON", "CO2")
  # names(Gridded_Model2) <- c("LAT", "LON", "CO2")
  
  if (z == 1) {return(Model1_Matrix)} else if (z == 2) {return(Model2_Matrix)} else {return(print("ERROR!"))}
  
  # Calibrate cell numbers to LAT/LON
  # Gridded_Model1$LAT <- minLAT + Resolution*(Gridded_Model1$LAT + 0.05)
  # Gridded_Model1$LON <- minLON + Resolution*(Gridded_Model1$LON + 0.05)
  # Gridded_Model2$LAT <- minLAT + Resolution*(Gridded_Model2$LAT + 0.05)
  # Gridded_Model2$LON <- minLON + Resolution*(Gridded_Model2$LON + 0.05)
  
  # Place NA's in each cell containing 0 so they are not graphed
  # Gridded_Model1$LAT[Gridded_Model1$CO2 == 0] <- NA
  # Gridded_Model1$LON[Gridded_Model1$CO2 == 0] <- NA
  # Gridded_Model2$LAT[Gridded_Model2$CO2 == 0] <- NA
  # Gridded_Model2$LON[Gridded_Model2$CO2 == 0] <- NA
  
  # RetVals <- data.frame(Gridded_Model1, Gridded_Model2)
  # names(RetVals) <- c("LAT1", "LON1", "CO2", "LAT2", "LON2", "CO2")
  
  # return(RetVals)
  
}

# Theta should be in radians
RotateToAxis <- function(x, theta) {
  
    RotatedDispersion <- as.data.frame(cbind(
                                           x$LON*(180/pi)*sin(-1*theta) + x$LAT*(180/pi)*cos(-1*theta),
                                           x$LON*(180/pi)*cos(-1*theta) - x$LAT*(180/pi)*sin(-1*theta),
                                           x$CO2))
  
    names(RotatedDispersion) <- c("LAT", "LON", "CO2")
    
    return(RotatedDispersion)
  
}

LocateOrigin <- function(x, y, z) {
  
    x_range <- max( max(x$LON), max(y$LON) ) - min( min(x$LON), min(y$LON) ) + 1
    y_range <- max( max(x$LAT), max(y$LAT) ) - min( min(x$LAT), min(y$LAT) ) + 1
  
    # Translate the area into the number of grid cells based on the Resolution required.
    x_steps <- round(x_range/Resolution, 0)
    y_steps <- round(y_range/Resolution, 0)
  
    # Set up 2 matrices to store values in
    Model1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
    Model2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
  
    # These values are going to be used in interations to define the bounds of each grid cell
    minLON <- min(min(x$LON), min(y$LON))
    minLAT <- min(min(x$LAT), min(y$LAT))
  
    # Grid the dispersions
    for (g in 1:y_steps) {
    
        for(h in 1:x_steps) {
      
            CellAveragedPollutant_1 <- subset(x, x$LON >= minLON + Resolution*(h-1) & x$LON < minLON + Resolution*h &
                                                  x$LAT >= minLAT + Resolution*(g-1) & x$LAT < minLAT + Resolution*g)
      
            CellAveragedPollutant_2 <- subset(y, y$LON >= minLON + Resolution*(h-1) & y$LON < minLON + Resolution*h &
                                                  y$LAT >= minLAT + Resolution*(g-1) & y$LAT < minLAT + Resolution*g)
      
            if (min(CellAveragedPollutant_1$LON) < 0 & max(CellAveragedPollutant_1$LON) > 0 &
                min(CellAveragedPollutant_1$LAT < 0 & max(CellAveragedPollutant_1$LAT))) {
            
                Origins <- c(g,h)
                
                else if(min(CellAveragedPollutant_2$LON) < 0 & max(CellAveragedPollutant_2$LON) > 0 &
                        min(CellAveragedPollutant_2$LAT < 0 & max(CellAveragedPollutant_2$LAT))) {
                  
                    Origins <- c(g,h)
                  
                } else {}
              
            }
        }
    }
  
    return(Origins)

}
