##Would we be picking out for a certain day (i)? 
##How do we not get a million vectors and just the angle?
##Ideas for variance of the angle? I dont think the number is correct..

library(geosphere)

Model1 <- read.table("JEC_A", sep = "", header = TRUE) 
Model2 <- read.table("JEC_E", sep = "", header = TRUE) ##all parameters included

for (i in 2:366) {
    
    ## JEC-A
    DayModel1 <- subset(Model1, DA == i)     #This will create a dataframe (DayModel1) that only has Day "i" in it.
  
    plant <- c(-96.1172, 39.28682)
  
    lat <- DayModel1[,3]
    long <- DayModel1[,4]
    conc <- DayModel1[,5]
    emit <- cbind(long, lat, conc)
  
    ex.angle <- bearing(plant, emit[,1:2])
    print(ex.angle)
  
    mean.angle <- sum(ex.angle*emit)/sum(emit)
    print(mean.angle)
  
    var.angle1 <- sqrt((sum(na.omit(emit)*(ex.angle - mean.angle)))^2)/sum(na.omit(emit))
    print(var.angle1)
  
    ## JEC-E
    DayModel2 <- subset(Model2, DA == i)     #This will create a dataframe (DayModel2) that only has Day "i" in it.
  
    plant2 <- c(-96.1172, 39.28682)
  
    lat2 <- DayModel2[,3]
    long2 <- DayModel2[,4]
    conc2 <- DayModel2[,5]
    emit2 <- cbind(long, lat, conc)
  
    ex.angle2 <- bearing(plant2, emit2[,1:2])
    print(ex.angle2)
  
    mean.angle2 <- sum(ex.angle2*emit2)/sum(emit2)
    print(mean.angle2)
  
    var.angle2 <- sqrt((sum(emit)*(sum(ex.angle - mean.angle)^2))/sum(emit))
    print(var.angle2)
    
    ### Difference of E - A
    mean.angleJEC <- mean.angle2 - mean.angle
    print(mean.angleJEC)
  
    var.angleJEC <- var.angle2 - var.angle1
    print(var.angleJEC)
    
}

####other things for code
plot(long, lat, xlim= c(-97, -94), ylim=c(39, 41))
points((-96.1172),(39.28682), col='red')
