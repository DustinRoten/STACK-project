ModelType <- "A"
MonthName <- c("January",
               "February",
               "March",
               "April",
               "May",
               "June",
               "July",
               "August",
               "September",
               "October",
               "November",
               "December")

for (i in 1:12) {
  
  #filepath <- paste("JSC-2012/", ModelType, "-", MonthName[i], sep = "")             #Temporary because I'm an idiot
  filepath <- paste(ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
  for (j in 1:length(files)) {
    
    Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
    colnames(Day)[1] <- "Day"
    colnames(Day)[2] <- "Hour"
    colnames(Day)[3] <- "Lat"
    colnames(Day)[4] <- "Lon"
    colnames(Day)[5] <- "Conc"
    Day[,6] <- NULL
    
    if (j == 1) {Month <- Day}
    
    else{Month <- as.data.frame(rbind(Month, Day))}
  }
  
  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))
  
}

Model = NULL
temp = NULL

for (i in 1:12) {
  
  temp <- paste("data", i, sep = "")
  assign(temp, read.csv(paste(ModelType, "-", MonthName[i], ".csv", sep = "")))
  
}

Model <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
Model <- Model[2:6]
Model <- Model[order(Model$Day),]

write.csv(Model, paste(ModelType,"-",2012,sep = ""))







ModelType <- "B"
MonthName <- c("January",
               "February",
               "March",
               "April",
               "May",
               "June",
               "July",
               "August",
               "September",
               "October",
               "November",
               "December")

for (i in 1:12) {
  
  #filepath <- paste("JSC-2012/", ModelType, "-", MonthName[i], sep = "")             #Temporary because I'm an idiot
  filepath <- paste(ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
for (j in 1:length(files)) {
  
Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
colnames(Day)[1] <- "Day"
colnames(Day)[2] <- "Hour"
colnames(Day)[3] <- "Lat"
colnames(Day)[4] <- "Lon"
colnames(Day)[5] <- "Conc"
Day[,6] <- NULL

  if (j == 1) {Month <- Day}
    
  else{Month <- as.data.frame(rbind(Month, Day))}
}

  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))

}

Model = NULL
temp = NULL

for (i in 1:12) {

    temp <- paste("data", i, sep = "")
    assign(temp, read.csv(paste(ModelType, "-", MonthName[i], ".csv", sep = "")))

}

Model <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
Model <- Model[2:6]
Model <- Model[order(Model$Day),]

write.csv(Model, paste(ModelType,"-",2012,sep = ""))








ModelType <- "C"

for (i in 1:12) {
  
  #filepath <- paste("JSC-2012/", ModelType, "-", MonthName[i], sep = "")             #Temporary because I'm an idiot
  filepath <- paste(ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
  for (j in 1:length(files)) {
    
    Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
    colnames(Day)[1] <- "Day"
    colnames(Day)[2] <- "Hour"
    colnames(Day)[3] <- "Lat"
    colnames(Day)[4] <- "Lon"
    colnames(Day)[5] <- "Conc"
    Day[,6] <- NULL
    
    if (j == 1) {Month <- Day}
    
    else{Month <- as.data.frame(rbind(Month, Day))}
  }
  
  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))
  
}

Model = NULL
temp = NULL

for (i in 1:12) {
  
  temp <- paste("data", i, sep = "")
  assign(temp, read.csv(paste(ModelType, "-", MonthName[i], ".csv", sep = "")))
  
}

Model <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
Model <- Model[2:6]
Model <- Model[order(Model$Day),]

write.csv(Model, paste(ModelType,"-",2012,sep = ""))







ModelType <- "D"

for (i in 1:12) {
  
  #filepath <- paste("JSC-2012/", ModelType, "-", MonthName[i], sep = "")             #Temporary because I'm an idiot
  filepath <- paste(ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
  for (j in 1:length(files)) {
    
    Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
    colnames(Day)[1] <- "Day"
    colnames(Day)[2] <- "Hour"
    colnames(Day)[3] <- "Lat"
    colnames(Day)[4] <- "Lon"
    colnames(Day)[5] <- "Conc"
    Day[,6] <- NULL
    
    if (j == 1) {Month <- Day}
    
    else{Month <- as.data.frame(rbind(Month, Day))}
  }
  
  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))
  
}

Model = NULL
temp = NULL

for (i in 1:12) {
  
  temp <- paste("data", i, sep = "")
  assign(temp, read.csv(paste(ModelType, "-", MonthName[i], ".csv", sep = "")))
  
}

Model <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
Model <- Model[2:6]
Model <- Model[order(Model$Day),]

write.csv(Model, paste(ModelType,"-",2012,sep = ""))





ModelType <- "E"
MonthName <- c("January",
               "February",
               "March",
               "April",
               "May",
               "June",
               "July",
               "August",
               "September",
               "October",
               "November",
               "December")

for (i in 1:12) {
  
  #filepath <- paste("JSC-2012/", ModelType, "-", MonthName[i], sep = "")             #Temporary because I'm an idiot
  filepath <- paste(ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
  for (j in 1:length(files)) {
    
    Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
    colnames(Day)[1] <- "Day"
    colnames(Day)[2] <- "Hour"
    colnames(Day)[3] <- "Lat"
    colnames(Day)[4] <- "Lon"
    colnames(Day)[5] <- "Conc"
    Day[,6] <- NULL
    
    if (j == 1) {Month <- Day}
    
    else{Month <- as.data.frame(rbind(Month, Day))}
  }
  
  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))
  
}

Model = NULL
temp = NULL

for (i in 1:12) {
  
  temp <- paste("data", i, sep = "")
  assign(temp, read.csv(paste(ModelType, "-", MonthName[i], ".csv", sep = "")))
  
}

Model <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
Model <- Model[2:6]
Model <- Model[order(Model$Day),]

write.csv(Model, paste(ModelType,"-",2012,sep = ""))