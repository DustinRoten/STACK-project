#-----------------------------------------------------------------------------------------------#
# HYSPLIT-FullAutomation                                                                        #
# Below is a first attempt at a full automation of a sensitivity analysis for any location(s).  #
# Any year greater than 2000.                                                                   #
#-----------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------#
# Immediately Run Script - User is prompted for all required inputs. #
#--------------------------------------------------------------------#



# Ask user for the year of interest, number of locations to be considered and the information at each location.
if( interactive() ) {
  
  NAMpath <- readline(prompt = "This script uses NAM - 12km data. Include the NAM directory here - ")  
  
  StartYear <- as.numeric(readline(prompt = "Input Year (YYYY > 2000) - "))
  
  NumberOfLocations <- as.numeric(readline(prompt = "Enter the number of emissions sources - "))
  
  # initialize an empty dataframe
  LocationInformation <- data.frame()
  
  for(i in 1:NumberOfLocations) {
    
    LocationInformation[i, 1] <- readline(paste(prompt = "Provide a three letter title for location", i, "-", " ", sep = " "))
    LocationInformation[i, 2] <- as.numeric(readline(paste(prompt = "What is the total eGRID emission value for", LocationInformation[i,1], "in kilograms ?", " ", sep = " ")))
    LocationInformation[i, 3] <- as.numeric(readline(paste(prompt = "How many exhaust points are there at location", LocationInformation[i,1], "?", " ", sep = " ")))
    LocationInformation[i, 4] <- as.numeric(readline(paste(prompt = "What is the eGRID latitude value for", LocationInformation[i,1],"?", " ", sep = " ")))
    LocationInformation[i, 5] <- as.numeric(readline(paste(prompt = "What is the eGRID longitude value for", LocationInformation[i,1],"?", " ", sep = " ")))
    
  }
  
  names(LocationInformation) <- c("Name", "eGRID_Emissions", "Exhaust_Points", "eGRID_Lat", "eGRID_Lon")
  
  for(i in 1:nrow(LocationInformation)) {
    
    StackParams <- data.frame()
    
    for(j in 1:LocationInformation[i, 3]) {
      
      Latitude <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                            "Please provide the stack latitude (deg) - ", sep = "")))
      
      Longitude <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                             "Please provide the stack longitude (deg) - ", sep = "")))
      
      Height <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                          "Please provide the stack height (m) - ", sep = "")))
      
      EmisRate <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                            "Please provide the stack emission rate (kg/hr) - ", sep = "")))
      
      Area <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                        "Please provide the stack area (m^2) - ", sep = "")))
      
      Heat <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                        "Please provide the net stack heat (W) - ", sep = "")))
      
      temp <- c(Latitude, Longitude, Height, EmisRate, Area, Heat)
      
      StackParams <- rbind(StackParams, temp)
    }
    
    names(StackParams) <- c("Latitude", "Longitude", "Height", "EmisRate", "Area", "Heat")
    
    assign(noquote(paste(LocationInformation[i, 1], "_StackParams", sep = "")), StackParams)
    
  }
  
  
  
  Pollutant <- readline(prompt = "Enter pollutant name (XXXX) - ")
  
  ParticleDiameter <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                     "Please provide the particle diameter (um) - ", sep = ""))
  
  ParticleDensity <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                    "Please provide the particle density (g/cc) - ", sep = ""))
  
  ParticleDepoVelocity <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                         "Please provide the deposition velocity (m/s) - ", sep = ""))
  
  ParticleMolecularWeight <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                            "Please provide the particle molecular weight (g) - ", sep = ""))
  
  ParticleARatio <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                   "Please provide the particle A-Ratio - ", sep = ""))
  
  ParticleDRatio <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                   "Please provide the particle D-Ratio - ", sep = ""))
  
  ParticleHenry <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                  "Please provide the 'Henry' - ", sep = ""))
  
  ParticleHenryConstant <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                          "Please provide the value for Henry's Constant (M/a) - ", sep = ""))
  
  ParticleInCloud <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                    "Please provide the In-cloud value (l/l) - ", sep = ""))
  
  ParticleBelowCloud <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                       "Please provide the Below-Cloud value (1/s) - ", sep = ""))
  
  ParticleRadioactive <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                        "Please provide the particle halflife (days) - ", sep = ""))
  
  ParticleResuspensionFactor <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                               "Please provide the pollutant resuspension factor (1/m) - ", sep = ""))
  
  Resolution <- as.numeric(readline(paste(prompt = "Please provie a resolution (in degrees) for this analysis - ")))
  
  
  # Insert a parameter checking method here!
  
}

# User information completed


ChemicalParameters1 <- as.numeric(c(ParticleDiameter, ParticleDensity, 1))
ChemicalParameters2 <- as.numeric(c(ParticleDepoVelocity, ParticleMolecularWeight, ParticleARatio, ParticleDRatio, ParticleHenry))
ChemicalParameters3 <- as.numeric(c(ParticleHenryConstant, ParticleInCloud, ParticleBelowCloud))
ChemicalParameters4 <- as.numeric(ParticleRadioactive)
ChemicalParameters5 <- as.numeric(ParticleResuspensionFactor)


# Constructing the time management dataframe
MonthNames <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
DaysInMonth <- c(31, if (StartYear %% 4 == 0) {29} else{28}, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
MonthData <- data.frame(MonthNames, DaysInMonth)


# Constructing a vector of meteorology file names
MeteorologyFileNames <- NULL
c <- 1
for(a in 1:12) {
  
    for(b in 1:MonthData[a,2]) {
    
        MeteorologyFileNames[c] <- paste(StartYear, if(a <= 9) {"0"} else {}, a, if(b <= 9) {"0"} else {}, b, "_nam12", sep = "")
        c <- c + 1
        
    }
}

##### Run from here if parameters are already entered #####

# Check that all 3 required files are present
##if(file.exists("SETUP.CFG") & file.exists("EMITIMES") & file.exists("CONTROL") != TRUE) {
##    print("SETUP, EMITIMES, or CONTROL file missing")
##    stopifnot(file.exists("SETUP.CFG") & file.exists("EMITIMES") & file.exists("CONTROL"))
##} else {}
#

# Do work in the SystemFiles directory
setwd("SystemFiles")


# LOOP MODEL TYPE (A-F)
ModelType <- c("A", "B", "C", "D", "E", "F")

# A- eGRID Model
# B- 0m Stack Height
# C- 0m Stack Diameter
# D- 0W Net Heat (Om/s Exit Velocity)
# E- "Full" Model
# F- Modified eGRID Model (with EMITIMES file)


### Constructing the CONTROL file for HYSPLIT ###

# Model type to be used
for(z in 1:6) {     # Begins the "Model Type" loop
  
    ModType <- ModelType[z]
    
    # The *_StackParams file will be needed for each point source.
    for(i in 1:nrow(LocationInformation)) {     # Starts specific "Stack Information" for each location
      
      eval(parse(text = paste("StackInfo", "<- ", LocationInformation[i,1], "_StackParams", sep = "")))
      
      # This will generate the parent data frame that will be continuously appended. Each point source and model type will receive it's own file. XXX_X.
      
      ParentFileName <- paste(LocationInformation[i,1], "_", ModType, sep = "")
      ColumnNames <- c("DA", "HR", "LAT", "LON", paste(Pollutant))
      ParentDataFrame <- as.data.frame(setNames(replicate(6, numeric(0), simplify = FALSE), ColumnNames))
      
      eval(parse(text = paste(write.table(ParentDataFrame, ParentFileName, row.names = FALSE, col.names = TRUE))))
    
      for(q in 1:12) {     # Starts the loop for each month
        
          for(m in 1:MonthData[q,2]) {     # Starts the loop for each day of the month
          
              cat(
            
                  paste(StartYear - 2000, q, m, 00, collapse = " "),"\n",
                  if(ModType == "A") {1} else {LocationInformation[i,3]}, "\n",
            
                  sep = "", file = "CONTROL"
            
              )

              # This break in the CONTROL file is where multiple stacks (if applicaple) get added.
              # This is achieved by appending the portion of the CONTROL file generated from the above code.
          
              if(ModType == "A") {
            
                  line <- paste(LocationInformation[i,4], LocationInformation[i,5])
                  write(line, file = "CONTROL", append = TRUE)
            
              if(file.exists("SETUP.CFG") == TRUE) {
              
                  file.rename("SETUP.CFG", "NO_SETUP.CFG")
              
              } else {}
            
              } else if(ModType == "B") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = "CONTROL", append = TRUE)
              
                  }
            
              } else if(ModType == "C") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], 0, StackInfo[j,6], sep = " ")
                      write(line, file = "CONTROL", append = TRUE)
              
                  }
            
              } else if(ModType == "D") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], 0, sep = " ")
                      write(line, file = "CONTROL", append = TRUE)
              
                  }
            
              } else if(ModType == "E") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = "CONTROL", append = TRUE)
              
                }
            
              } else if(ModType == "F") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], 0, 0, sep = " ")
                      write(line, file = "CONTROL", append = TRUE)
              
                  }
            
              }     # This closes StackInfo
          
          
          # The remaining parameters of the CONTROL file are added here by appending the portion of the CONTROL file generated above.
          
          y <- m + sum(MonthData[1:q-1, 2])     # The value of y selects the meteorology file to be used.
          
          cat(
            
              24, "\n",     # Total run time (hrs)
              0, "\n",      # Method of vertical motion
              20000, "\n",  # Top of the model (m)
              3, "\n",      # Number of NAM12km files loaded in
            
              paste(NAMpath), "\n",
              paste(if(q == 1 & m == 1) {paste(StartYear-1, "1231_nam12", sep = "")} else {MeteorologyFileNames[y-1]}), "\n",
            
              paste(NAMpath), "\n",
              paste(MeteorologyFileNames[y]), "\n",
            
              paste(NAMpath), "\n",
              paste(if(q == 12 & m == 31) {paste(StartYear+1, "0101_nam12", sep = "")} else {MeteorologyFileNames[y+1]}), "\n",
            
              1, "\n",      # Number of pollutants
              Pollutant, "\n",
              (LocationInformation[i,2]/sum(MonthData[,2]))/24, "\n",    # This is an hourly rate
              24, "\n",
              paste(StartYear - 2000, q, m, 0, 0, collapse = " "), "\n",
              1, "\n",      # Number of grids = number of pollutants
              paste(round(mean(StackInfo[,1]), 5), round(mean(StackInfo[,2]), 5), collapse = " "), "\n",
              paste( c(0.05, 0.05), collapse = " "), "\n",     # Resolution of the grid (lat, lon)
              paste( c(80.0, 80.0), collapse = " "), "\n",     # Size of the display grid (lat, lon)
              "./", "\n",    # Save the files here
              paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""), "\n",    # This is the individual file name
              1, "\n",
              20000, "\n", # Vertical levels, top of model
              paste(StartYear - 2000, q, m, 0, 0, collapse = " "), "\n",
            
              # This conditionals adjusts the model stop date at the end of each month
              paste(        
              
              (if(q == 12 & m == MonthData[12,2]) {temp <- as.numeric((StartYear + 1)) - 2000} else {StartYear - 2000}),
              
              (if(m <= (MonthData[q,2]-1)) {q}
               else if(m == MonthData[q,2] & q != 12) {q+1}
               else if(q == 12 & m == MonthData[12,2]) {1}),
              
              (if (m <= (MonthData[q,2]-1)) {m+1} else {1}),
              00, 00, collapse = " "), "\n",
              # END stop date management
            
              paste( c(00, 24, 00), collapse = " "), "\n",     # Analysis method - averaging
              1, "\n",      # Number of particles for deposition
              paste(ChemicalParameters1, collapse = " "), "\n",
              paste(ChemicalParameters2, collapse = " "), "\n",
              paste(ChemicalParameters3, collapse = " "), "\n",
              paste(ChemicalParameters4, collapse = " "), "\n",
              paste(ChemicalParameters5, collapse = " "), "\n",
            
              sep = "", file = "CONTROL", append = TRUE
            
          )
          
          # EMITIMES file begins here
          if(ModType != "A") {
          
              cat(
              
                  paste("YYYY MM DD HH   DURATION(hhhh) #RECORDS", sep = ""),"\n",
                  paste("YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)"), "\n",
                  paste(StartYear, q, m, 0, 9999, LocationInformation[i,3], collapse = " "), "\n",
                  sep = "", file = "EMITIMES"
              
              )
          
              if(ModType == "B") {
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StartYear, q, m, 0, 0, 2400, StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = "EMITIMES", append = TRUE)
              
                  }
            
              } else if(ModType == "C") {
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StartYear, q, m, 0, 0, 2400, StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], 0, StackInfo[j,6], sep = " ")
                      write(line, file = "EMITIMES", append = TRUE)
              
                  }
            
              } else if(ModType == "D") {
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StartYear, q, m, 0, 0, 2400, StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], 0, sep = " ")
                      write(line, file = "EMITIMES", append = TRUE)
              
                  }
            
              } else if(ModType == "E") {
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StartYear, q, m, 0, 0, 2400, StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = "EMITIMES", append = TRUE)
              
                  }
            
              } else if(ModType == "F") {
            
                  for(j in 1:LocationInformation[i,3]) {
              
                      line <- paste(StartYear, q, m, 0, 0, 2400, StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], 0, 0, sep = " ")
                      write(line, file = "EMITIMES", append = TRUE)
              
                  }
            
              }
            
          } # This closes the conditional EMITIMES file generation

          # Run the HYSPLIT model to produce a binary file output. Convert this to an ASCII file to be used for the analysis.
          system2("./hycs_std")
          system2("./con2asc", paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""))
          
          # The con2asc appends each output ASCII file with an unwanted delimiter in the file name. That is fixed here.
          # The original binary file is also overwritten.
          file.rename(list.files(pattern = "_00", full.names = TRUE), paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""))
          
          # The single ASCII file is then appended to the large file.
          # 'temp' is a temporary object that reads in the ASCII file
          CurrentCSV <- read.csv(paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""), header = TRUE, sep = "")
          names(CurrentCSV) <- ColumnNames
          write.table(CurrentCSV, ParentFileName, append = TRUE, row.names = FALSE, col.names = FALSE)
          
          # The single ASCII file is then deleted in order to save space.
          file.remove(paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""))
          
        }     # Closes the day
      }     # Closes the Mont
    }     # Closes LocationInformation
}     # Closes ModelType



##### The section that follows is the MRS measure #####



for(d in 1:nrow(LocationInformation)) {
  
    Model2 <- as.data.frame(read.table(paste(LocationInformation[d,1], "_", "E", sep = ""), header = TRUE, sep = "")[1:5])
    Model2$DA <- Model2$DA - 1
    Model2$DA[Model2$DA == 0] <- (c-1)
  
    for(e in 1:length(ModelType)) {
        
        if(ModelType[e] != "E") {
        
            Model1 <- read.table(paste(LocationInformation[d,1], "_", ModelType[e], sep = ""), header = TRUE, sep = "")[1:5]
            Model1$DA <- Model1$DA - 1
            Model1$DA[Model1$DA == 0] <- (c-1)
            
            Metric = NULL
            
            for(f in 1:(c-1) ) {
              
                DayModel1 <- subset(Model1, DA == f)
                DayModel2 <- subset(Model2, DA == f)
                
                x_range <- max( max(DayModel1$LON), max(DayModel2$LON)) - min(min(DayModel1$LON), min(DayModel2$LON)) + 1
                y_range <- max( max(DayModel1$LAT), max(DayModel2$LAT)) - min(min(DayModel1$LAT), min(DayModel2$LAT)) + 1
                
                x_steps <- round(x_range/Resolution, 0)
                y_steps <- round(y_range/Resolution, 0)
                
                DayModel1_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
                DayModel2_Matrix <- matrix(0, nrow = y_steps, ncol = x_steps)
                
                # This section of code executes the MRS measure
                for (g in 1:y_steps) {
                  
                    for(h in 1:x_steps) {
                      
                        CellAveragedPollutant_1 <- mean(DayModel1[,5][DayModel1$LON >= min(DayModel1$LON) + Resolution*(h-1) &
                                                             DayModel1$LON < min(DayModel1$LON) + Resolution*h &
                                                             DayModel1$LAT >= min(DayModel1$LAT) + Resolution*(g-1) &
                                                             DayModel1$LAT < min(DayModel1$LAT) + Resolution*g]
                                                )
                        
                        CellAveragedPollutant_2 <- mean(DayModel2[,5][DayModel2$LON >= min(DayModel2$LON) + Resolution*(h-1) &
                                                                 DayModel2$LON < min(DayModel2$LON) + Resolution*h &
                                                                 DayModel2$LAT >= min(DayModel2$LAT) + Resolution*(g-1) &
                                                                 DayModel2$LAT < min(DayModel2$LAT) + Resolution*g]
                        )
                        
                        DayModel1_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_1), 0, CellAveragedPollutant_1)
                        DayModel2_Matrix[g,h] <- ifelse(is.nan(CellAveragedPollutant_2), 0, CellAveragedPollutant_2)
                        
                    }
                  
                }
                
                # Metric calculation is performed here (as a percentage %)
                Metric[f] <- ((100*20000*(Resolution*111000)^2)/(2*(LocationInformation[d,2]/(c-1))))*sum(abs(DayModel2_Matrix - DayModel1_Matrix))
                
                # PAIGE'S THINGS GO HERE!
                
            }
          
            # Write output file here
            write.csv(Metric, paste(LocationInformation[d,1], "_", ModelType[e], "_", StartYear, sep = ""))
            
        } else {}
      
    }
  
}



##### Plot Results Here #####



library(ggplot2)
library(reshape2)

for(k in 1:nrow(LocationInformation)) {
  
    for(l in 1:length(ModelType)) {
      
          if(ModelType[l] != "E") {
            
              eval(parse(text = paste("Plot", "_", ModelType[l], "_", StartYear, "_", LocationInformation[k,1], " <- ",
                                      "as.data.frame(read.csv(", "'", LocationInformation[k,1], "_", ModelType[l], "_", StartYear, "'", ", header = TRUE", "))", sep = "")))
      
              eval(parse(text = paste("names(", "Plot", "_", ModelType[l], "_", StartYear, "_", LocationInformation[k,1], ")", " <- ", "c('Day', 'MRS')", sep = "")))
              
          } else {}
      
    }
  
}

for(p in 1:length(ModelType)) {
    
    if(ModelType[p] != "E") {
      
        eval(parse(text = paste("Combined", "_", ModelType[p], " <- ", "data.frame(1:(c-1))", sep = "")))
        eval(parse(text = paste("colnames(", "Combined", "_", ModelType[p], ")", "[1]", " <- ", " ", "'", "Day", "'", sep = "")))
      
    } else {}
  
}


for(n in 1:nrow(LocationInformation)) {
    
    for(o in 1:length(ModelType)) {
      
        if(ModelType[o] != "E") {
          
            eval(parse(text = paste("Combined", "_", ModelType[o], "[", n + 1, "]", " <- ", "Plot", "_", ModelType[o], "_", StartYear, "_", LocationInformation[n,1], "[", 2, "]", sep = "")))
            eval(parse(text = paste("colnames(", "Combined", "_", ModelType[o], ")", "[", n + 1, "]", " <- ", " ", "'", LocationInformation[n,1], "'", sep = "")))
          
        } else {}
      
    }
  
}


for(r in 1:length(ModelType)) {
  
    if(ModelType[r] != "E") {
  
        eval(parse(text = paste("MRSmeasure", ModelType[r], " <- ", "melt(", "Combined", "_", ModelType[r], ", ", "id.vars = ", "'Day'", ", ", "variable.name = ", "'series'", ")", sep = "")))
    
        if(paste("MRSmeasure", ModelType[r], sep = "") == "MRSmeasureA") {
          
            ResultsA <- ggplot(MRSmeasureA, aes(Day, value)) + 
                          geom_line() + 
                          ylim(0, max(Combined_A[2:4])) +
                          ylab("Difference (%)") +
                          facet_grid(series ~ .) +
                          ggtitle("Sensitivity to All Parameters") +
                          theme_bw() +            
                          theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                          theme(plot.title = element_text(size = 30, face = "bold")) +
                          theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))

            ggsave("ResultsA.jpg", plot = ResultsA, scale = 1, width = 14, height = 8)
          
        } else if(paste("MRSmeasure", ModelType[r], sep = "") == "MRSmeasureB") {
          
          ResultsB <- ggplot(MRSmeasureB, aes(Day, value)) + 
                        geom_line() + 
                        ylim(0, max(Combined_B[2:4])) +
                        ylab("Metric Value (%)") +
                        facet_grid(series ~ .) +
                        ggtitle("Sensitivity to Stack Height") +
                        theme_bw() +            
                        theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                        theme(plot.title = element_text(size = 30, face = "bold")) +
                        theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
          
          ggsave("ResultsB.jpg", plot = ResultsB, scale = 1, width = 14, height = 8)
          
        } else if(paste("MRSmeasure", ModelType[r], sep = "") == "MRSmeasureC") {
          
          ResultsC <- ggplot(MRSmeasureC, aes(Day, value)) + 
                        geom_line() + 
                        ylim(0, max(Combined_C[2:4])) +
                        ylab("Metric Value (%)") +
                        facet_grid(series ~ .) +
                        ggtitle("Sensitivity to Stack Diameter") +
                        theme_bw() +            
                        theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                        theme(plot.title = element_text(size = 30, face = "bold")) +
                        theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
          
          ggsave("ResultsC.jpg", plot = ResultsC, scale = 1, width = 14, height = 8)
          
        } else if(paste("MRSmeasure", ModelType[r], sep = "") == "MRSmeasureD") {
          
          ResultsD <- ggplot(MRSmeasureD, aes(Day, value)) + 
                        geom_line() + 
                        ylim(0, max(Combined_D[2:4])) +
                        ylab("Metric Value (%)") +
                        facet_grid(series ~ .) +
                        ggtitle("Sensitivity to Exhaust Velocity") +
                        theme_bw() +            
                        theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                        theme(plot.title = element_text(size = 30, face = "bold")) +
                        theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
          
          ggsave("ResultsD.jpg", plot = ResultsD, scale = 1, width = 14, height = 8)
          
        } else if(paste("MRSmeasure", ModelType[r], sep = "") == "MRSmeasureF") {
          
          ResultsF <- ggplot(MRSmeasureF, aes(Day, value)) + 
                        geom_line() + 
                        ylim(0, max(Combined_F[2:4])) +
                        ylab("Metric Value (%)") +
                        facet_grid(series ~ .) +
                        ggtitle("Sensitivity to All Parameters (Forced 0's)") +
                        theme_bw() +            
                        theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                        theme(plot.title = element_text(size = 30, face = "bold")) +
                        theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
          
          ggsave("ResultsF.jpg", plot = ResultsF, scale = 1, width = 14, height = 8)
          
        }
        
    } else{}
  
}



##### The cleaning process begins here #####


dir.create(paste("../HYSPLIT-Results-", StartYear, sep = ""))

for(s in 1:length(ModelType)) {
    
    file.copy(paste("Results", ModelType[s], ".jpg", sep = ""), paste("../HYSPLIT-Results-", StartYear, sep = ""))
    file.remove(paste("Results", ModelType[s], ".jpg", sep = ""))
  
    for(u in 1:nrow(LocationInformation)) {
      
        file.copy(paste(LocationInformation[u,1], "_", ModelType[s], sep = ""), paste("../HYSPLIT-Results-", StartYear, sep = ""))
        file.remove(paste(LocationInformation[u,1], "_", ModelType[s], sep = ""))
        
        file.copy(paste(LocationInformation[u,1], "_", ModelType[s], "_", StartYear, sep = ""), paste("../HYSPLIT-Results-", StartYear, sep = ""))
        file.remove(paste(LocationInformation[u,1], "_", ModelType[s], "_", StartYear, sep = ""))
          
    }
  
}

print("disregard warnings above") 
setwd("..")


