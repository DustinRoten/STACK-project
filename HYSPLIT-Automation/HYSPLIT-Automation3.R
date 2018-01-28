# -----------------------------------
# HYSPLIT - FullAutomation (3)
# Year > 2000
# Dustin Roten - Jan. 2018
# -----------------------------------

SystemType <- Sys.info()[1]

# User Alerts
Alerts <- TRUE
if(Alerts == TRUE) {do.call(file.remove, list(list.files("~/Google Drive/RAutomation/", full.names = TRUE)))} else {}

# Install functions
if(require(geosphere) == FALSE) {
    install.packages("geosphere")
    library(geosphere)
} else {require(geosphere)}

if(require(ggplot2) == FALSE) {
    install.packages("ggplot2")
    library(ggplot2)
} else {require(ggplot2)}

if(require(reshape2) == FALSE) {
    install.packages("reshape2")
    library(reshape2)
} else {require(reshape2)}

# Load custom functions
source("./SystemFiles/Custom_R_Functions.R")

# Ask user for previous run
PreviousRun <- readline(prompt = "Load variables from a previous run? (Y/N) - ")
if(PreviousRun == "Y" | PreviousRun == "y" | PreviousRun == "Yes" | PreviousRun == "yes" | PreviousRun == "YES") {
  
    VariablePath <- readline(prompt = "Provide the file path - ")
    
    if(file.exists(VariablePath)) {
        print(paste("Using", VariablePath, sep = " "))
    } else {
        print("File does not exist. Exiting script now.")
        break
    }
    
  
} else {
  
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
    
    }
  
    save.image(file = paste("HYSPLIT_Vars_", Sys.Date(), sep = ""), version = NULL, ascii = FALSE)
  
}

# If a previous object file exists, the script will start from here.
load(VariablePath)

# User information completed. The following organizes the pollutant's parameters
ChemicalParameters1 <- as.numeric(c(ParticleDiameter, ParticleDensity, 1))
ChemicalParameters2 <- as.numeric(c(ParticleDepoVelocity, ParticleMolecularWeight, ParticleARatio, ParticleDRatio, ParticleHenry))
ChemicalParameters3 <- as.numeric(c(ParticleHenryConstant, ParticleInCloud, ParticleBelowCloud))
ChemicalParameters4 <- as.numeric(ParticleRadioactive)
ChemicalParameters5 <- as.numeric(ParticleResuspensionFactor)


# Constructing the time management dataframe
MonthNames <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
DaysInMonth <- c(31, if (StartYear %% 4 == 0) {29} else{28}, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
MonthData <- data.frame(MonthNames, DaysInMonth)


# Constructing a vector of meteorology file names (NAM-12km)
MeteorologyFileNames <- NULL
c <- 1
for(a in 1:12) {
  
  for(b in 1:MonthData[a,2]) {
    
    MeteorologyFileNames[c] <- paste(StartYear, if(a <= 9) {"0"} else {}, a, if(b <= 9) {"0"} else {}, b, "_nam12", sep = "")
    c <- c + 1
    
  }
}

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
for(z in 1:length(ModelType)) {     # Begins the "Model Type" loop
  
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
          
          if(file.exists("SETUP.CFG") == TRUE) {file.rename("SETUP.CFG", "NO_SETUP.CFG")} else {}
          
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
        if(SystemType == "Windows") {
            system2("./hycs_std.exe")
            system2("./con2asc.exe", paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""))
        } else {
          system2("./hycs_std")
          system2("./con2asc", paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""))
        }
        
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
    }     # Closes the Month
    
    if(Alerts == TRUE) {
        write.table(Sys.time(), file = paste("RawData_", LocationInformation[i,1], "_", ModType, ".tsv", sep = ""), col.names = FALSE, row.names = FALSE)
        file.rename(from = paste("RawData_", LocationInformation[i,1], "_", ModType, ".tsv", sep = ""),
                    to = paste("~/Google Drive/RAutomation/RawData_", LocationInformation[i,1], "_", ModType, ".tsv", sep = ""))
    } else{}
    
  }     # Closes LocationInformation
}     # Closes ModelType