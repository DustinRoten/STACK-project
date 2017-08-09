#Loop HYSPLIT for an entire year!- 24hr Dispersions (2012)
#Dustin Roten

ModelType <- "A"      #A - EPA data used (only), B - 0m Stack Height, C - 0m/s Emissions, D - 0m^2 area, E - Full Model

MonthNumber <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
MonthName <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
DaysInMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30) #One less day in December
EDAS1 <- c("edas.jan12.001", "edas.feb12.001", "edas.mar12.001", "edas.apr12.001", "edas.may12.001", "edas.jun12.001",
           "edas.jul12.001", "edas.aug12.001", "edas.sep12.001", "edas.oct12.001", "edas.nov12.001", "edas.dec12.001")
EDAS2 <- c("edas.jan12.002", "edas.feb12.002", "edas.mar12.002", "edas.apr12.002", "edas.may12.002", "edas.jun12.002",
           "edas.jul12.002", "edas.aug12.002", "edas.sep12.002", "edas.oct12.002", "edas.nov12.002", "edas.dec12.002")
MonthMatrix <- data.frame(MonthNumber, MonthName, DaysInMonth, EDAS1, EDAS2)

for(q in 1:12) {

for (j in 1:MonthMatrix[q,3]) {

StartYear <- 2012
StartTime <- c(12, q, j, 00)     #Input model's start date and time (YY MM DD HH)

NumOfStartLocs <- 1           #Number of starting locations
StartLocInfo1 <- c(39.2865, -96.1172, 0, 1433462.63) #Starting Location #1 Parameters (lat, lon, height(AGL), emission rate(mass/h), area(m^2), heat(W))
# StartLocInfo2 <- c(39.28681, -96.11721, 174.96, 557748.97, 47.17, 16788214.8) #Starting Location #2 Parameters (lat, lon, height(AGL), emission rate(mass/h), area(m^2), heat(W))
# StartLocInfo3 <- c(39.28681, -96.11618, 174.96, 568829.78, 47.17, 16591050.0) #Starting Location #3 Parameters (lat, lon, height(AGL), emission rate(mass/h), area(m^2), heat(W))

TotRunTime <- 24  #Total run time (hr)
VertMot <- 0      #Method of vertical motion
TopLvl <- 20000.0 #Upper level of the model

PolNum <- 1                       #Number of pollutant species
NameTemp <- "CO2"                 #Name of pollutant species
PolRat <- 1433462.63              #Emission rate (mass/hr)
PolDur <- 24                      #Pollutant duration (hr)
RelStart <- c(12, q, j, 00, 00)   #Pollutant start (YY, MM, DD, HH, MM)

CenterLatLon <- c(39.2865, -96.1172)        #Center the display grid (lat, lon)
Spacing <- c(0.05, 0.05)                      #Resolution of the display grid (lat, lon)
Span <- c(80.0, 80.0)                         #size of the display grid (lat, lon)
OutputDir <- "./"                             #Output directory
OutputName <- paste(ModelType, StartTime[1], StartTime[2], StartTime[3], StartTime[4], sep = "-", collapse = NULL) #output name (YY-MM-DD-HH)

Layers <- c(1, 20000)     #Vertical levels, top of model

SampleStart <- c(12, q, j, 00, 00)        #Start Sample Date (YY MM DD HH MM)
SampleStop <- c(12, (if (j <= (MonthMatrix[q,3]-1)) {q} else {q+1}), (if (j <= (MonthMatrix[q,3]-1)) {j+1} else {1}), 00, 00) #Stop Sample Date (YY MM DD HH MM)
Method <- c(00, 24, 00)    #Method (XX HH MM)

ChemParams1 <- c(0.000257, 0.00197, 1.0)      #Particle Diameter (um), Density (g/cc), Shape
ChemParams2 <- c(0.0, 44.01, 0.0, 0.0, 0.0)     #Deposition Velocity (m/s), Mol Wgt (g), A-Ratio, D-Ratio, Henry
ChemParams3 <- c(101.325, 0.0, 0.0)           #Henry's Constant (M/a), In-cloud (l/l), Below-cloud (1/s)
ChemParams4 <- c(0.0, 0.0)                    #Radioactive Decay - Halflife (days), Pollutant Resuspension Factor (1/m)

#Feed in meteorology data: Number of met files and file paths
EDASpath <- "C:/hysplit4/working/"

#Selecting MET Data
MetData = NULL

# #EMITIMES file generation
# cat(paste("YYYY MM DD HH   DURATION(hhhh) #RECORDS","\n",
#           "YYYY MM DD HH MM DURATION(hhmm) LAT LON HGT(m) RATE(/h) AREA(m2) HEAT(w)"), "\n",
#     paste(StartYear),
#     paste(StartTime[2:4], sep = " "),
#     paste("9999"),
#     paste(NumOfStartLocs, sep = " ", collapse = " "), "\n",
#     
#     paste(StartYear),
#     paste(SampleStart[2:5], sep = " "),
#     paste(PolDur,"00", sep = ""),
#     paste(StartLocInfo1, sep = " ", collapse = " "), "\n",
#     
#     paste(StartYear),
#     paste(SampleStart[2:5], sep = " "),
#     paste(PolDur,"00", sep = ""),
#     paste(StartLocInfo2, sep = " ", collapse = " "), "\n",
#     
#     paste(StartYear),
#     paste(SampleStart[2:5], sep = " "),
#     paste(PolDur,"00", sep = ""),
#     paste(StartLocInfo3, sep = " ", collapse = " "),
#     
#     file = "EMITIMES")

cat(paste(StartTime, collapse = " "), "\n",
    NumOfStartLocs, "\n",
    paste(StartLocInfo1, collapse = " "), "\n",
   # paste(StartLocInfo2, collapse = " "), "\n",
   # paste(StartLocInfo3, collapse = " "), "\n",
    TotRunTime, "\n",
    VertMot, "\n",
    TopLvl, "\n",
    "2", "\n",                                #The 2 is hardcoded for now
    EDASpath, "\n",
    paste(MonthMatrix[q,4], collapse = "\n"), "\n",
    EDASpath, "\n",
    paste(MonthMatrix[q,5], collapse = "\n"), "\n",
    PolNum, "\n",
    NameTemp, "\n",
    PolRat, "\n",
    PolDur, "\n",
    paste(RelStart, collapse = " "), "\n",
    GridNum <- PolNum, "\n",
    paste(CenterLatLon, collapse = " "), "\n",
    paste(Spacing, collapse = " "), "\n",
    paste(Span, collapse = " "), "\n",
    paste(OutputDir, collapse = " "), "\n",
    paste(OutputName, collapse = " "), "\n",
    paste(Layers, collapse = "\n"), "\n",
    paste(SampleStart, collapse = " "), "\n",
    paste(SampleStop, collapse = " "), "\n",
    paste(Method, collapse = " "), "\n",
    (DepoNum <- PolNum), "\n",
    paste(ChemParams1, collapse = " "), "\n",
    paste(ChemParams2, collapse = " "), "\n",
    paste(ChemParams3, collapse = " "), "\n",
    paste(ChemParams4, collapse = "\n"), "\n",
sep='', file = "CONTROL")

system("hycs_std.exe")
system(paste("con2asc.exe", OutputName, collapse = " "))
file.remove(OutputName)

FolderName <- paste(ModelType,"-", MonthMatrix[q,2], sep = "", collapse = "")

if (dir.exists(file.path(getwd(), FolderName)) == TRUE) {
  paste(ModelType,"-",MonthMatrix[q,2], sep = "", collapse = "")
}

else {
  dir.create((file.path(getwd(), FolderName)))
}

ModOutputName <- paste(OutputName, 
                       if ((sum(MonthMatrix$DaysInMonth[1:q])-(MonthMatrix[q,3]-j)+1) <= 9) {"_00"}
                       else if ((sum(MonthMatrix$DaysInMonth[1:q])-(MonthMatrix[q,3]-j)+1) <= 99) {"_0"}
                       else {"_"}, (sum(MonthMatrix$DaysInMonth[1:q])-(MonthMatrix[q,3]-j)+1)      
                       , "_00", sep = "", collapse = "")

FinalDestination <- paste(ModelType,"-", MonthMatrix[q,2], sep = "", collapse = "")

file.rename(paste(getwd(), "/", ModOutputName, sep = "", collapse = ""), 
          paste(getwd(), "/", FinalDestination, "/", OutputName, sep = "", collapse = ""))

}
  
}


