#Plotting
library(reshape2)
library(ggplot2)

for (y in 1:length(ModelType)) {
  
    if (ModelType[y] != "E") {
  
        CombinedMRS <- NULL
        CombinedMeanAngle <- NULL
        CombinedVarAngle <- NULL
        CombinedCOM <- NULL
      
        for (x in 1:nrow(LocationInformation)) {

            ToBeCombined <- read.csv(paste(LocationInformation[x,1], "_", ModelType[y], "_", StartYear, "_", Resolution, sep = ""))
        
            CombinedMRS <- as.data.frame(cbind(CombinedMRS, ToBeCombined$MRS))
            names(CombinedMRS)[x] <- paste(LocationInformation[x,1])
            
            CombinedMeanAngle <- as.data.frame(cbind(CombinedMeanAngle, ToBeCombined$MeanAngle))
            names(CombinedMeanAngle)[x] <- paste(LocationInformation[x,1])
            
            CombinedVarAngle <- as.data.frame(cbind(CombinedVarAngle, ToBeCombined$VarAngle))
            names(CombinedVarAngle)[x] <- paste(LocationInformation[x,1])
            
            CombinedCOM <- as.data.frame(cbind(CombinedCOM, ToBeCombined$CenterOfMass))
            names(CombinedCOM)[x] <- paste(LocationInformation[x,1])
        
        }
        
        write.csv(CombinedMRS, paste("CombinedMRS", "_", StartYear, "_", ModelType[y], sep = ""))
        write.csv(CombinedMeanAngle, paste("CombinedMean", "_", StartYear, "_", ModelType[y], sep = ""))
        write.csv(CombinedVarAngle, paste("CombinedVar", "_", StartYear, "_", ModelType[y], sep = ""))
        write.csv(CombinedCOM, paste("CombinedCOM", "_", StartYear, "_", ModelType[y], sep = ""))
        
    } else {}

}


ModelStatements <- c("All Parameters", "Stack Height", "Stack Diameter", "Exit Velocity", "ERROR!", "All Parameters (Forced 0's)")
CombinedFileNames <- c("MRS", "Mean", "Var", "COM")
#Metric Units?


for (j in 1:length(CombinedFileNames)) {

    for (i in 1:length(ModelType)) {

        if (ModelType[i] != "E") {
      
            ToBePlotted <- read.csv(paste("Combined", CombinedFileNames[j], "_", StartYear, "_", ModelType[i], sep = ""))
            names(ToBePlotted)[1] <- "Day"

            ToBePlotted <- melt(ToBePlotted, id.vars = "Day", variable.name = "series")

            SavePlot <- ggplot(data = ToBePlotted, aes(x = Day, y = value)) +
                          geom_line() +
                          ylim(0, max(ToBePlotted$value) + 1) +
                          ylab("Metric (%)") +
                          facet_grid(series ~ .) +
                          ggtitle(paste("Sensitivity to ", ModelStatements[i], " ", "(", Resolution, " ", "Degree Resolution", ")", sep = "")) +
                          theme_bw() +
                          theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
                          theme(plot.title = element_text(size = 30, face = "bold")) +
                          theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold"))
            
            ggsave(paste("Results", ModelType[i], "_", CombinedFileNames[j], ".jpg", sep = ""), plot = SavePlot, scale = 1, width = 14, height = 8)
            
        } else {}
    }
}
