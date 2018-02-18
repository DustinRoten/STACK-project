library(ggplot2)
library(grid)
library(reshape2)

PlotCol <- 1    # 1 = Column 3, 2 = Column 4, etc.

Titles <- c("Sensitivity to All Parameters",
            "Sensitivity to Stack Height",
            "Sensitivity to Stack Diameter",
            "Sensitivity to Exit Velocity",
            "Sensitivity to All Parameters (Modified)")

PlantTitles <- c("Jeffrey Energy Center", "J.S. Cooper Station", "TransAlta Centralia Station")

ModelType <- c("A", "B", "C", "D", "F")

for(i in 1:5) {

    Data_1 <- read.csv(paste("JEC_", ModelType[i],"_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    Data_2 <- read.csv(paste("JSC_", ModelType[i],"_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    Data_3 <- read.csv(paste("TCG_", ModelType[i],"_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    
    DataFrame <- as.data.frame(cbind(1:366, Data_1, Data_2, Data_3))
    names(DataFrame) <- c("Day", "Jeffrey Energy", "J.S. Cooper", "TransAlta Centralia")
    Data_Melted <- melt(DataFrame, id.vars = "Day")
    
    plot <- ggplot(Data_Melted, aes(x = Day, y = value)) +
              geom_line(aes(color = variable), size = 1.0) + 
              facet_grid(variable ~ ., scales = "free_y") +
              ylim(0, max(max(Data_1), max(Data_2), max(Data_3))) +
              ggtitle(Titles[i]) +
              ylab(expression(paste(Phi[RSM], "(%)", sep = " "))) +
              theme_bw() +
              theme(text = element_text(size=20)) +
              theme(legend.position = "none")
    
    ggsave(paste("Senstivity_", ModelType[i], ".jpg", sep = ""), plot = plot, device = "jpg",
           width = 12, height = 8, units = "in")

}

Locations <- c("JEC", "JSC", "TCG")

for(i in 1:3) {
    
    Data_1 <- read.csv(paste(Locations[i], "_A", "_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    Data_2 <- read.csv(paste(Locations[i], "_B", "_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    Data_3 <- read.csv(paste(Locations[i], "_C", "_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    Data_4 <- read.csv(paste(Locations[i], "_D", "_2012_0.1", sep = ""), header = TRUE)[,2 + PlotCol]
    
    DataFrame <- as.data.frame(cbind(1:366, Data_1, Data_2, Data_3, Data_4))
    
    names(DataFrame) <- c("Day",
                          "Sensitivity to \n All Parameters",
                          "Sensitivity to \n Stack Height",
                          "Sensitivity to \n Stack Diameter",
                          "Sensivitiy to \n Exhaust Velocity")
    
    Data_Melted <- melt(DataFrame, id.vars = "Day")
    
    plot <- ggplot(Data_Melted, aes(x = Day, y = value)) +
      geom_line(aes(color = variable), size = 1.0) + 
      facet_grid(variable ~ ., scales = "free_y") +
      ylim(0, max(max(Data_1), max(Data_2), max(Data_3))) +
      ggtitle(PlantTitles[i]) +
      ylab(expression(paste(Phi[RSM], "(%)", sep = " "))) +
      theme_bw() +
      theme(text = element_text(size=20)) +
      theme(legend.position = "none")
    
    ggsave(paste(Locations[i], "_", "Senstivity.jpg", sep = ""), plot = plot, device = "jpg",
           width = 12, height = 10, units = "in")

}
