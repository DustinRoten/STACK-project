library(ggplot2)
library(reshape2)

MRSmeasureA <- as.data.frame(read.csv("MRSmeasureA"))
MRSmeasureB <- as.data.frame(read.csv("MRSmeasureB"))
MRSmeasureC <- as.data.frame(read.csv("MRSmeasureC"))
MRSmeasureD <- as.data.frame(read.csv("MRSmeasureD"))

names(MRSmeasureA) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia Generating")
names(MRSmeasureB) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia Generating")
names(MRSmeasureC) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia Generating")
names(MRSmeasureD) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia Generating")

MRSmeasureA[2] <- 100*MRSmeasureA[2]
MRSmeasureA[3] <- 100*MRSmeasureA[3]
MRSmeasureA[4] <- 100*MRSmeasureA[4]

MRSmeasureB[2] <- 100*MRSmeasureB[2]
MRSmeasureB[3] <- 100*MRSmeasureB[3]
MRSmeasureB[4] <- 100*MRSmeasureB[4]

MRSmeasureC[2] <- 100*MRSmeasureC[2]
MRSmeasureC[3] <- 100*MRSmeasureC[3]
MRSmeasureC[4] <- 100*MRSmeasureC[4]

MRSmeasureD[2] <- 100*MRSmeasureD[2]
MRSmeasureD[3] <- 100*MRSmeasureD[3]
MRSmeasureD[4] <- 100*MRSmeasureD[4]

MRSmeasure1 <- melt(MRSmeasureA ,  id.vars = 'Day', variable.name = 'series')
MRSmeasure2 <- melt(MRSmeasureB ,  id.vars = 'Day', variable.name = 'series')
MRSmeasure3 <- melt(MRSmeasureC ,  id.vars = 'Day', variable.name = 'series')
MRSmeasure4 <- melt(MRSmeasureD ,  id.vars = 'Day', variable.name = 'series')


################## Additional Analyses #####################

ScaledMax <- 1.05*max(na.omit(MRSmeasureA[,-1]), na.omit(MRSmeasureB[,-1]), na.omit(MRSmeasureC[,-1]), na.omit(MRSmeasureD[,-1]))

ggplot(MRSmeasure1, aes(Day, value)) + 
  geom_line() + 
  ylim(0, ScaledMax) +
  ylab("Metric Value (%)") +
  facet_grid(series ~ .) +
  ggtitle("Sensitivity to All Parameters") +
  theme_bw()

ggplot(MRSmeasure2, aes(Day, value)) + 
  geom_line() + 
  ylim(0, ScaledMax) +
  ylab("Metric Value (%)") +
  facet_grid(series ~ .) +
  ggtitle("Sensitivity to Stack Height") +
  theme_bw()

ggplot(MRSmeasure3, aes(Day, value)) + 
  geom_line() + 
  ylim(0, ScaledMax) +
  ylab("Metric Value (%)") +
  facet_grid(series ~ .) +
  ggtitle("Sensitivity to Stack Diameter") +
  theme_bw()

ggplot(MRSmeasure4, aes(Day, value)) + 
  geom_line() + 
  ylim(0, ScaledMax) +
  ylab("Metric Value (%)") +
  facet_grid(series ~ .) +
  ggtitle("Sensitivity to Exhaust Velocity") +
  theme_bw()