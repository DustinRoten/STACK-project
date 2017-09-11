library(ggplot2)
library(scales)
library(reshape2)
library(ggmap)

Angular <- read.csv("AngularMetrics", header = TRUE)
Dilation <- read.csv("DilationMetrics", header = TRUE)
Shift <- read.csv("ShiftMetrics", header = TRUE)

########## MRS ##########

# MRS - Angular
ggplot(data = Angular, aes(x = theta, y = MRS)) +
  geom_line() +
  xlab("Angle of Rotation") +
  ylab("") +
  scale_x_continuous(breaks = c(0, (1/2), 1, (3/2), 2), labels = expression(0, pi/2, pi, 3*pi/2, 2*pi)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# MRS - Dilation
ggplot(data = Dilation, aes(x = theta, y = MRS)) +
  geom_line() +
  theme_bw() +
  xlab("Scaling Factor") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# MRS - Shift
ggplot(data = Shift, aes(x = theta, y = MRS)) +
  geom_line() +
  theme_bw() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))



########## Delta Theta ##########

# Theta - Angular
ggplot(data = Angular, aes(x = theta, y = MeanAngle)) +
  geom_line() +
  xlab("Angle of Rotation") +
  ylab("") +
  scale_x_continuous(breaks = c(0, (1/2), 1, (3/2), 2), labels = expression(0, pi/2, pi, 3*pi/2, 2*pi)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# Theta - Dilation
ggplot(data = Dilation, aes(x = theta, y = MeanAngle)) +
  geom_line() +
  theme_bw() +
  xlab("Scaling Factor") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# Theta - Shift
ggplot(data = Shift, aes(x = theta, y = MeanAngle)) +
  geom_line() +
  theme_bw() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))


########## Delta Sigma ##########

# Sigma - Angular
ggplot(data = Angular, aes(x = theta, y = StdAngle)) +
  geom_line() +
  xlab("Angle of Rotation") +
  ylab("") +
  scale_x_continuous(breaks = c(0, (1/2), 1, (3/2), 2), labels = expression(0, pi/2, pi, 3*pi/2, 2*pi)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# Sigma - Dilation
ggplot(data = Dilation, aes(x = theta, y = StdAngle)) +
  geom_line() +
  theme_bw() +
  xlab("Scaling Factor") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# Sigma - Shift
ggplot(data = Shift, aes(x = theta, y = StdAngle)) +
  geom_line() +
  theme_bw() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))


########## Delta COM ##########

# COM - Angular
ggplot(data = Angular, aes(x = theta, y = COM)) +
  geom_line() +
  xlab("Angle of Rotation") +
  ylab("") +
  scale_x_continuous(breaks = c(0, (1/2), 1, (3/2), 2), labels = expression(0, pi/2, pi, 3*pi/2, 2*pi)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# COM - Dilation
ggplot(data = Dilation, aes(x = theta, y = COM)) +
  geom_line() +
  theme_bw() +
  xlab("Scaling Factor") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

# COM - Shift
ggplot(data = Shift, aes(x = theta, y = COM)) +
  geom_line() +
  theme_bw() +
  xlab("Horizontal Shift (Degrees)") +
  ylab("") +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold", angle = -90)) +
  theme(plot.title = element_text(size = 30, face = "bold")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25,face="bold")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))
