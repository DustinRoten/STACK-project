source("TEST-DEMOFunctions.R")
library(reshape)
library(ggplot2)

Dispersion <- read.delim("JEC-10000m2.txt", header = TRUE, sep = "")

Gridded_Dispersion <- GridDispersions2(Dispersion, Dispersion, 1, 1)

Melted_Dispersion <- melt(Gridded_Dispersion)

Melted_Dispersion <- subset(Melted_Dispersion, Melted_Dispersion$value != 0)

Origin <- subset(Melted_Dispersion, Melted_Dispersion$value == max(Melted_Dispersion$value))

# Plot One
P1 <- ggplot() +
  scale_fill_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                         values = c(0, 0.0012, 0.002, 0.004, 0.009, 1), na.value = "NA") +
  geom_tile(data = Melted_Dispersion, aes(x = X2, y = X1, fill = value)) +
  geom_point(data=Origin, aes(x=X1, y=X2, shape = 19), size = 12) +
  scale_shape_identity() +
  theme_bw() +
  xlab("Matrix Index (i-x)") +
  ylab("Matrix Index (j-y)") +
  ggtitle("Plotted Matrix") +
  theme(legend.position = "none") +
  theme(text = element_text(size=28))

# Plot Two
Sub_Melted_Dispersion <- subset(Melted_Dispersion, Melted_Dispersion$value != max(Melted_Dispersion$value))

P2 <- ggplot(data = Melted_Dispersion, aes(x = X2, y = X1, fill = value)) +
  scale_fill_gradientn(colours = c("green", "darkgreen", "yellow", "darkorange", "orange", "red"),
                       values = c(0, 0.0012, 0.002, 0.004, 0.009, 1), na.value = "NA") +
  geom_tile() +
  geom_segment(aes(y = Melted_Dispersion$X1[Melted_Dispersion$value == max(Melted_Dispersion$value)],
                   x = Melted_Dispersion$X2[Melted_Dispersion$value == max(Melted_Dispersion$value)],
                   xend = X2, yend = X1, colour = value), arrow = arrow(length = unit(1,"cm")),
              size = 3, color = "black",
              data = Sub_Melted_Dispersion) +
  geom_point(data=Origin, aes(x=X1, y=X2, shape = 19), size = 12) +
  scale_shape_identity() +
  theme_bw() +
  xlab("Matrix Index (i-x)") +
  ylab("Matrix Index (j-y)") +
  ggtitle("Elements as Vectors") +
  theme(legend.position = "none") +
  theme(text = element_text(size=28))

print(P1)
print(P2)
