# Generating images below
xlabs <- c("Shift (Degrees)", "Rotation (Degrees)", "Dilation Factor", "Dilation Factor")

for (i in 1:4) {
    eval(parse(text = paste()))
}
  
for (i in 1:4) {
  for (j in 1:4) {
    
    p <- ggplot(data = cat(Names_Scenarios[i]), aes(x = c(1:nrow(Metrics[i])), y = Metrics[j])) +
      geom_line() +
      xlab(xlabs[i]) +
      ylab("") +
      ylim(0, max(max(1.01, '*', Metrics[1], "$", Names_Metrics[j], sep = ""))),
      max(1.01*cat(Metrics[2], "$", Names_Metrics[j], sep = "")),
      max(1.01*cat(Metrics[3], "$", Names_Metrics[j], sep = "")),
      max(1.01*cat(Metrics[4], "$", Names_Metrics[j], sep = ""))
        )) +
      theme_bw() +
      theme(strip.text.y = element_text(size = 30, colour = "black", face = "bold", angle = -90)) +
      theme(plot.title = element_text(size = 40, face = "bold")) +
      theme(axis.text=element_text(size=40), axis.title=element_text(size=40,face="bold")) +
      theme(axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 05))) +
      theme(plot.margin=unit(c(0.4,0.4,0.4,0.4),"cm"))

      ggsave(paste(Metrics[i], "-", Names_Metrics[j]), p, device = "jpg", width = 10, height = 8, units = "in")
}}
