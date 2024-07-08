
#### theme ####

library(ggplot2)

# pca

mytheme.pca <- theme(plot.title = element_text(hjust = 0.5, size = 18),
                     axis.text = element_text(size = 18),
                     axis.title = element_text(size = 18,
                                               color = "black"),
                     panel.background = element_blank(),
                     panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(),
                     axis.line = element_line(color = "black"),
                     legend.key = element_rect(fill = "transparent"),
                     legend.title = element_text(face = "bold", size = 16),
                     legend.text = element_text(face = c(rep("italic", 2)), size = 16))

# nmds

mytheme.nmds <- theme(plot.title = element_text(hjust = 0.5, size = 18),
                      axis.text = element_text(size = 18),
                      axis.title = element_text(size = 18,
                                                color = "black"),
                      panel.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      axis.line = element_line(color = "black"),
                      legend.key = element_rect( fill = "transparent", color = "black"),
                      legend.title = element_text( face = "bold", size = 16),
                      legend.text = element_text(face = c(rep("italic", 2)), size = 16))
