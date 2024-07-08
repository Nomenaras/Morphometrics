### ratio ###

ratio <- raw %>% 
  mutate(rPaH= PaH/PaH, rPaL = PaL/PaH, rAeH = AeH/PaH, rAeL = AeL/PaH,
         rVcL = VcL/PaH, rVcaH = VcaH/PaH, rSeL = SeL/PaH, rVuL = VuL/PaH,
         rTeH = TeH/PaH, rVoL = VoL/PaH)


ratios <- ratio[,c(1,16:25)]

rat_sp2 <- ratio[,c(1, 15:25)]

colnames(rat_sp2) <-  c("Genera", "sp_id","PaH","PaL", "AeH","AeL", "VcL", "VcaH" ,"SeL", "VuL", "TeH", "VoL") 


#### exported data ####

### Ratio ###

write.csv(rat_sp2, file = "/Users/antman/Documents/ratio.csv", row.names = FALSE)
  
##### CIT's ####

# ratio

library(dplyr)

library(ggplot2)

library(partykit)

rt_cit <- rat_sp2[,-c(1)]

rt_cit$Genera <- as.factor(rt_cit$Genera)

rt_ConInfTree <- ctree(Genera~., 
                       data = rt_cit)
str(rt_cit)

# Print model

print(rt_ConInfTree)

png(file = "conditionalClassification_raw.png",
    width = 1200, height = 400)

# Plotting graph

plot(rt_ConInfTree)


#### PCA ####

library(factoextra)

library(FactoMineR)

library(ggplot2)

# ratio
rat_sp2 <- rat_sp2[,-c (2)] #remove sp_id
rt_pca <- PCA(rat_sp2[,-1])


# plot individuals PCA 

#shape_final

order_sp <- c("Mohan","Momad", "Mopha", "Modrm01", "Moter02", "Moter03", "Moter03b", "MoMG01", "MoMG02", "MoMG03",
              "Syfis", "Symod", "Syhil01", "Syhil02", "SyMG01")

# shape

shp_val = c( 22, 15, 18, 17, 10, 16, 20, 8, 3, 4, 15, 17, 10, 16, 8)

rat_pca <- fviz_pca_ind(rt_pca, axes.linetype = NA, geom = "none",
                        repel = FALSE, legend.title = "Genera", labelsize = 4)+ 
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = rat_sp2$Genera, shape = ratio$sp_id), size = 3)+
  coord_fixed(ratio = 1)+
  labs(title = NULL, shape = NULL)+ xlab ("PC1 (47.62%)")+ ylab ("PC2 (24.71%)")+
  mytheme.pca

ggsave("/Users/antman/Documents/rat_pca.pdf", plot = rat_pca , width = 20, height = 15, unit = "cm")


#### Ward ####

library(dendextend)

library(factoextra)

### Add species name 

str(rat_sp2)

as.matrix(rat_sp2)

rat_sp2 <- as.matrix(rat_sp2)

rownames(rat_sp2) <- raw$sp_id

as.data.frame(rat_sp2)

rt_dat <- rat_sp2[,-c(1:2)]

as.data.frame(rt_dat)

str(rt_dat)

# single linkage

D.Hel_rt2 = dist(rt_dat, method = "euclidean",  diag = FALSE, upper = FALSE, p = 2)

# ward

C.Ward_rt2 = hclust(D.Hel_rt2^2,method = 'ward') 

C.Ward_rt2$height = sqrt(C.Ward_rt2$height)

plot(C.Ward_rt2)

dend_rt2 <- as.dendrogram(C.Ward_rt2)

col_rt2 <- ifelse(grepl("Mo", labels(dend_rt2)), "#134b73", "#e41a1c") # Monomorium

pre_fig_rt2 <- assign_values_to_leaves_edgePar(dend = dend_rt2 , value = col_rt2, edgePar = "col") 

# plot

ward_rt2 <- fviz_dend(pre_fig_rt2, cex = 0.5, show_labels = T, horiz = TRUE,
                      main = NULL)

ggsave("/Users/antman/Documents/ward.pdf", plot = ward_rt2, width = 25, height = 20, unit = "cm")



#### NMDS ####
# ratio

library(vegan) 

library(ggplot2)

str(rat_sp2)
rat_nmds <- rat_sp2[,-1]

data(rat_nmds)

rat_nmds1 <- metaMDS(rat_nmds, k = 2, distance = "euclidean", autotransform = F)

rat_nmds1

stressplot(rat_nmds1)

rat_dat.scores <- as.data.frame(scores(rat_nmds1)$sites)

rat_dat.scores$Genera <- rat_sp2$Genera

colnames(rat_dat.scores)[3] <- c("Genera")

head(rat_dat.scores)

rat_plot_nmds <- ggplot(rat_dat.scores, aes(x = NMDS1, y = NMDS2, group = Genera),
                        repel = FALSE, legend.title = "Genera", labelsize = 6)+
  scale_shape_manual(values = shp_val, breaks = order_sp)+
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = Genera, shape = ratio$sp_id), size = 3)+
  coord_fixed(ratio = 1.69)+
  labs(title = NULL, shape = NULL) +
  mytheme.nmds


ggsave("/Users/antman/Documents/rat_nmds.pdf", plot = rat_plot_nmds , width = 20, height = 15, unit = "cm")


#### LDA ####

library(dplyr)

library(MASS)

library(caret)

# ratio

rat_ld <- rat_sp2[,-c(1,3)]

str(rat_ld)

# Split the data into training and test

set.seed(123)

rt_training.samples <-  rat_ld$sp_id %>%
  createDataPartition(p = 1, list = FALSE)  # change (80, 60, 100)

rt_training <- rat_ld[rt_training.samples, ]

rt_testing <- rat_ld[-rt_training.samples, ]

str(rt_training)

# Fit the model
rt_model <- lda(sp_id~., data = rt_training)

summary(rt_model)

plot(rt_model)

p <- predict(rt_model, rt_training)

lda.data1 <- cbind(rt_training, p$x)

rat_plot_lda <- ggplot(lda.data1, aes(LD1, LD2)) +
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = rat_sp2$Genera, shape = ratio$sp_id), size = 3)+
  coord_fixed(ratio = 1.75)+
  labs(title = NULL, colour = "Genera", shape = NULL, labelsize = 6)+
  mytheme.nmds

ggsave("/Users/antman/Documents/rat_lda.pdf", plot = rat_plot_lda , width = 20, height = 15, unit = "cm")

