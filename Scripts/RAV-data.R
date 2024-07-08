### RAV ### 

rv_data <- raw

str(raw)
# added mean PaH

rv <- raw %>% 
  mutate(mPaH = mean(PaH), rPaH= PaH/PaH, rPaL = PaL/PaH, rAeH = AeH/PaH, rAeL = AeL/PaH,
         rVcL = VcL/PaH, rVcaH = VcaH/PaH, rSeL = SeL/PaH, rVuL = VuL/PaH,
         rTeH = TeH/PaH, rVoL = VoL/PaH)

# variable (1: PaL, etc...)
#PaL
rv1  <-  lm(rPaL ~ PaH, data = rv)

rv1$coefficients[1]

rv_data$b_PaL <- rv1$coefficients[1]
rv_data$a_PaL <- rv1$coefficients[2]

#AeH
rv2  <-  lm(rAeH ~ PaH, data = rv)

rv2$coefficients[1]

rv_data$b_AeH <- rv2$coefficients[1]
rv_data$a_AeH <- rv2$coefficients[2]

#AeL
rv3  <-  lm(rAeL ~ PaH, data = rv)

rv3$coefficients[1]

rv_data$b_AeL <- rv3$coefficients[1]
rv_data$a_AeL <- rv3$coefficients[2]

#VcL
rv4  <-  lm(rVcL ~ PaH, data = rv)

rv4$coefficients[1]

rv_data$b_VcL <- rv4$coefficients[1]
rv_data$a_VcL <- rv4$coefficients[2]

#VcaH
rv6  <-  lm(rVcaH ~ PaH, data = rv)

rv6$coefficients[1]

rv_data$b_VcaH <- rv6$coefficients[1]
rv_data$a_VcaH <- rv6$coefficients[2]

#SeL
rv7  <-  lm(rSeL ~ PaH, data = rv)

rv7$coefficients[1]

rv_data$b_SeL <- rv7$coefficients[1]
rv_data$a_SeL <- rv7$coefficients[2]

#VuL
rv8  <-  lm(rVuL ~ PaH, data = rv)

rv8$coefficients[1]

rv_data$b_VuL <- rv8$coefficients[1]
rv_data$a_VuL <- rv8$coefficients[2]

#TeH
rv9  <-  lm(rTeH ~ PaH, data = rv)

rv9$coefficients[1]

rv_data$b_TeH <- rv9$coefficients[1]
rv_data$a_TeH <- rv9$coefficients[2]

#VoL
rv10  <-  lm(rVoL ~ PaH, data = rv)

rv10$coefficients[1]

rv_data$b_VoL <- rv10$coefficients[1]
rv_data$a_VoL <- rv10$coefficients[2]

#PaH
rv11  <-  lm(rPaH ~ PaH, data = rv)

rv11$coefficients[2]

rv_data$b_PaH <- rv11$coefficients[1]
rv_data$a_PaH <- rv11$coefficients[2]

# update

rv <- rv_data %>% 
  mutate(mPaH = mean(PaH), rPaH= PaH/PaH, rPaL = PaL/PaH, rAeH = AeH/PaH, rAeL = AeL/PaH,
         rVcL = VcL/PaH, rVcaH = VcaH/PaH, rSeL = SeL/PaH, rVuL = VuL/PaH, rTeH = TeH/PaH, rVoL = VoL/PaH )

view(rv)

# Division_rav

drv_data <- rv %>% 
  mutate(ravpal = ((a_PaL * PaH + b_PaL)*(a_PaL * mPaH + b_PaL)), ravpah = ((a_PaH * PaH + b_PaH)*(a_PaH * mPaH + b_PaH)), 
         ravael = ((a_AeL * PaH + b_AeL)*(a_AeL * mPaH + b_AeL)), ravaeh = ((a_AeH * PaH + b_AeH)*(a_AeH * mPaH + b_AeH)), 
         ravvcl = ((a_VcL * PaH + b_VcL)*(a_VcL * mPaH + b_VcL)), ravvcah = ((a_VcaH * PaH + b_VcaH)*(a_VcaH * mPaH + b_VcaH)),
         ravsel = ((a_SeL * PaH + b_SeL)*(a_SeL * mPaH + b_SeL)),ravvul = ((a_VuL * PaH + b_VuL)*(a_VuL * mPaH + b_VuL)), 
         ravteh = ((a_TeH * PaH + b_TeH)*(a_TeH * mPaH + b_TeH)), 
         ravvol = ((a_VoL * PaH + b_VoL)*(a_VoL * mPaH + b_VoL)))


# mixed data
rv_dat <- drv_data %>%
  mutate(newPaH = rPaH/ ravpah, newPaL = rPaL/ ravpal, newAeH = rAeH/ ravaeh, 
         newAeL = rAeL/ ravael, newVcL = rVcL/ ravvcl, newVcaH = rVcaH/ ravvcah, 
         newSeL = rSeL/ ravsel, newVuL = rVuL/ ravvul, newTeH = rTeH/ ravteh, 
         newVoL = rVoL/ ravvol)

str(rv_dat)

rav <- rv_dat[,c(1, 15, 57:66)] 


rav_sp2 <- rv_dat[,c(1, 15, 57:66)]

colnames(rav_sp2) <-  c("Genera", "sp_id","PaH","PaL", "AeH","AeL", "VcL", "VcaH" ,"SeL", "VuL", "TeH", "VoL") 

str(rav_sp2)

#### exported data ####

### Ratio ###

write.csv(rav_sp2, file = "/Users/antman/Documents/rav.csv", row.names = FALSE)


##### CIT's ####

# rav

library(dplyr)

library(ggplot2)

library(partykit)

rv_cit <- rav_sp2[,-c(1)]

rv_cit$sp_id <- as.factor(rv_cit$sp_id)

rv_ConInfTree <- ctree(sp_id~., 
                       data = rv_cit)
str(rv_cit)

# Print model

print(rv_ConInfTree)

png(file = "conditionalClassification_raw.png",
    width = 1200, height = 400)

# Plotting graph

plot(rv_ConInfTree)


#### PCA ####

library(factoextra)

library(FactoMineR)

library(ggplot2)

# ratio
rav_sp2 <- rav_sp2[,-c (2)] #remove sp_id
rv_pca <- PCA(rav_sp2[,-1])

# plot individuals PCA 

#shape_final

order_sp <- c("Mohan","Momad", "Mopha", "Modrm01", "Moter02", "Moter03", "Moter03b", "MoMG01", "MoMG02", "MoMG03",
              "Syfis", "Symod", "Syhil01", "Syhil02", "SyMG01")

# shape

shp_val = c( 22, 15, 18, 17, 10, 16, 20, 8, 3, 4, 15, 17, 10, 16, 8)

rav_pca <- fviz_pca_ind(rv_pca, axes.linetype = NA, geom = "none",
                        repel = FALSE, legend.title = "Genera", labelsize = 4)+ 
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = rav_sp2$Genera, shape = rav$sp_id), size = 3)+
  coord_fixed(ratio = 1.45)+
  labs(title = NULL, shape = NULL) + xlab ("PC1 (47.62%)")+ ylab ("PC2 (24.71%)")+
  mytheme.pca

ggsave("/Users/antman/Documents/rav_pca.pdf", plot = rav_pca , width = 20, height = 15, unit = "cm")


#### Ward's method ####

library(dendextend)

library(factoextra)

### Add species name 

str(rav_sp2)

as.matrix(rav_sp2)

rav_sp2 <- as.matrix(rav_sp2)

rownames(rav_sp2) <- raw$sp_id

as.data.frame(rav_sp2)

str(rav_sp2)

rav_dat <- rav_sp2[,-c(1:2)]

as.data.frame(rav_dat)

str(rav_dat)

# single linkage

D.Hel_rv2 = dist(rav_dat, method = "euclidean",  diag = FALSE, upper = FALSE, p = 2)

#ward
D.Hel_rv2  = dist(rav_dat, method = "euclidean",  diag = FALSE, upper = FALSE, p = 2)

C.Ward_rv2 = hclust(D.Hel_rv2^2,method = 'ward') 

C.Ward_rv2$height = sqrt(C.Ward_rv2$height)

plot(C.Ward_rv2)


#### NMDS ####
# rav

library(vegan) 

library(ggplot2)

rav_nmds <- rav_sp2[,-c(1:2)]

rav_nmds <- sapply(rav_nmds, as.numeric)

as.data.frame(rav_nmds)

str(rav_nmds)

rav_nmds1 <- metaMDS(rav_nmds, k = 2, distance = "euclidean", autotransform = F)

rav_nmds1

stressplot(rav_nmds1)

rav_dat.scores <- as.data.frame(scores(rav_nmds1)$sites)

rav_dat.scores$Genera <- rav_sp2$Genera

colnames(rav_dat.scores)[3] <- c("Genera")

head(rav_dat.scores)

rav_plot_nmds <- ggplot(rav_dat.scores, aes(x = NMDS1, y = NMDS2, group = Genera),
                        repel = FALSE, legend.title = "Genera", labelsize = 6)+
  scale_shape_manual(values = shp_val, breaks = order_sp)+
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = Genera, shape = rav$sp_id), size = 3)+
  coord_fixed(ratio = 1.69)+
  labs(title = NULL, shape = NULL) +
  mytheme.nmds


ggsave("/Users/antman/Documents/rav_nmds.pdf", plot = rav_plot_nmds , width = 20, height = 15, unit = "cm")


#### LDA ####

library(dplyr)

library(MASS)

library(caret)

# raw 

rav_ld <- rav_sp2[,-c(1,3)]

str(rav_ld)

# Split the data into training and test

set.seed(123)

rv_training.samples <-  rav_ld$sp_id %>%
  createDataPartition(p = 1, list = FALSE)  # change (80, 60, 100)

rv_training <- rav_ld[rv_training.samples, ]

rv_testing <- rav_ld[-rv_training.samples, ]

str(rv_training)

# Fit the model
rv_model <- lda(sp_id~., data = rv_training)

summary(rv_model)

plot(rv_model)

p <- predict(rv_model, rv_training)

lda.data <- cbind(rv_training, p$x)

rav_plot_lda <- ggplot(lda.data, aes(LD1, LD2)) +
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = rav_sp2$Genera, shape = rav$sp_id), size = 3)+
  coord_fixed(ratio = 1.5)+
  labs(title = NULL, colour = "Genera", shape = NULL, labelsize = 6)+
  mytheme.nmds

ggsave("/Users/antman/Documents/rav_lda.pdf", plot = rav_plot_lda , width = 20, height = 15, unit = "cm")

