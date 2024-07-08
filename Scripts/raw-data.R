
#### Initial data with rep ####

data_in <- read.csv("/Users/antman/Documents/Data_Nomena/Data/Data_mensuration_finale/data_measurement_raw.csv", sep = ";")


library(tidyverse)

library(dplyr)

# create new column

library("stringr") 

ind <- str_sub(data_in$CODE, 6)

data_in <- data_in %>% 
  mutate(Ind = ind) %>% 
  relocate(Ind, .after = species) #relocate the new column

# variables needed
ext_pv <- data_in[,c(8:10,12:25)] 

# remove BmH and VcmH, VcbH
ext_pv <- ext_pv[,-c(8, 9, 15)]


#### Intraclass Correlation Coefficient (ICC) ####

# Syllophopsis

data_syllo <- ext_pv %>% 
  filter(Genera == "Syllophopsis") %>%
  dplyr::select(c(1:14))

# create new_id
data_syllo$sp_id <- paste(data_syllo$species, data_syllo$Ind, sep = "_") 

# Monomorium 
data_mono <- ext_pv %>% 
  filter(Genera == "Monomorium") %>% 
  dplyr::select(c(1:14))

data_mono$sp_id <- paste(data_mono$species, data_mono$Ind, sep = "_")


#### Load hanneli ####

data_han <- read.csv("/Users/antman/Documents/Data_Nomena/Data/Data_mensuration_finale/data_hanneli.csv")

ind <- str_sub(data_han$CODE, 6)

data_han <- data_han %>% 
  mutate(Ind = ind) %>% 
  relocate(Ind, .after = species)

data_han <- data_han %>% 
  relocate(AeH, .after = AeL)

ext_han <- data_han[,-c(4)]

ext_han$sp_id <- paste(ext_han$species, ext_han$Ind, sep = "_")

dat_mono <- rbind(data_mono, ext_han[1:16,]) #hanneli added


#### ICC ####

library("irr")

traits <- c("PaL","PaH","VuL", "SeL", "VcaH", "VcL", "TeH", "AeL", "AeH", "VoL")

len <- length(traits)

# Syllophopsis

sy_result <- matrix(NA, nrow = len, ncol = 2)

colnames(sy_result) <- c("s_traits", "s_icc")

for(i in 1:len){
  sy_temp1 <-  data_syllo[,c("Rep",traits[i],"sp_id")]
  sy_temp2 <- sy_temp1 %>% 
    pivot_wider(names_from = Rep, values_from = traits[i])
  sy_temp3 <- icc(
    sy_temp2[-1], model = "twoway", 
    type = "agreement", unit = "average"
  )
  sy_result[i, ] <- c(traits[i], round(sy_temp3$value,4))
}

# Monomorium

mo_result <- matrix(NA, nrow = len, ncol = 2)

colnames(mo_result) <- c("traits", "m_icc")

for(i in 1:len){
  mo_temp1 <-  dat_mono[,c("Rep",traits[i],"sp_id")]
  mo_temp2 <- mo_temp1 %>% 
    pivot_wider(names_from = Rep, values_from = traits[i])
  mo_temp3 <- icc(
    mo_temp2[-1], model = "twoway", 
    type = "agreement", unit = "average"
  )
  mo_result[i, ] <- c(traits[i], round(mo_temp3$value,4))
}

# combine the ICC of both genus

d_results <- data.frame(mo_result, sy_result)

icc_results <- d_results[,-3]


# export_data 

write.csv(icc_results, file = "/Users/antman/Documents/icc_results.csv", row.names = FALSE)

#### data transformation ####

# raw data (76 ind)

data_in <- rbind(dat_mono, data_syllo) #Data_in with hanneli

library(tidyverse)

library(dplyr)

library(factoextra)

library(FactoMineR)

for(i in 6:14){
  temp <- as.numeric(data_in[,i])
  data_in[,i] <- temp
}

raw <- data_in %>% 
  group_by(Genera, sp_id, Ind) %>% 
  summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
  dplyr::select( - Rep)


#### exported data ####

### Raw ###

write.csv(raw, file = "/Users/antman/Documents/raw.csv", row.names = FALSE)

#add new column

raw$sp_id <- c(rep("Mohan",4), rep("Moter02", 12), rep("Modrm01", 7), rep("Moter03", 4), 
            rep("Mopha", 4), rep("MoMG01", 3), rep("MoMG02",  7), rep("MoMG03", 2), 
            rep("Moter03b", 3), rep("Momad", 8), rep("Syhil01", 6),
            rep("Syhil02", 6), rep("Syfis", 6), rep("SyMG01", 2), rep("Symod", 6))

# test test

raw_sp <- raw %>% 
  mutate(sp_id = paste(sp_id, Ind, sep = "_")) %>% 
  dplyr::select(-sp_code) %>% 
  relocate(sp_id, .after = Ind)


raw_cit <- raw_sp

raw_cit$sp_id <- raw$sp_id

view(raw_cit)

##### CIT's ####

# raw

library(dplyr)

library(ggplot2)

library(partykit)

rw_cit <- raw_sp[,-c(2, 3, 14)]

rw_cit$Genera <- as.factor(rw_cit$Genera)

rw_ConInfTree <- ctree(Genera~., 
                       data = rw_cit)

# species
rw_cit <- raw_cit[,-c(1,2,14)]

rw_cit$sp_id <- as.factor(rw_cit$sp_id)

rw_ConInfTree <- ctree(sp_id~., 
                       data = rw_cit)

str(rw_cit)

# Print model

print(rw_ConInfTree)

png(file = "conditionalClassification_raw.png",
    width = 1200, height = 400)

# Plotting graph

plot(rw_ConInfTree)


#### PCA ####

library(factoextra)

library(FactoMineR)

library(ggplot2)

# raw
raw_sp2 <- raw_sp[,-c (2,3, 14)]
rw_pca <- PCA(raw_sp2[,-1])

# plot individuals PCA 

#shape_final

order_sp <- c("Mohan","Momad", "Mopha", "Modrm01", "Moter02", "Moter03", "Moter03b", "MoMG01", "MoMG02", "MoMG03",
              "Syfis", "Symod", "Syhil01", "Syhil02", "SyMG01")

# shape

shp_val = c( 22, 15, 18, 17, 10, 16, 20, 8, 3, 4, 15, 17, 10, 16, 8)

raw_pca <- fviz_pca_ind(rw_pca, axes.linetype = NA, geom = "none",
                        repel = FALSE, legend.title = "Genera", labelsize = 4)+ 
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = raw_sp2$Genera, shape = raw_sp$sp_id), size = 3)+
  coord_fixed(ratio = 2.5)+
  labs(title = NULL, shape = NULL) + xlab ("PC1 (78.12%)")+ ylab ("PC2 (13.61%)")+
  mytheme.pca

ggsave("/Users/antman/Documents/raw_pca.pdf", plot = raw_pca , width = 20, height = 15, unit = "cm")


#### UPGMA ####

library(dendextend)

library(factoextra)

### Add species name 

str(raw_sp)


md_sp <- raw_sp


raw_sp2 <- raw_sp[,-c(2, 3, 14)]

as.matrix(raw_sp2)

raw_sp2 <- as.matrix(raw_sp)

rownames(raw_sp2) <- raw$sp_id

as.data.frame(raw_sp2)

rw_dat <- raw_sp2[,-c(1:3, 14)]

as.data.frame(rw_dat)

str(rw_dat)

# single linkage

D.Hel_rw2 = dist(rw_dat, method = "euclidean",  diag = FALSE, upper = FALSE, p = 2)


#ward

D.Hel_rw2 = dist(rw_dat, method = "euclidean",  diag = FALSE, upper = FALSE, p = 2)

C.Ward_rw2 = hclust(D.Hel_rw2^2,method = 'ward') 

C.Ward_rw2$height = sqrt(C.Ward_rw2$height)

plot(C.Ward_rw2)

library(dendextend)

dend_rw2 <- as.dendrogram(C.Ward_rw2)

col_rw2 <- ifelse(grepl("Mo", labels(dend_rw2)), "#134b73", "#e41a1c") # Monomorium

pre_fig_rw2 <- assign_values_to_leaves_edgePar(dend = dend_rw2 , value = col_rw2, edgePar = "col") 

# plot

ward_rw2 <- fviz_dend(pre_fig_rw2, cex = 0.5, show_labels = T, horiz = TRUE,
                      main = NULL)

ggsave("/Users/antman/Documents/ward_rw2.png", plot = ward_rw2 , width = 25, height = 20, unit = "cm")


#### NMDS ####
# raw

library(vegan) 

library(ggplot2)

raw_nmds <- raw_sp2[,-1]

data(raw_nmds)

raw_nmds1 <- metaMDS(raw_nmds, k = 2, distance = "euclidean", autotransform = F)

raw_nmds1

stressplot(raw_nmds1)

raw_dat.scores <- as.data.frame(scores(raw_nmds1)$sites)

raw_dat.scores$Genera <- raw_sp2$Genera

colnames(raw_dat.scores)[3] <- c("Genera")

head(raw_dat.scores)

raw_plot_nmds <- ggplot(raw_dat.scores, aes(x = NMDS1, y = NMDS2, group = Genera),
                        repel = FALSE, legend.title = "Genera", labelsize = 6)+
  scale_shape_manual(values = shp_val, breaks = order_sp)+
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = Genera, shape = raw_sp$sp_id), size = 3)+
  coord_fixed(ratio = 2.5)+
  labs(title = NULL, shape = NULL) +
  mytheme.nmds


ggsave("/Users/antman/Documents/raw_nmds.pdf", plot = raw_plot_nmds , width = 20, height = 15, unit = "cm")


#### LDA ####

library(dplyr)

library(MASS)

library(caret)

# raw 

raw_ld <- raw_sp[,-c(1:3)]

str(raw_ld)

# Split the data into training and test

set.seed(123)

rw_training.samples <-  raw_ld$sp_id %>%
  createDataPartition(p = 1, list = FALSE)  # change (80, 60, 100)

rw_training <- raw_ld[rw_training.samples, ]

rw_testing <- raw_ld[-rw_training.samples, ]

str(rw_training)

# Fit the model
rw_model <- lda(sp_id~., data = rw_training)

summary(rw_model)

plot(rw_model)

p <- predict(rw_model, rw_training)

lda.data <- cbind(rw_training, p$x)

raw_plot_lda <- ggplot(lda.data, aes(LD1, LD2)) +
  scale_shape_manual(values= shp_val, breaks = order_sp) +
  scale_color_manual(values = c("#134b73", "#e41a1c"))+
  geom_point(aes(colour = raw_sp$Genera, shape = raw_sp$sp_id), size = 3)+
  coord_fixed(ratio = 1.1)+
  labs(title = NULL, colour = "Genera", shape = NULL, labelsize = 6)+
  mytheme.nmds

ggsave("/Users/antman/Documents/raw_lda.pdf", plot = raw_plot_lda , width = 20, height = 15, unit = "cm")
