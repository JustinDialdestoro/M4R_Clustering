# load packages
library("scales")
library("ggplot2")
library("ggpubr")
library("extrafont")

# read in data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

# find optimal number of principal components to retain for FAMD
user_pcs <- famd(ml100k_dem, 2, TRUE, 22)
var_u <- data.frame(var = user_pcs$variance[, 2], p = 1:22)

pca_u <- ggplot(var_u, aes(x = p, y = var)) +
  geom_line(color = hue_pal()(10)[8], linewidth = 1) +
  geom_point(color = hue_pal()(10)[8], size = 2.5) +
  theme(legend.position = "none") + xlab("Principal Components") +
  ylab("Explained Variance (%)") + theme_bw(base_size = 20) +
  ggtitle("FAMD of User Demographic Variables") +
  theme(text = element_text(family = "LM Roman 10"))

# find optimal number of principal components to retain for FAMD
item_pcs <- famd(ml100k_feat_d, 2, FALSE, 21)
var_i <- data.frame(var = item_pcs$variance[, 2], p = 1:21)
pca_i <- ggplot(var_i, aes(x = p, y = var)) +
  geom_line(color = hue_pal()(10)[8], linewidth = 1) +
  geom_point(color = hue_pal()(10)[8], size = 2.5) +
  theme(legend.position = "none") + xlab("Principal Components") +
  ylab("Explained Variance (%)") + theme_bw(base_size = 20) +
  ggtitle("FAMD of Item Feature Variables") +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 15 x 6
print(ggarrange(pca_u, pca_i))