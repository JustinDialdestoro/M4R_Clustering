# load packages
library("scales")
library("ggplot2")

# read in data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

# find optimal number of principal components to retain for FAMD
user_pcs <- famd(ml100k_dem, 2, TRUE, 22)
var_u <- data.frame(var = user_pcs$variance[, 2], p = 1:22)

print(ggplot(var_u, aes(x = p, y = var)) +
        geom_line(color = hue_pal()(10)[8]) +
        geom_point(color = hue_pal()(10)[8]) +
        theme(legend.position = "none") + xlab("Principal Components") +
        ylab("Explained Variance (%)") +
        ggtitle("FAMD of User Demographic Variables"))

# find optimal number of principal components to retain for FAMD
item_pcs <- famd(ml100k_feat_d, 2, FALSE, 21)
var_i <- data.frame(var = item_pcs$variance[, 2], p = 1:21)
print(ggplot(var_i, aes(x = p, y = var)) +
        geom_line(color = hue_pal()(10)[8]) +
        geom_point(color = hue_pal()(10)[8]) +
        theme(legend.position = "none") + xlab("Principal Components") +
        ylab("Explained Variance (%)") +
        ggtitle("FAMD of Item Feature Variables"))