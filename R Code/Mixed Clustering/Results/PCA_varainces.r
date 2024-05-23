# read in data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

# find optimal number of principal components to retain for FAMD
user_pcs <- famd(ml100k_dem, 2, TRUE, 22)
var <- user_pcs$variance[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")
abline(h = 4.545455, lty = 2, col = hue_pal()(2)[1])

# find optimal number of principal components to retain for FAMD
item_pcs <- famd(ml100k_feat_d, 2, FALSE, 21)
var <- item_pcs$variance[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")