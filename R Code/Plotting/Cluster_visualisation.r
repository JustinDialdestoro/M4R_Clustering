# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# cluster users
clust_labels_u <- rep(c(), 8)
clust_labels_u[[1]] <- gow_pam(ml100k_dem, 7)
clust_labels_u[[2]] <- hl_pam(ml100k_dem, 5)
clust_labels_u[[3]] <- kprototypes(ml100k_dem, 5)
clust_labels_u[[4]] <- mixed_k(ml100k_dem, 4)
clust_labels_u[[5]] <- mskmeans(ml100k_dem, 7)
clust_labels_u[[6]] <- famd(ml100k_dem, 7)
clust_labels_u[[7]] <- mrkmeans(ml100k_dem, 4)
clust_labels_u[[8]] <- kamila_clust(ml100k_dem, 5)

# variance normalise age
ml100k_dem$age <- unit_var_normalise(ml100k_dem$age)
# dummy code gender and occupation variable
ml100k_dem <- dummy_cols(ml100k_dem, select_columns = "gender")
ml100k_dem$gender <- NULL
ml100k_dem <- dummy_cols(ml100k_dem, select_columns = "occupation")
ml100k_dem$occupation <- NULL

tsne <- Rtsne(as.matrix(ml100k_dem), check_duplicates = FALSE)
# TSNE plots
for (i in 1:8) {
  plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
       ylab = "Second dimension",
       col = alpha(hue_pal()(7)[clust_labels_u[[i]]$clusters], 0.4))
}

# cluster items
clust_labels_i <- rep(c(), 8)
clust_labels_i[[1]] <- gow_pam(ml100k_feat_b, 3, FALSE)
clust_labels_i[[2]] <- hl_pam(ml100k_feat_d, 4, FALSE)
clust_labels_i[[3]] <- kprototypes(ml100k_feat_b, 5, FALSE)
clust_labels_i[[4]] <- mixed_k(ml100k_feat_c, 6, FALSE)
clust_labels_i[[5]] <- mskmeans(ml100k_feat_d, 5, FALSE)
clust_labels_i[[6]] <- famd(ml100k_feat_d, 7, FALSE)
clust_labels_i[[7]] <- mrkmeans(ml100k_feat_d, 5, FALSE)
clust_labels_i[[8]] <- kamila_clust(ml100k_feat_b, 5, FALSE)

# variance normalise continuous variables
ml100k_feat_d$year <- unit_var_normalise(ml100k_feat_d$year)
ml100k_feat_d$runtime <- unit_var_normalise(ml100k_feat_d$runtime)

tsne <- Rtsne(as.matrix(ml100k_feat_d), check_duplicates = FALSE)
# TSNE plots
for (i in 1:8) {
  plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
       ylab = "Second dimension",
       col = alpha(hue_pal()(7)[clust_labels_i[[i]]$clusters], 0.4))
}