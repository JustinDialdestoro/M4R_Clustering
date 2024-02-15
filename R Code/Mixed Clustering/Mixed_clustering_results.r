# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")

ml100k_feat <- read.csv("M4R_Clustering/Data/ml100k_feat_a.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering.r")

# initialise evaluation fixed variables
krange <- krange <- seq(from = 10, to = 100, by = 10)
n_range <- 2:15

# find optimal number of principal components to retain for FAMD
ml100k_dem_pca <- famd(ml100k_dem, 2, 22, TRUE, FALSE, TRUE)
cumvar <- ml100k_dem_pca[, 3]
plot(cumvar, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "Principal components",
     ylab = "Cumulative explained variance")

gow_obj_u <- best_n(ml100k_dem, n_range, gow_pam)
hl_obj_u <- best_n(ml100k_dem, n_range, hl_pam)
kproto_obj_u <- best_n(ml100k_dem, n_range, kprototypes)
mk_obj_u <- best_n(ml100k_dem, n_range, mixed_k)
msk_obj_u <- best_n(ml100k_dem, n_range, mskmeans)
famd_obj_u <- best_n_famd(ml100k_dem, n_range, 5)
mr_obj_u <- best_n(ml100k_dem, n_range, mrkmeans)
kam_obj_u <- best_n(ml100k_dem, n_range, kamila_clust)

mclust_obj_u <- cbind(gow_obj_u, hl_obj_u, kproto_obj_u, mk_obj_u, msk_obj_u,
                      famd_obj_u, mr_obj_u, kam_obj_u)
colnames(mclust_obj_u) <- c("gowpam", "hlpam", "kprototypes", "mixed kmeans",
                            "ms kmeans", "famd", "mr kmeans", "kamila")
write.csv(mclust_obj_u, file = "M4R_Clustering/Results/mclust_obj_u.csv",
          row.names = FALSE)

for (i in 1:8) {
  plot(n_range, mclust_obj_u[, i], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Clustering objective function")
}

# find optimal number of principal components to retain for FAMD
ml100k_feat_pca <- famd(ml100k_feat, 2, 24, FALSE, FALSE, TRUE)
cumvar <- ml100k_feat_pca[, 3]
plot(cumvar, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "Principal components",
     ylab = "Cumulative explained variance")

gow_obj_i <- best_n(ml100k_feat, n_range, gow_pam, FALSE)
hl_obj_i <- best_n(ml100k_feat, n_range, hl_pam, FALSE)
kproto_obj_i <- best_n(ml100k_feat, n_range, kprototypes, FALSE)
mk_obj_i <- best_n(ml100k_feat, n_range, mixed_k, FALSE)
msk_obj_i <- best_n(ml100k_feat, n_range, mskmeans, FALSE)
famd_obj_i <- best_n_famd(ml100k_feat, n_range, 5, FALSE)
mr_obj_i <- best_n(ml100k_feat, n_range, mrkmeans, FALSE)
kam_obj_i <- best_n(ml100k_feat, n_range, kamila_clust, FALSE)

mclust_obj_i <- cbind(gow_obj_i, hl_obj_i, kproto_obj_i, mk_obj_i, msk_obj_i,
                      famd_obj_i, mr_obj_i, kam_obj_i)
colnames(mclust_obj_i) <- c("gowpam", "hlpam", "kprototypes", "mixed kmeans",
                            "ms kmeans", "famd", "mr kmeans", "kamila")
write.csv(mclust_obj_i, file = "M4R_Clustering/Results/mclust_obj_i.csv",
          row.names = FALSE)