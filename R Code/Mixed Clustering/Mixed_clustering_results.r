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
krange <- krange <- seq(from = 10, to = 300, by = 10)
n_range <- 2:15

# find optimal number of principal components to retain for FAMD
ml100k_dem_pca <- famd(ml100k_dem, 2, TRUE, FALSE, TRUE, 22)
cumvar <- ml100k_dem_pca[, 3]
plot(cumvar, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "Principal components",
     ylab = "Cumulative explained variance")

gow_obj_u <- best_n(ml100k_dem, 2:25, gow_pam)
hl_obj_u <- best_n(ml100k_dem, 2:25, hl_pam)
kproto_obj_u <- best_n(ml100k_dem, 2:15, kprototypes)
mk_obj_u <- best_n(ml100k_dem, 2:15, mixed_k)
msk_obj_u <- best_n(ml100k_dem, 2:15, mskmeans)
famd_obj_u <- best_n_famd(ml100k_dem, 2:25, 5)
mrk_obj_u <- best_n(ml100k_dem, 2:25, mrkmeans)
kam_obj_u <- best_n(ml100k_dem, 2:15, kamila_clust)

full1 <- c(kproto_obj_u, rep(0, 10))
full2 <- c(mk_obj_u, rep(0, 10))
full3 <- c(msk_obj_u, rep(0, 10))
full4 <- c(kam_obj_u, rep(0, 10))

mclust_obj_u <- cbind(gow_obj_u, hl_obj_u, full1, full2, full3,
                      famd_obj_u, mrk_obj_u, full4)
colnames(mclust_obj_u) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk", "kam")
write.csv(mclust_obj_u, file = "M4R_Clustering/Results/mclust_obj_u.csv",
          row.names = FALSE)

plot(2:25, gow_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[1], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:25, hl_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[2], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:15, kproto_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[3], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:15, mk_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[4], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:15, msk_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[5], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:25, famd_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[6], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:25, mrk_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[7], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:15, kam_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[8], xlab = "n clusters",
     ylab = "Clustering objective function")

gow_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 12, krange, gen_acos_sim,
                          mean_centered, gow_pam)
hl_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 8, krange, gen_acos_sim,
                         mean_centered, hl_pam)
kproto_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                             mean_centered, kprototypes)
mk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                         mean_centered, mixed_k)
msk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                          mean_centered, mskmeans)
famd_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 12, krange, gen_acos_sim,
                           mean_centered, famd)
mrk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 21, krange, gen_acos_sim,
                          mean_centered, mrkmeans)
kam_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                          mean_centered, kamila_clust)

mclust_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)
mclust_u <- cbind(method = c(rep("gow", 10), rep("hl", 10), rep("kproto", 10),
                             rep("mk", 10), rep("msk", 10), rep("famd", 10),
                             rep("mrk", 10), rep("kamila", 10)),
                  mclust_u)
# write user mixed clustering results into file
write.csv(mclust_u, file = "M4R_Clustering/Results/mclust_u.csv",
          row.names = FALSE)

# load unclustered performance results
pred_acos <- read.csv("M4R_Clustering/Results/pred_acos.csv")
noclust_u <- pred_acos[pred_acos$predictor == "mean centred", ][2:6]
pref_clust_acos <- read.csv("M4R_Clustering/Results/pref_clust_acos.csv")
pref_clust_u <- pref_clust_acos[pref_clust_acos$predictor ==
                                  "mean centred", ][2:6]
clust_u <- read.csv("M4R_Clustering/Results/clust_u.csv")

full <- rbind(noclust_u[1:10, ], pref_clust_u[1:10, ], clust_u,
              gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottom",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottom",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottom",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottom",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

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
mrk_obj_i <- best_n(ml100k_feat, n_range, mrkmeans, FALSE)
kam_obj_i <- best_n(ml100k_feat, n_range, kamila_clust, FALSE)

mclust_obj_i <- cbind(gow_obj_i, hl_obj_i, kproto_obj_i, mk_obj_i, msk_obj_i,
                      famd_obj_i, mrk_obj_i, kam_obj_i)
colnames(mclust_obj_i) <- c("gowpam", "hlpam", "kprototypes", "mixed kmeans",
                            "ms kmeans", "famd", "mr kmeans", "kamila")
write.csv(mclust_obj_i, file = "M4R_Clustering/Results/mclust_obj_i.csv",
          row.names = FALSE)