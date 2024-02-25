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

# find optimal number of principal components to retain for FAMD
ml100k_dem_pca <- famd(ml100k_dem, 2, TRUE, FALSE, TRUE, 22)
var <- ml100k_dem_pca[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")
abline(h = 4.545455, lty = 2, col = hue_pal()(2)[1])

gow_obj_u <- best_n(ml100k_dem, 2:25, gow_pam)
hl_obj_u <- best_n(ml100k_dem, 2:25, hl_pam)
kproto_obj_u <- best_n(ml100k_dem, 2:15, kprototypes)
mk_obj_u <- best_n(ml100k_dem, 2:15, mixed_k)
msk_obj_u <- best_n(ml100k_dem, 2:15, mskmeans)
famd_obj_u <- best_n(ml100k_dem, 2:25, famd)
mrk_obj_u <- best_n(ml100k_dem, 2:15, mrkmeans)
kam_obj_u <- best_n(ml100k_dem, 2:15, kamila_clust)

full1 <- c(kproto_obj_u, rep(0, 10))
full2 <- c(mk_obj_u, rep(0, 10))
full3 <- c(msk_obj_u, rep(0, 10))
full4 <- c(mrk_obj_u, rep(0, 10))
full5 <- c(kam_obj_u, rep(0, 10))

mclust_obj_u <- cbind(gow_obj_u, hl_obj_u, full1, full2, full3,
                      famd_obj_u, full4, full5)
colnames(mclust_obj_u) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk", "kam")
loc <- "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_obj_u.csv"
write.csv(mclust_obj_u, file = loc, row.names = FALSE)

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
plot(2:15, mrk_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[7], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:15, kam_obj_u, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[8], xlab = "n clusters",
     ylab = "Clustering objective function")

gow_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                          mean_centered, gow_pam)
hl_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                         mean_centered, hl_pam)
kproto_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                             mean_centered, kprototypes)
mk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 4, krange, gen_acos_sim,
                         mean_centered, mixed_k)
msk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                          mean_centered, mskmeans)
famd_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                           mean_centered, famd)
mrk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 4, krange, gen_acos_sim,
                          mean_centered, mrkmeans)
kam_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                          mean_centered, kamila_clust)

mclust_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)
mclust_u <- cbind(method = c(rep("gow", 10), rep("hl", 10), rep("kproto", 10),
                             rep("mk", 10), rep("msk", 10), rep("famd", 10),
                             rep("mrk", 10), rep("kamila", 10)),
                  mclust_u)
# write user mixed clustering results into file
write.csv(mclust_u, file =
            "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_u.csv",
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
ml100k_feat_pca <- famd(ml100k_feat, 2, FALSE, FALSE, TRUE, 24)
var <- ml100k_feat_pca[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")
lines(0:26, c(0:26) * (-0.055) + 2.65, lty = 2, col = hue_pal()(2)[1])

gow_obj_i <- best_n(ml100k_feat, 2:25, gow_pam, FALSE)
hl_obj_i <- best_n(ml100k_feat, 2:25, hl_pam, FALSE)
kproto_obj_i <- best_n(ml100k_feat, 2:15, kprototypes, FALSE)
mk_obj_i <- best_n(ml100k_feat, 2:15, mixed_k, FALSE)
msk_obj_i <- best_n(ml100k_feat, 2:15, mskmeans, FALSE)
famd_obj_i <- best_n(ml100k_feat, 2:25, famd, FALSE)
mrk_obj_i <- best_n(ml100k_feat, 2:15, mrkmeans, FALSE)
kam_obj_i <- best_n(ml100k_feat, 2:15, kamila_clust, FALSE)

full1 <- c(kproto_obj_i, rep(0, 10))
full2 <- c(mk_obj_i, rep(0, 10))
full3 <- c(msk_obj_i, rep(0, 10))
full4 <- c(mrk_obj_i, rep(0, 10))
full5 <- c(kam_obj_i, rep(0, 10))

mclust_obj_i <- cbind(gow_obj_i, hl_obj_i, full1, full2, full3,
                      famd_obj_i, full4, full5)
colnames(mclust_obj_i) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk", "kam")
loc <- "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_obj_i.csv"
write.csv(mclust_obj_i, file = loc, row.names = FALSE)

plot(2:25, gow_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[1], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:25, hl_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[2], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:15, kproto_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[3], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:15, mk_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[4], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:15, msk_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[5], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:25, famd_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[6], xlab = "n clusters",
     ylab = "Total within cluster sum of squares")
plot(2:15, mrk_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[7], xlab = "n clusters",
     ylab = "Clustering objective function")
plot(2:15, kam_obj_i, lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[8], xlab = "n clusters",
     ylab = "Clustering objective function")

gow_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 3, krange, gen_ups_sim,
                          mean_centered, gow_pam, FALSE)
hl_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 4, krange, gen_ups_sim,
                         mean_centered, hl_pam, FALSE)
kproto_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 5, krange, gen_ups_sim,
                             mean_centered, kprototypes, FALSE)
mk_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 6, krange, gen_ups_sim,
                         mean_centered, mixed_k, FALSE)
msk_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 5, krange, gen_ups_sim,
                          mean_centered, mskmeans, FALSE)
famd_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 7, krange, gen_ups_sim,
                           mean_centered, famd, FALSE)
mrk_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 5, krange, gen_ups_sim,
                          mean_centered, mrkmeans, FALSE)
kam_i <- cval_mixed_clust(ml100k, ml100k_feat, 10, 5, krange, gen_ups_sim,
                          mean_centered, kamila_clust, FALSE)

mclust_i <- rbind(gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)
mclust_i <- cbind(method = c(rep("gow", 10), rep("hl", 10), rep("kproto", 10),
                             rep("mk", 10), rep("msk", 10), rep("famd", 10),
                             rep("mrk", 10), rep("kamila", 10)),
                  mclust_i)
# write user mixed clustering results into file
write.csv(mclust_i, file =
            "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_i.csv",
          row.names = FALSE)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_ups.csv"
pred_ups <- read.csv(loc)
noclust_i <- pred_ups[pred_ups$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/One-sided/clust_u.csv"
clust_i <- read.csv(loc)

full <- rbind(noclust_i[1:10, ], clust_i,
              gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("bottom",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("bottom",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("bottom",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:10, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 10 + 1):((i + 1) * 10), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("bottom",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5, horiz = TRUE)