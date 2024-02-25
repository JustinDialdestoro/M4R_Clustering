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
krange <- seq(from = 10, to = 300, by = 10)

# find optimal number of principal components to retain for FAMD
ml100k_dem_pca <- famd(ml100k_dem, 2, TRUE, FALSE, TRUE, 22)
var <- ml100k_dem_pca[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")
abline(h = 4.545455, lty = 2, col = hue_pal()(2)[1])

# find best number of clusters
gow_obj_u <- best_n(ml100k_dem, 2:25, gow_pam)
hl_obj_u <- best_n(ml100k_dem, 2:25, hl_pam)
kproto_obj_u <- best_n(ml100k_dem, 2:15, kprototypes)
mk_obj_u <- best_n(ml100k_dem, 2:15, mixed_k)
msk_obj_u <- best_n(ml100k_dem, 2:15, mskmeans)
famd_obj_u <- best_n(ml100k_dem, 2:25, famd)
mrk_obj_u <- best_n(ml100k_dem, 2:15, mrkmeans)
kam_obj_u <- best_n(ml100k_dem, 2:15, kamila_clust)

# fill in data to write into a file
full1 <- c(kproto_obj_u, rep(0, 10))
full2 <- c(mk_obj_u, rep(0, 10))
full3 <- c(msk_obj_u, rep(0, 10))
full4 <- c(mrk_obj_u, rep(0, 10))
full5 <- c(kam_obj_u, rep(0, 10))

# write results into a file
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

# cluster users
clust_labels_u <- rep(c(), 8)
clust_labels_u[[1]] <- gow_pam(ml100k_dem, 7)
clust_labels_u[[2]] <- hl_pam(ml100k_dem, 5)
clust_labels_u[[3]] <- kprototypes(ml100k_dem, 5)
clust_labels_u[[4]] <- mixed_k(ml100k_dem, 4)
clust_labels_u[[5]] <- mskmeans(ml100k_dem, 7)
clust_labels_u[[6]] <- famd(ml100k_dem, 7)
clust_labels_u[[7]] <- mrkmeans(ml100k_dem, 4)
clust_labels_u[[8]] <- kamila(ml100k_dem, 5)

# prepare data to create TSNE plot
df <- ml100k_dem
# variance normalise age
df$age <- unit_var_normalise(df$age)
# dummy code gender and occupation variable
df <- dummy_cols(df, select_columns = "gender")
df$gender <- NULL
df <- dummy_cols(df, select_columns = "occupation")
df$occupation <- NULL

tsne <- Rtsne(df, check_duplicates = FALSE, partial_pca = TRUE)
# TSNE plots
for (i in 1:8) {
  plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
       ylab = "Second dimension",
       col = alpha(hue_pal()(7)[clust_labels_u[[i]]], 0.4))
}

# evaluate mixed clustering over a k range
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

# write user mixed clustering results into file
mclust_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)
mclust_u <- cbind(method = c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
                             rep("mk", 30), rep("msk", 30), rep("famd", 30),
                             rep("mrk", 30), rep("kamila", 30)),
                  mclust_u)
write.csv(mclust_u, file =
            "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_u.csv",
          row.names = FALSE)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_ups.csv"
pred_ups <- read.csv(loc)
noclust_u <- pred_ups[pred_ups$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/One-sided/pref_clust_ups.csv"
pref_clust_ups <- read.csv(loc)
pref_clust_u <- pref_clust_ups[pref_clust_ups$predictor ==
                                 "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/One-sided/clust_u.csv"
clust_u <- read.csv(loc)

full <- rbind(noclust_u, pref_clust_u, clust_u,
              gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottomright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topleft",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

# find optimal number of principal components to retain for FAMD
ml100k_feat_pca <- famd(ml100k_feat, 2, FALSE, FALSE, TRUE, 24)
var <- ml100k_feat_pca[, 2]
plot(var, pch = 16, col = hue_pal()(2)[1], xlab = "Principal components",
     cex = 1.5, ylab = "Explained variance")
lines(0:26, c(0:26) * (-0.055) + 2.65, lty = 2, col = hue_pal()(2)[1])

# find best number of clusters
gow_obj_i <- best_n(ml100k_feat, 2:25, gow_pam, FALSE)
hl_obj_i <- best_n(ml100k_feat, 2:25, hl_pam, FALSE)
kproto_obj_i <- best_n(ml100k_feat, 2:15, kprototypes, FALSE)
mk_obj_i <- best_n(ml100k_feat, 2:15, mixed_k, FALSE)
msk_obj_i <- best_n(ml100k_feat, 2:15, mskmeans, FALSE)
famd_obj_i <- best_n(ml100k_feat, 2:25, famd, FALSE)
mrk_obj_i <- best_n(ml100k_feat, 2:15, mrkmeans, FALSE)
kam_obj_i <- best_n(ml100k_feat, 2:15, kamila_clust, FALSE)

# fill in data to write into a file
full1 <- c(kproto_obj_i, rep(0, 10))
full2 <- c(mk_obj_i, rep(0, 10))
full3 <- c(msk_obj_i, rep(0, 10))
full4 <- c(mrk_obj_i, rep(0, 10))
full5 <- c(kam_obj_i, rep(0, 10))

# write results into a file
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

# cluster items
clust_labels_i <- rep(c(), 8)
clust_labels_i[[1]] <- gow_pam(ml100k_dem, 3, FALSE)
clust_labels_i[[2]] <- hl_pam(ml100k_dem, 4, FALSE)
clust_labels_i[[3]] <- kprototypes(ml100k_dem, 5, FALSE)
clust_labels_i[[4]] <- mixed_k(ml100k_dem, 6, FALSE)
clust_labels_i[[5]] <- mskmeans(ml100k_dem, 5, FALSE)
clust_labels_i[[6]] <- famd(ml100k_dem, 7, FALSE)
clust_labels_i[[7]] <- mrkmeans(ml100k_dem, 5, FALSE)
clust_labels_i[[8]] <- kamila(ml100k_dem, 5, FALSE)

# prepare data to create TSNE plot
df <- ml100k_feat
# variance normalise continuous variables
df$year <- unit_var_normalise(df$year)
df$runtime <- unit_var_normalise(df$runtime)
# dummy code title type
df <- dummy_cols(df, select_columns = "titleType")
df$titleType <- NULL
# dummy code director type
df <- dummy_cols(df, select_columns = "director")
df$director <- NULL
# dummy code writer type
df <- dummy_cols(df, select_columns = "writer")
df$writer <- NULL

tsne <- Rtsne(df, check_duplicates = FALSE, partial_pca = TRUE)
# TSNE plots
for (i in 1:8) {
  plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
       ylab = "Second dimension",
       col = alpha(hue_pal()(7)[clust_labels_i[[i]]], 0.4))
}

# evaluate mixed clustering over a k range
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

# write user mixed clustering results into file
mclust_i <- rbind(gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)
mclust_i <- cbind(method = c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
                             rep("mk", 30), rep("msk", 30), rep("famd", 30),
                             rep("mrk", 30), rep("kamila", 30)),
                  mclust_i)
write.csv(mclust_i, file =
            "M4R_Clustering/Results/Mixed clustering/One-sided/mclust_i.csv",
          row.names = FALSE)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
noclust_i <- pred_i[pred_i$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/One-sided/clust_i.csv"
clust_i <- read.csv(loc)

full <- rbind(noclust_i, clust_i,
              gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topleft",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)