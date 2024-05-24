# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# initialise evaluation fixed variables
ui <- gen_ui_matrix(ml100k, ml100k)
krange <- krange <- seq(from = 10, to = 300, by = 10)
n_range <- 2:15

# user within cluster sum of squares
clust_obj_u <- best_n(ui, n_range)
write.csv(clust_obj_u,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_u.csv",
          row.names = FALSE)
plot(n_range, clust_obj_u, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "n clusters",
     ylab = "Average within-cluster sum of squares")

# cluster users
clust_labels <- rating_clust(ui, 6)
# generate TSNE points using appropriate similarity
sim <- gen_euc_sim(ui)
sim[is.na(sim)] <- 0
tsne <- Rtsne(sim, check_duplicates = FALSE, partial_pca = TRUE,
              is.distance = TRUE)
# TSNE plot
plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
     ylab = "Second dimension", col = alpha(hue_pal()(6)[clust_labels], 0.4))

# evaluate performance over a k range
clust_u <- cval_clust(ml100k, 10, 6, krange, gen_acos_sim, mean_centered)
write.csv(clust_u,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv",
          row.names = FALSE)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_ups.csv"
pred_ups <- read.csv(loc)
noclust_u <- pred_ups[pred_ups$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/pref_clust_ups.csv"
pref_clust_ups <- read.csv(loc)
pref_clust_u <- pref_clust_ups[pref_clust_ups$predictor ==
                                 "mean centred", ][2:6]

full <- rbind(clust_u, noclust_u, pref_clust_u)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_u$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_u$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
lines(krange, pref_clust_u$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[3])
legend("bottom",
       c("no clustering", "kmeans clustering", "rating preference clustering"),
       col = c(hue_pal()(3)),
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_u$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_u$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
lines(krange, pref_clust_u$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[3])
legend("bottom",
       c("no clustering", "kmeans clustering", "rating preference clustering"),
       col = c(hue_pal()(3)),
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_u$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "R^2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_u$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
lines(krange, pref_clust_u$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[3])
legend("bottom",
       c("no clustering", "kmeans clustering", "rating preference clustering"),
       col = c(hue_pal()(3)),
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_u$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_u$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
lines(krange, pref_clust_u$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[3])
legend("bottom",
       c("no clustering", "kmeans clustering", "rating preference clustering"),
       col = c(hue_pal()(3)),
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

# item within cluster sum of squares
clust_obj_i <- best_n(ui, n_range, FALSE)
write.csv(clust_obj_i,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_i.csv",
          row.names = FALSE)
plot(n_range, clust_obj_i, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "n clusters",
     ylab = "Average within-cluster sum of squares")

# cluster items
clust_labels <- rating_clust(ui, 5, FALSE)
# generate TSNE points using appropriate similarity
sim <- gen_euc_sim(ui, FALSE)
sim[is.na(sim)] <- 0
tsne <- Rtsne(sim, check_duplicates = FALSE, partial_pca = TRUE,
              is.distance = TRUE)
# TSNE plot
plot(tsne$Y[, 1], tsne$Y[, 2], pch = 19, xlab = "First dimension",
     ylab = "Second dimension", col = alpha(hue_pal()(5)[clust_labels], 0.4))

# evaluate performance over a k range
clust_i <- cval_clust(ml100k, 10, 5, krange, gen_ups_sim, mean_centered, FALSE)
write.csv(clust_i,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv",
          row.names = FALSE)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
noclust_i <- pred_i[pred_i$predictor == "mean centred", ][2:6]

full <- rbind(clust_i, noclust_i)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_i$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_i$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
legend("bottom",
       c("no clustering", "kmeans clustering"),
       col = hue_pal()(3)[1:2],
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_i$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_i$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
legend("bottom",
       c("no clustering", "kmeans clustering"),
       col = hue_pal()(3)[1:2],
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_i$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours", ylab = "R^2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_i$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
legend("bottom",
       c("no clustering", "kmeans clustering"),
       col = hue_pal()(3)[1:2],
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, noclust_i$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(3)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, clust_i$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(3)[2])
legend("bottom",
       c("no clustering", "kmeans clustering"),
       col = hue_pal()(3)[1:2],
       lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)
