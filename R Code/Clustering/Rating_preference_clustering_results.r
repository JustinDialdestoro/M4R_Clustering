# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

wsum_clust <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, weighted_sum,
                              acos_clust, 3, 4)
mcent_clust <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, mean_centered,
                               acos_clust, 3, 4)
zscore_clust <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, z_score,
                                acos_clust, 3, 4)
disc_clust <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, discrete,
                              acos_clust, 3, 4)

pref_clust_u <- rbind(wsum_clust, mcent_clust, zscore_clust, disc_clust)

pref_clust_u <- cbind(predictor = c(rep("weighted sum", n),
                                    rep("mean centred", n),
                                    rep("z score", n),
                                    rep("discrete", n)), pref_clust_u)

# write user preference clustering results into file
write.csv(pref_clust_u, file = "M4R_Clustering/Results/pref_clust_u.csv",
          row.names = FALSE)

# read previously obtained prediction comparison
pred_u <- read.csv("M4R_Clustering/Results/pred_u.csv")
wsum_pred <- pred_u[pred_u$predictor == "weighted sum", 2:6]
mcent_pred <- pred_u[pred_u$predictor == "mean centred", 2:6]
zscore_pred <- pred_u[pred_u$predictor == "z score", 2:6]
disc_pred <- pred_u[pred_u$predictor == "discrete", 2:6]

full <- rbind(pref_clust_u, pred_u)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_clust$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_clust$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_clust$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_clust$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_pred$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_pred$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_pred$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_pred$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(1, 1, 1, 1, 2), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_clust$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_clust$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_clust$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_clust$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_pred$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_pred$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_pred$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_pred$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(1, 1, 1, 1, 2), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_clust$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_clust$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_clust$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_clust$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_pred$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_pred$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_pred$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_pred$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(1, 1, 1, 1, 2), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_clust$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_clust$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_clust$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_clust$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_pred$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_pred$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_pred$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_pred$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(1, 1, 1, 1, 2), lwd = 2, cex = 0.8, horiz = TRUE)

beta_range <- c(3, 3.2, 3.4, 3.6, 3.8, 4, 4.2)

beta_search <- NULL

p <- 1

for (i in 1:7) {
  results <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, weighted_sum,
                             acos_clust, 2, beta_range[i])
  results <- cbind(beta = rep(beta_range[i], n), results)

  beta_search <- rbind(beta_search, results)
}


# write alpha, beta comparison results into file
write.csv(beta_search, file = "M4R_Clustering/Results/beta_search.csv",
          row.names = FALSE)

ymax <- max(beta_search$rmse)
ymin <- min(beta_search$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_search[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(7)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:6) {
  lines(krange, beta_search[(30 * i + 1):(30 * (i + 1)), ]$rmse, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(7)[i + 1])
}
legend("topright", c("b=3", "b=3.2", "b=3.4", "b=3.6", "b=3.8", "b=4", "b=4.2"),
       col = hue_pal()(7), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$mae)
ymin <- min(alpha_beta$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_search[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(7)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:6) {
  lines(krange, beta_search[(30 * i + 1):(30 * (i + 1)), ]$mae, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(7)[i + 1])
}
legend("topright", c("b=3", "b=3.2", "b=3.4", "b=3.6", "b=3.8", "b=4", "b=4.2"),
       col = hue_pal()(7), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$r2)
ymin <- min(alpha_beta$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_search[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(7)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:6) {
  lines(krange, beta_search[(30 * i + 1):(30 * (i + 1)), ]$r2, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(7)[i + 1])
}
legend("topright", c("b=3", "b=3.2", "b=3.4", "b=3.6", "b=3.8", "b=4", "b=4.2"),
       col = hue_pal()(7), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$online)
ymin <- min(alpha_beta$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_search[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(7)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:6) {
  lines(krange, beta_search[(30 * i + 1):(30 * (i + 1)), ]$online, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(7)[i + 1])
}
legend("topright", c("b=3", "b=3.2", "b=3.4", "b=3.6", "b=3.8", "b=4", "b=4.2"),
       col = hue_pal()(7), lty = 1, lwd = 2, cex = 0.8)
