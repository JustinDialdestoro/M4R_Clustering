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

# acosine beta search
beta_range <- c(2.9, 3, 3.1, 3.9, 4, 4.1, 4.3, 4.4, 4.5)

beta_acos <- NULL

for (b in beta_range) {
  results <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, weighted_sum,
                             acos_clust, 2, b)
  results <- cbind(beta = rep(b, n), results)

  beta_acos <- rbind(beta_acos, results)
}

# write beta comparison results into file
write.csv(beta_acos, file = "M4R_Clustering/Results/beta_acos.csv",
          row.names = FALSE)

ymax <- max(beta_acos$rmse)
ymin <- min(beta_acos$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_acos[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_acos[(30 * i + 1):(30 * (i + 1)), ]$rmse, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                     "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_acos$mae)
ymin <- min(beta_acos$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_acos[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_acos[(30 * i + 1):(30 * (i + 1)), ]$mae, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                     "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_acos$r2)
ymin <- min(beta_acos$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_acos[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_acos[(30 * i + 1):(30 * (i + 1)), ]$r2, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("bottomright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                        "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_acos$online)
ymin <- min(beta_acos$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_acos[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_acos[(30 * i + 1):(30 * (i + 1)), ]$online, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("bottomright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                        "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

wsum_acos_c <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, weighted_sum,
                               acos_clust, 3, 4.5)
mcent_acos_c <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, mean_centered,
                                acos_clust, 3, 4.5)
zscore_acos_c <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, z_score,
                                 acos_clust, 3, 4.5)
disc_acos_c <- cval_pref_clust(ml100k, 10, krange, gen_acos_sim, discrete,
                               acos_clust, 3, 4.5)

pref_clust_acos <- rbind(wsum_acos_c, mcent_acos_c, zscore_acos_c, disc_acos_c)

pref_clust_acos <- cbind(predictor = c(rep("weighted sum", n),
                                       rep("mean centred", n),
                                       rep("z score", n),
                                       rep("discrete", n)), pref_clust_acos)

# write user preference clustering results into file
write.csv(pref_clust_acos, file = "M4R_Clustering/Results/pref_clust_acos.csv",
          row.names = FALSE)

# read previously obtained prediction comparison
pred_acos <- read.csv("M4R_Clustering/Results/pred_acos.csv")
wsum_acos <- pred_u[pred_u$predictor == "weighted sum", 2:6]
mcent_acos <- pred_u[pred_u$predictor == "mean centred", 2:6]
zscore_acos <- pred_u[pred_u$predictor == "z score", 2:6]
disc_acos <- pred_u[pred_u$predictor == "discrete", 2:6]

full <- rbind(pref_clust_acos, pred_acos)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos_c$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_acos$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_acos$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos_c$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_acos$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_acos$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos_c$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_acos$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_acos$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos_c$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_acos$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_acos$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

# UPS beta search
beta_ups <- NULL

for (b in beta_range) {
  results <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, weighted_sum,
                             ups_clust, 2, b)
  results <- cbind(beta = rep(b, n), results)

  beta_ups <- rbind(beta_ups, results)
}

# write beta comparison results into file
write.csv(beta_ups, file = "M4R_Clustering/Results/beta_ups.csv",
          row.names = FALSE)

ymax <- max(beta_ups$rmse)
ymin <- min(beta_ups$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_ups[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_ups[(30 * i + 1):(30 * (i + 1)), ]$rmse, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                     "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_ups$mae)
ymin <- min(beta_ups$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_ups[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_ups[(30 * i + 1):(30 * (i + 1)), ]$mae, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                     "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_ups$r2)
ymin <- min(beta_ups$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_ups[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_ups[(30 * i + 1):(30 * (i + 1)), ]$r2, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("bottomright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                        "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(beta_ups$online)
ymin <- min(beta_ups$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, beta_ups[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, beta_ups[(30 * i + 1):(30 * (i + 1)), ]$online, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("bottomright", c("b=2.9", "b=3", "b=3.1", "b=3.9", "b=4",
                        "b=4.1", "b=4.3", "b=4.4", "b=4.5"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

wsum_ups_c <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, weighted_sum,
                              ups_clust, 3, 4.5)
mcent_ups_c <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, mean_centered,
                               ups_clust, 3, 4.5)
zscore_ups_c <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, z_score,
                                ups_clust, 3, 4.5)
disc_ups_c <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, discrete,
                              ups_clust, 3, 4.5)

pref_clust_ups <- rbind(wsum_ups_c, mcent_ups_c, zscore_ups_c, disc_ups_c)

pref_clust_ups <- cbind(predictor = c(rep("weighted sum", n),
                                      rep("mean centred", n),
                                      rep("z score", n),
                                      rep("discrete", n)), pref_clust_ups)

# write user preference clustering results into file
write.csv(pref_clust_ups, file = "M4R_Clustering/Results/pref_clust_ups.csv",
          row.names = FALSE)

# read previously obtained prediction comparison
pred_ups <- read.csv("M4R_Clustering/Results/pred_ups.csv")
wsum_ups <- pred_u[pred_u$predictor == "weighted sum", 2:6]
mcent_ups <- pred_u[pred_u$predictor == "mean centred", 2:6]
zscore_ups <- pred_u[pred_u$predictor == "z score", 2:6]
disc_ups <- pred_u[pred_u$predictor == "discrete", 2:6]

full <- rbind(pref_clust_ups, pred_ups)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups_c$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups_c$rmse, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_ups$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_ups$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$rmse, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups_c$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups_c$mae, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_ups$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_ups$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$mae, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups_c$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups_c$r2, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_ups$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_ups$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$r2, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups_c$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups_c$online, lty = 1, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
lines(krange, wsum_ups$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[1])
lines(krange, mcent_ups$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$online, lty = 2, type = "l", lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete",
                   "clustered"),
       col = c(hue_pal()(4), "black"),
       lty = c(2, 2, 2, 2, 1), lwd = 2, cex = 0.8, horiz = TRUE)
