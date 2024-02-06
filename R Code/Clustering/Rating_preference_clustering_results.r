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

wsum_u <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, weighted_sum,
                          acos_clust, 3, 4)
mcent_u <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, mean_centered,
                           acos_clust, 3, 4)
zscore_u <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, z_score,
                            acos_clust, 3, 4)
disc_u <- cval_pref_clust(ml100k, 10, krange, en_ups_sim, discrete,
                          acos_clust, 3, 4)

pref_clust_u <- rbind(wsum_u, mcent_u, zscore_u, disc_u)

pref_clust_u <- cbind(predictor = c(rep("weighted sum", n),
                                    rep("mean centred", n),
                                    rep("z score", n),
                                    rep("discrete", n)), pref_clust_u)

# write user preference clustering results into file
write.csv(pref_clust_u, file = "M4R_Clustering/Results/pref_clust_u.csv",
          row.names = FALSE)

ymax <- max(pref_clust_u$rmse)
ymin <- min(pref_clust_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$mae)
ymin <- min(pref_clust_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$r2)
ymin <- min(pref_clust_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$online)
ymin <- min(pref_clust_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

alpha_range <- c(3, 3.25, 3.5)
beta_range <- c(3.75, 4, 4.25)

alpha_beta <- NULL

p <- 1

for (i in 1:3) {
  for (j in 1:3) {
    results <- cval_pref_clust(ml100k, 10, krange, gen_ups_sim, weighted_sum,
                               acos_clust, alpha_range[i], beta_range[j])
    results <- cbind(beta = rep(beta_range[j], n), results)
    results <- cbind(alpha = rep(alpha_range[i], n), results)

    alpha_beta <- rbind(alpha_beta, results)
  }
}

# write alpha, beta comparison results into file
write.csv(alpha_beta, file = "M4R_Clustering/Results/alpha_beta",
          row.names = FALSE)

ymax <- max(alpha_beta$rmse)
ymin <- min(alpha_beta$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, alpha_beta[1:30, ]$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, alpha_beta[(30 * i + 1):(30 * (i + 1)), ]$rmse, lty = 2,
        type = "b", pch = 4, lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("a=3, b=3.75", "a=3, b=4",
                     "a=3, b=4.25", "a=3.25, b=3.75",
                     "a=3.25, b=4", "a=3.25, b=4.25",
                     "a=3.5, b=3.75", "a=3.5, b=4",
                     "a=3.5, b=4.25"),
       col = hue_pal()(9), lty = 2, pch = 4, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$mae)
ymin <- min(alpha_beta$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, alpha_beta[1:30, ]$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, alpha_beta[(30 * i + 1):(30 * (i + 1)), ]$mae, lty = 2,
        type = "b", pch = 4, lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("a=3, b=3.75", "a=3, b=4",
                     "a=3, b=4.25", "a=3.25, b=3.75",
                     "a=3.25, b=4", "a=3.25, b=4.25",
                     "a=3.5, b=3.75", "a=3.5, b=4",
                     "a=3.5, b=4.25"),
       col = hue_pal()(9), lty = 2, pch = 4, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$r2)
ymin <- min(alpha_beta$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, alpha_beta[1:30, ]$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, alpha_beta[(30 * i + 1):(30 * (i + 1)), ]$r2, lty = 2,
        type = "b", pch = 4, lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("a=3, b=3.75", "a=3, b=4",
                     "a=3, b=4.25", "a=3.25, b=3.75",
                     "a=3.25, b=4", "a=3.25, b=4.25",
                     "a=3.5, b=3.75", "a=3.5, b=4",
                     "a=3.5, b=4.25"),
       col = hue_pal()(9), lty = 2, pch = 4, lwd = 2, cex = 0.8)

ymax <- max(alpha_beta$online)
ymin <- min(alpha_beta$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, alpha_beta[1:30, ]$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, alpha_beta[(30 * i + 1):(30 * (i + 1)), ]$online, lty = 2,
        type = "b", pch = 4, lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("a=3, b=3.75", "a=3, b=4",
                     "a=3, b=4.25", "a=3.25, b=3.75",
                     "a=3.25, b=4", "a=3.25, b=4.25",
                     "a=3.5, b=3.75", "a=3.5, b=4",
                     "a=3.5, b=4.25"),
       col = hue_pal()(9), lty = 2, pch = 4, lwd = 2, cex = 0.8)
