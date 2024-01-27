# read in the data
u100k <- read.csv("M4R_Clustering/Data/u100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 100, by = 10)

# # evaluate cosine, adjusted cosine, and pcc
euc_scores_wsum_u <- cval(u100k, 10, krange,  gen_euc_sim, weighted_sum)
euc_scores_mcent_u <- cval(u100k, 10, krange,  gen_euc_sim, mean_centered)
euc_scores_z_u <- cval(u100k, 10, krange,  gen_euc_sim, z_score)
euc_scores_disc_u <- cval(u100k, 10, krange,  gen_euc_sim, discrete)

scores <- rbind(euc_scores_wsum_u, euc_scores_mcent_u,
                euc_scores_z_u, euc_scores_disc_u)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mcent_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(5)[3])
lines(krange, euc_scores_disc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mcent_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, euc_scores_disc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mcent_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, euc_scores_disc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

# evaluate cosine, adjusted cosine, and pcc
euc_scores_wsum_i <- cval(u100k, 10, krange, gen_euc_sim, weighted_sum, FALSE)
euc_scores_mean_i <- cval(u100k, 10, krange,  gen_euc_sim, mean_centered, FALSE)
euc_scores_z_i <- cval(u100k, 10, krange,  gen_euc_sim, z_score, FALSE)
euc_scores_disc_i <- cval(u100k, 10, krange,  gen_euc_sim, discrete, FALSE)

scores <- rbind(euc_scores_wsum_i, euc_scores_mean_i,
                euc_scores_z_i, euc_scores_disc_i)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mean_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, euc_scores_disc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mean_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, euc_scores_disc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, euc_scores_wsum_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, euc_scores_mean_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, euc_scores_z_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, euc_scores_disc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)