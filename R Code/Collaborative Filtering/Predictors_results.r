# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)

# # evaluate cosine, adjusted cosine, and pcc
wsum_u <- cval(ml100k, 10, krange,  gen_acos_sim, weighted_sum)
mcent_u <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered)
zscore_u <- cval(ml100k, 10, krange,  gen_acos_sim, z_score)
disc_u <- cval(ml100k, 10, krange,  gen_acos_sim, discrete)

scores_u <- rbind(wsum_u, mcent_u, zscore_u, disc_u)

# write user predictor results into file
write.csv(scores_u, file = "M4R_Clustering/Results/pred_u.csv",
          row.names = FALSE)

ymax <- max(scores_u$rmse)
ymin <- min(scores_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(5)[3])
lines(krange, disc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$mae)
ymin <- min(scores_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$r2)
ymin <- min(scores_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$online)
ymin <- min(scores_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

# evaluate cosine, adjusted cosine, and pcc
wsum_i <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
mean_i <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered, FALSE)
zscore_i <- cval(ml100k, 10, krange,  gen_acos_sim, z_score, FALSE)
disc_i <- cval(ml100k, 10, krange,  gen_acos_sim, discrete, FALSE)

scores_i <- rbind(wsum_i, mean_i, zscore_i, disc_i)

# write item predictor results into file
write.csv(scores_i, file = "M4R_Clustering/Results/pred_i.csv",
          row.names = FALSE)

ymax <- max(scores_i$rmse)
ymin <- min(scores_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_i$mae)
ymin <- min(scores_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_i$r2)
ymin <- min(scores_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_i$online)
ymin <- min(scores_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)
