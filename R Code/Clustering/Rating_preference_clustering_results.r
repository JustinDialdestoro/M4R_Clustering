# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

krange <- seq(from = 10, to = 100, by = 10)

wsum <- cval(ml100k, 10, krange, gen_ups_sim, weighted_sum)
mcent <- cval(ml100k, 10, krange, gen_ups_sim, mean_centered)
zscore <- cval(ml100k, 10, krange, gen_ups_sim, z_score)
disc <- cval(ml100k, 10, krange,  gen_ups_sim, discrete)

scores <- rbind(wsum, mcent, zscore, disc)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(5)[3])
lines(krange, disc$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(5)[3])
lines(krange, disc$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(5)[3])
lines(krange, disc$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$online)
ymin <- min(scores$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, zscore$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, disc$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

# write user predictor results into file
write.csv(scores, file = "M4R_Clustering/Results/pred_ups.csv",
          row.names = FALSE)

# ups_scores_uclust <- cval_pref_clust(ml100k, 10, krange,
#                                      gen_ups_sim, weighted_sum, ups_clust)
