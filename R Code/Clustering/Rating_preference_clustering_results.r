u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

krange <- seq(from = 10, to = 100, by = 10)

ups_scores <- cval(u100k, 10, krange, gen_ups_sim, weighted_sum)
ups_scores_2 <- cval(u100k, 10, krange, gen_ups_sim, mean_centered)
ups_scores_3 <- cval(u100k, 10, krange, gen_ups_sim, z_score)
ups_scores_uclust <- cval_pref_clust(u100k, 10, krange,
                                     gen_ups_sim, weighted_sum, ups_clust)

library("viridis")

scores <- rbind(ups_scores, ups_scores_2, ups_scores_3, ups_scores_uclust)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, ups_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, ups_scores_2$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, ups_scores_3$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, ups_scores_uclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottomleft",
       c("weighted sum", "mean centred", "z-score", "user rating clustering"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, ups_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, ups_scores_2$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, ups_scores_3$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, ups_scores_uclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottomleft",
       c("weighted sum", "mean centred", "z-score", "user rating clustering"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, ups_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, ups_scores_2$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[2])
lines(krange, ups_scores_3$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[3])
lines(krange, ups_scores_uclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(4)[4])
legend("bottomleft",
       c("weighted sum", "mean centred", "z-score", "user rating clustering"),
       col = viridis(4), lty = 2, pch = 4, lwd = 2, cex = 1)
