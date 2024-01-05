u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

nrange <- seq(from = 2, to = 10)

euc_scores <- cval_rating_clust(u100k, 10, 30, nrange,
                                gen_euc_sim, weighted_sum)

library("viridis")

scores <- euc_scores

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "n clusters", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "n clusters", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(4)[1], xlab = "n clusters", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
