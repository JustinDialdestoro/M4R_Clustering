u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R/CF_clust1.r")
source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")
source("M4R_Clustering/R/Predictors.r")

krange <- seq(from = 10, to = 300, by = 10)

ups_scores <- cross_val(u100k, 10, krange, gen_ups_sim, weighted_sum)
# ups_scores_uclust <- cross_val_clust1(u100k, 10, krange,
#                                       gen_ups_sim, weighted_sum, ups_clust)

plot(krange, ups_scores$rmse, type = "l", col = "red", lwd = 2,
     ylim = c(0.898, 0.99))
lines(krange, ups_scores_uclust$rmse, type = "l", col = "blue", lwd = 2)
legend("right", c("no clustering", "user rating clustering"),
       col = c("red", "blue"), lwd = 2, cex = 1)

plot(krange, ups_scores$mae, type = "l", col = "red", lwd = 2,
     ylim = c(0.708, 0.787))
lines(krange, ups_scores_uclust$mae, type = "l", col = "blue", lwd = 2)
legend("right", c("no clustering", "user rating clustering"),
       col = c("red", "blue"), lwd = 2, cex = 1)

plot(krange, ups_scores$r2, type = "l", col = "red", lwd = 2,
     ylim = c(0.225, 0.39))
lines(krange, ups_scores_uclust$r2, type = "l", col = "blue", lwd = 2)
legend("right", c("no clustering", "user rating clustering"),
       col = c("red", "blue"), lwd = 2, cex = 1)