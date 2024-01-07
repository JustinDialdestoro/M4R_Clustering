# read in the data
u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 100, by = 10)

# evaluate cosine, adjusted cosine, and pcc
euc_scores_1 <- cval(u100k, 10, gen_euc_sim, weighted_sum, krange)
euc_scores_2 <- cval(u100k, 10, gen_euc_sim, mean_centered, krange)
euc_scores_3 <- cval(u100k, 10, gen_euc_sim, z_score, krange)
euc_scores_4 <- cval(u100k, 10, gen_euc_sim, discrete, krange)

plot(krange, euc_scores_1$rmse, type = "l", col = viridis(4)[1], lwd = 2,
     ylim = c(0.95, 1.105))
lines(krange, euc_scores_2$rmse, type = "l", col = viridis(4)[2], lwd = 2)
lines(krange, euc_scores_3$rmse, type = "l", col = viridis(4)[3], lwd = 2)
lines(krange, euc_scores_4$rmse, type = "l", col = viridis(4)[4], lwd = 2)
legend("right", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lwd = 2, cex = 1)

plot(krange, euc_scores_1$mae, type = "l", col = viridis(4)[1], lwd = 2,
     ylim = c(0.74, 0.8))
lines(krange, euc_scores_2$mae, type = "l", col = viridis(4)[2], lwd = 2)
lines(krange, euc_scores_3$mae, type = "l", col = viridis(4)[3], lwd = 2)
lines(krange, euc_scores_4$mae, type = "l", col = viridis(4)[4], lwd = 2)
legend("right", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lwd = 2, cex = 1)

plot(krange, euc_scores_1$r2, type = "l", col = viridis(4)[1], lwd = 2,
     ylim = c(0.15, 0.29))
lines(krange, euc_scores_2$r2, type = "l", col = viridis(4)[2], lwd = 2)
lines(krange, euc_scores_3$r2, type = "l", col = viridis(4)[3], lwd = 2)
lines(krange, euc_scores_4$r2, type = "l", col = viridis(4)[4], lwd = 2)
legend("right", c("weighted sum", "mean centered", "z score", "discrete"),
       col = viridis(4), lwd = 2, cex = 1)