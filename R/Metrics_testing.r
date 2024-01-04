# read in the data
u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

# call functions
source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)

# evaluate cosine, adjusted cosine, and pcc
cos_scores <- cross_val(u100k, 10, krange, gen_cos_sim, weighted_sum)
acos_scores <- cross_val(u100k, 10, krange, gen_acos_sim, weighted_sum)
pcc_scores <- cross_val(u100k, 10, krange, gen_pcc_sim, weighted_sum)
jacc_scores <- cross_val(u100k, 10, krange, gen_jacc_sim, weighted_sum)
euc_scores <- cross_val(u100k, 10, krange, gen_euc_sim, weighted_sum)
mhat_scores <- cross_val(u100k, 10, krange, gen_mhat_sim, weighted_sum)
cheb_scores <- cross_val(u100k, 10, krange, gen_cheb_sim, weighted_sum)

library("viridis")

plot(krange, cos_scores$rmse, type = "l", col = viridis(7)[1], lwd = 2,
     ylim = c(0.955, 1.05))
lines(krange, acos_scores$rmse, type = "l", col = viridis(7)[2], lwd = 2)
lines(krange, pcc_scores$rmse, type = "l", col = viridis(7)[3], lwd = 2)
lines(krange, jacc_scores$rmse, type = "l", col = viridis(7)[4], lwd = 2)
lines(krange, euc_scores$rmse, type = "l", col = viridis(7)[5], lwd = 2)
lines(krange, mhat_scores$rmse, type = "l", col = viridis(7)[6], lwd = 2)
lines(krange, cheb_scores$rmse, type = "l", col = viridis(7)[7], lwd = 2)
legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
                        "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lwd = 2, cex = 1)

plot(krange, cos_scores$mae, type = "l", col = viridis(7)[1], lwd = 2,
     ylim = c(0.75, 0.825))
lines(krange, acos_scores$mae, type = "l", col = viridis(7)[2], lwd = 2)
lines(krange, pcc_scores$mae, type = "l", col = viridis(7)[3], lwd = 2)
lines(krange, jacc_scores$mae, type = "l", col = viridis(7)[4], lwd = 2)
lines(krange, euc_scores$mae, type = "l", col = viridis(7)[5], lwd = 2)
lines(krange, mhat_scores$mae, type = "l", col = viridis(7)[6], lwd = 2)
lines(krange, cheb_scores$mae, type = "l", col = viridis(7)[7], lwd = 2)
legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
                        "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lwd = 2, cex = 1)

plot(krange, cos_scores$r2, type = "l", col = viridis(7)[1], lwd = 2,
     ylim = c(0.165, 0.28))
lines(krange, acos_scores$r2, type = "l", col = viridis(7)[2], lwd = 2)
lines(krange, pcc_scores$r2, type = "l", col = viridis(7)[3], lwd = 2)
lines(krange, jacc_scores$r2, type = "l", col = viridis(7)[4], lwd = 2)
lines(krange, euc_scores$r2, type = "l", col = viridis(7)[5], lwd = 2)
lines(krange, mhat_scores$r2, type = "l", col = viridis(7)[6], lwd = 2)
lines(krange, cheb_scores$r2, type = "l", col = viridis(7)[7], lwd = 2)
legend("topright", c("cosine", "adjusted cosine", "pearson's correlation",
                     "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lwd = 2, cex = 1)