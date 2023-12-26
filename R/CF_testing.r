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
cos_scores <- cross_val(u100k, 10, gen_cos_sim, krange)
acos_scores <- cross_val(u100k, 10, gen_acos_sim, krange)
pcc_scores <- cross_val(u100k, 10, gen_pcc_sim, krange)
euc_scores <- cross_val(u100k, 10, gen_euc_sim, krange)
mhat_scores <- cross_val(u100k, 10, gen_mhat_sim, krange)

library("viridis")

plot(krange, cos_scores$rmse, type = "l", col = viridis(5)[1], lwd = 2,
     ylim = c(0.955, 1.05))
lines(krange, acos_scores$rmse, type = "l", col = viridis(5)[2], lwd = 2)
lines(krange, pcc_scores$rmse, type = "l", col = viridis(5)[3], lwd = 2)
lines(krange, euc_scores$rmse, type = "l", col = viridis(5)[4], lwd = 2)
lines(krange, mhat_scores$rmse, type = "l", col = viridis(5)[5], lwd = 2)
legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
                     "euclidean", "manhattan"),
       col = viridis(5), lwd = 2, cex = 1)

plot(krange, cos_scores$mae, type = "l", col = viridis(5)[1], lwd = 2,
     ylim = c(0.75, 0.825))
lines(krange, acos_scores$mae, type = "l", col = viridis(5)[2], lwd = 2)
lines(krange, pcc_scores$mae, type = "l", col = viridis(5)[3], lwd = 2)
lines(krange, euc_scores$mae, type = "l", col = viridis(5)[4], lwd = 2)
lines(krange, mhat_scores$mae, type = "l", col = viridis(5)[5], lwd = 2)
legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
                        "euclidean", "manhattan"),
       col = viridis(5), lwd = 2, cex = 1)

plot(krange, cos_scores$r2, type = "l", col = viridis(5)[1], lwd = 2,
     ylim = c(0.16, 0.28))
lines(krange, acos_scores$r2, type = "l", col = viridis(5)[2], lwd = 2)
lines(krange, pcc_scores$r2, type = "l", col = viridis(5)[3], lwd = 2)
lines(krange, euc_scores$r2, type = "l", col = viridis(5)[4], lwd = 2)
lines(krange, mhat_scores$r2, type = "l", col = viridis(5)[5], lwd = 2)
legend("topright", c("cosine", "adjusted cosine", "pearson's correlation",
                     "euclidean", "manhattan"),
       col = viridis(5), lwd = 2, cex = 1)