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

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)

# # evaluate cosine, adjusted cosine, and pcc
# cos_scores_u <- cval(u100k, 10, krange, gen_cos_sim, weighted_sum)
# acos_scores_u <- cval(u100k, 10, krange, gen_acos_sim, weighted_sum)
# pcc_scores_u <- cval(u100k, 10, krange, gen_pcc_sim, weighted_sum)
# jacc_scores_u <- cval(u100k, 10, krange, gen_jacc_sim, weighted_sum)
# euc_scores_u <- cval(u100k, 10, krange, gen_euc_sim, weighted_sum)
# mhat_scores_u <- cval(u100k, 10, krange, gen_mhat_sim, weighted_sum)
# cheb_scores_u <- cval(u100k, 10, krange, gen_cheb_sim, weighted_sum)

# scores <- rbind(cos_scores_u, acos_scores_u, pcc_scores_u, jacc_scores_u,
#                 euc_scores_u, mhat_scores_u, cheb_scores_u)

# ymax <- max(scores$rmse)
# ymin <- min(scores$rmse)
# ygap <- 0.2 * (ymax - ymin)

# plot(krange, cos_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#      cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "RMSE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(krange, acos_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[2])
# lines(krange, pcc_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[3])
# lines(krange, jacc_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, cex = 0.5, col = viridis(7)[4])
# lines(krange, euc_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[5])
# lines(krange, mhat_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[6])
# lines(krange, cheb_scores_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[7])
# legend("bottom", c("cosine", "adjusted cosine", "pearson's correlation",
#                    "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

# ymax <- max(scores$mae)
# ymin <- min(scores$mae)
# ygap <- 0.2 * (ymax - ymin)

# plot(krange, cos_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#      cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(krange, acos_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[2])
# lines(krange, pcc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[3])
# lines(krange, jacc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[4])
# lines(krange, euc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[5])
# lines(krange, mhat_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[6])
# lines(krange, cheb_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[7])
# legend("bottom", c("cosine", "adjusted cosine", "pearson's correlation",
#                    "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

# ymax <- max(scores$r2)
# ymin <- min(scores$r2)
# ygap <- 0.2 * (ymax - ymin)

# plot(krange, cos_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#      cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(krange, acos_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[2])
# lines(krange, pcc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[3])
# lines(krange, jacc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[4])
# lines(krange, euc_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[5])
# lines(krange, mhat_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[6])
# lines(krange, cheb_scores_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       cex = 0.5, col = viridis(7)[7])
# legend("bottom", c("cosine", "adjusted cosine", "pearson's correlation",
#                    "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

# # evaluate cosine, adjusted cosine, and pcc
# cos_scores_i <- cval(u100k, 10, krange, gen_cos_sim, weighted_sum, FALSE)
# acos_scores_i <- cval(u100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
# pcc_scores_i <- cval(u100k, 10, krange, gen_pcc_sim, weighted_sum, FALSE)
# jacc_scores_i <- cval(u100k, 10, krange, gen_jacc_sim, weighted_sum, FALSE)
# euc_scores_i <- cval(u100k, 10, krange, gen_euc_sim, weighted_sum, FALSE)
# mhat_scores_i <- cval(u100k, 10, krange, gen_mhat_sim, weighted_sum, FALSE)
# cheb_scores_i <- cval(u100k, 10, krange, gen_cheb_sim, weighted_sum, FALSE)

scores <- rbind(cos_scores_i, acos_scores_i, pcc_scores_i, jacc_scores_i,
                euc_scores_i, mhat_scores_i, cheb_scores_i)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_scores_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_scores_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)


ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_scores_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)
