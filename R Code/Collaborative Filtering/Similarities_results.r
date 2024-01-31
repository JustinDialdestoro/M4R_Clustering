# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)

# evaluate cosine, adjusted cosine, and pcc
cos_u <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum)
acos_u <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum)
pcc_u <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum)
jacc_u <- cval(ml100k, 10, krange, gen_jacc_sim, weighted_sum)
euc_u <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum)
mhat_u <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum)
cheb_u <- cval(ml100k, 10, krange, gen_cheb_sim, weighted_sum)

scores_u <- rbind(cos_u, acos_u, pcc_u, jacc_u,
                  euc_u, mhat_u, cheb_u)

ymax <- max(scores_u$rmse)
ymin <- min(scores_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, cex = 0.5, col = viridis(7)[4])
lines(krange, euc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$mae)
ymin <- min(scores_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$r2)
ymin <- min(scores_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_u$online)
ymin <- min(scores_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, cex = 0.5, col = viridis(7)[4])
lines(krange, euc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

# evaluate cosine, adjusted cosine, and pcc
cos_i <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum, FALSE)
acos_i <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
pcc_i <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum, FALSE)
jacc_i <- cval(ml100k, 10, krange, gen_jacc_sim, weighted_sum, FALSE)
euc_i <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum, FALSE)
mhat_i <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum, FALSE)
cheb_i <- cval(ml100k, 10, krange, gen_cheb_sim, weighted_sum, FALSE)

scores_i <- rbind(cos_i, acos_i, pcc_i, jacc_i,
                  euc_i, mhat_i, cheb_i)

ymax <- max(scores_i$rmse)
ymin <- min(scores_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_i$mae)
ymin <- min(scores_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)


ymax <- max(scores_i$r2)
ymin <- min(scores_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[4])
lines(krange, euc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores_i$online)
ymin <- min(scores_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
     cex = 0.5, col = viridis(7)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[2])
lines(krange, pcc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[3])
lines(krange, jacc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, cex = 0.5, col = viridis(7)[4])
lines(krange, euc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[5])
lines(krange, mhat_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[6])
lines(krange, cheb_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      cex = 0.5, col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)
