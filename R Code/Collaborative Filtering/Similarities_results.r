# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

# evaluate cosine, adjusted cosine, and pcc
cos_u <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum)
acos_u <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum)
pcc_u <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum)
jacc_u <- cval(ml100k, 10, krange, gen_jacc_sim, weighted_sum)
euc_u <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum)
mhat_u <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum)
cheb_u <- cval(ml100k, 10, krange, gen_cheb_sim, weighted_sum)
ups_u <- cval(ml100k, 10, krange, gen_ups_sim, weighted_sum)

sim_u <- rbind(cos_u, acos_u, pcc_u, jacc_u, euc_u, mhat_u, cheb_u, ups_u)

sim_u <- cbind(metric = c(rep("cosine", n), rep("acosine", n),
                          rep("pcc", n), rep("jaccard", n),
                          rep("euclidean", n), rep("manhattan", n),
                          rep("chebyshev", n), rep("ups", n)), sim_u)

# write user similarity results into file
write.csv(sim_u, file = "M4R_Clustering/Results/sim_u.csv",
          row.names = FALSE)

ymax <- max(sim_u$rmse)
ymin <- min(sim_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$mae)
ymin <- min(sim_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$r2)
ymin <- min(sim_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$online)
ymin <- min(sim_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

# evaluate cosine, adjusted cosine, and pcc
cos_i <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum, FALSE)
acos_i <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
pcc_i <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum, FALSE)
jacc_i <- cval(ml100k, 10, krange, gen_jacc_sim, weighted_sum, FALSE)
euc_i <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum, FALSE)
mhat_i <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum, FALSE)
cheb_i <- cval(ml100k, 10, krange, gen_cheb_sim, weighted_sum, FALSE)
ups_i <- cval(ml100k, 10, krange, gen_ups_sim, weighted_sum, FALSE)

sim_i <- rbind(cos_i, acos_i, pcc_i, jacc_i, euc_i, mhat_i, cheb_i, ups_i)

sim_i <- cbind(metric = c(rep("cosine", n), rep("acosine", n),
                          rep("pcc", n), rep("jaccard", n),
                          rep("euclidean", n), rep("manhattan", n),
                          rep("chebyshev", n), rep("ups", n)), sim_i)

# write user similarity results into file
write.csv(sim_i, file = "M4R_Clustering/Results/sim_i.csv",
          row.names = FALSE)

ymax <- max(sim_i$rmse)
ymin <- min(sim_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$mae)
ymin <- min(sim_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$r2)
ymin <- min(sim_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$online)
ymin <- min(sim_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(8)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[2])
lines(krange, pcc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[3])
lines(krange, jacc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[4])
lines(krange, euc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[5])
lines(krange, mhat_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[6])
lines(krange, cheb_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[7])
lines(krange, ups_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = viridis(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)