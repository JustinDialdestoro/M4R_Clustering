# load colour package
library("scales")

# read in the data
sim_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_u.csv")
sim_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_i.csv")

ymax <- max(sim_u$rmse)
ymin <- min(sim_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$mae)
ymin <- min(sim_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$r2)
ymin <- min(sim_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_u$online)
ymin <- min(sim_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$rmse)
ymin <- min(sim_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$mae)
ymin <- min(sim_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$r2)
ymin <- min(sim_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(sim_i$online)
ymin <- min(sim_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, cos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(8)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, acos_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[2])
lines(krange, pcc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[3])
lines(krange, jacc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[4])
lines(krange, euc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[5])
lines(krange, mhat_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[6])
lines(krange, cheb_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[7])
lines(krange, ups_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(8)[8])
legend("bottom", c("cosine", "adjusted cosine", "PCC", "jaccard",
                   "euclidean", "manhattan", "chebyshev", "UPS"),
       col = hue_pal()(8), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pred_u$rmse)
ymin <- min(pred_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$mae)
ymin <- min(pred_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$mae, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$r2)
ymin <- min(pred_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$r2, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$online)
ymin <- min(pred_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$online, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pred_i$rmse)
ymin <- min(pred_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$rmse, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$mae)
ymin <- min(pred_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$mae, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$mae, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$r2)
ymin <- min(pred_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$r2, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$r2, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$online)
ymin <- min(pred_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$online, lty = 1, type = "l", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$online, lty = 1, type = "l", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 1, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)
