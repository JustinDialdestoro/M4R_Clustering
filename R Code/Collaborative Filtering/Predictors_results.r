# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

# # evaluate cosine, adjusted cosine, and pcc
wsum_u <- cval(ml100k, 10, krange,  gen_acos_sim, weighted_sum)
mcent_u <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered)
zscore_u <- cval(ml100k, 10, krange,  gen_acos_sim, z_score)
disc_u <- cval(ml100k, 10, krange,  gen_acos_sim, discrete)

pred_u <- rbind(wsum_u, mcent_u, zscore_u, disc_u)

pred_u <- cbind(predictor = c(rep("weighted sum", n), rep("mean centred", n),
                              rep("z score", n), rep("discrete", n)), pred_u)

# write user predictor results into file
write.csv(pred_u, file = "M4R_Clustering/Results/pred_u.csv",
          row.names = FALSE)

ymax <- max(pred_u$rmse)
ymin <- min(pred_u$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$mae)
ymin <- min(pred_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$r2)
ymin <- min(pred_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_u$online)
ymin <- min(pred_u$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

# evaluate cosine, adjusted cosine, and pcc
wsum_i <- cval(ml100k, 10, krange, gen_ups_sim, weighted_sum, FALSE)
mean_i <- cval(ml100k, 10, krange,  gen_ups_sim, mean_centered, FALSE)
zscore_i <- cval(ml100k, 10, krange,  gen_ups_sim, z_score, FALSE)
disc_i <- cval(ml100k, 10, krange,  gen_ups_sim, discrete, FALSE)

pred_i <- rbind(wsum_i, mean_i, zscore_i, disc_i)

pred_i <- cbind(predictor = c(rep("weighted sum", n), rep("mean centred", n),
                              rep("z score", n), rep("discrete", n)), pred_i)

# write item predictor results into file
write.csv(pred_i, file = "M4R_Clustering/Results/pred_i.csv",
          row.names = FALSE)

ymax <- max(pred_i$rmse)
ymin <- min(pred_i$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$mae)
ymin <- min(pred_i$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$r2)
ymin <- min(pred_i$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_i$online)
ymin <- min(pred_i$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mean_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_i$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)
