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

# evaluate adjusted cosine predictor performance
wsum_acos <- cval(ml100k, 10, krange,  gen_acos_sim, weighted_sum)
mcent_acos <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered)
zscore_acos <- cval(ml100k, 10, krange,  gen_acos_sim, z_score)
disc_acos <- cval(ml100k, 10, krange,  gen_acos_sim, discrete)

pred_acos <- rbind(wsum_acos, mcent_acos, zscore_acos, disc_acos)

pred_acos <- cbind(predictor = c(rep("weighted sum", n), rep("mean centred", n),
                                 rep("z score", n), rep("discrete", n)),
                   pred_acos)

# write user predictor results into file
write.csv(pred_acos, file = "M4R_Clustering/Results/pred_acos.csv",
          row.names = FALSE)

ymax <- max(pred_acos$rmse)
ymin <- min(pred_acos$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_acos$mae)
ymin <- min(pred_acos$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_acos$r2)
ymin <- min(pred_acos$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_acos$online)
ymin <- min(pred_acos$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_acos$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_acos$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_acos$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_acos$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

# evaluate adjusted cosine predictor performance
wsum_ups <- cval(ml100k, 10, krange,  gen_ups_sim, weighted_sum)
mcent_ups <- cval(ml100k, 10, krange,  gen_ups_sim, mean_centered)
zscore_ups <- cval(ml100k, 10, krange,  gen_ups_sim, z_score)
disc_ups <- cval(ml100k, 10, krange,  gen_ups_sim, discrete)

pred_ups <- rbind(wsum_ups, mcent_ups, zscore_ups, disc_ups)

pred_ups <- cbind(predictor = c(rep("weighted sum", n), rep("mean centred", n),
                                rep("z score", n), rep("discrete", n)),
                  pred_ups)

# write user predictor results into file
write.csv(pred_ups, file = "M4R_Clustering/Results/pred_ups.csv",
          row.names = FALSE)

ymax <- max(pred_ups$rmse)
ymin <- min(pred_ups$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_ups$mae)
ymin <- min(pred_ups$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_ups$r2)
ymin <- min(pred_ups$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(pred_ups$online)
ymin <- min(pred_ups$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_ups$online, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)", ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_ups$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_ups$online, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_ups$online, lty = 2, type = "b", pch = 4, lwd = 2,
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
