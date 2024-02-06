# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

wsum_u <- cval_pref_clust(ml100k, 10, krange,
                          gen_ups_sim, weighted_sum, acos_clust)
mcent_u <- cval_pref_clust(ml100k, 10, krange,
                           gen_ups_sim, mean_centered, acos_clust)
zscore_u <- cval_pref_clust(ml100k, 10, krange,
                            gen_ups_sim, z_score, acos_clust)
disc_u <- cval_pref_clust(ml100k, 10, krange,
                          gen_ups_sim, discrete, acos_clust)

pref_clust_u <- rbind(wsum_u, mcent_u, zscore_u, disc_u)

pref_clust_u <- cbind(predictor = c(rep("weighted sum", n),
                                    rep("mean centred", n),
                                    rep("z score", n),
                                    rep("discrete", n)), pref_clust_u)

# write item predictor results into file
write.csv(pref_clust_u, file = "M4R_Clustering/Results/pref_clust_u.csv",
          row.names = FALSE)

ymax <- max(pref_clust_u$rmse)
ymin <- min(pref_clust_u$rmse)
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
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$mae)
ymin <- min(pref_clust_u$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$r2)
ymin <- min(pref_clust_u$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, wsum_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = hue_pal()(4)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(krange, mcent_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[2])
lines(krange, zscore_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[3])
lines(krange, disc_u$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = hue_pal()(4)[4])
legend("bottom", c("weighted sum", "mean centered", "z score", "discrete"),
       col = hue_pal()(4), lty = 2, pch = 4, lwd = 2, cex = 0.8, horiz = TRUE)

ymax <- max(pref_clust_u$online)
ymin <- min(pref_clust_u$online)
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
