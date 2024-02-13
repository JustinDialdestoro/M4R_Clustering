# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

krange <- seq(from = 10, to = 100, by = 10)
nk <- length(krange)

n_range <- 2:10
uclust <- NULL

for (n in n_range) {
  results <- cval_clust(ml100k, 10, n, krange, gen_acos_sim,
                        euc_clust, mean_centered)
  results <- cbind(n = rep(n, nk), results)

  uclust <- rbind(uclust, results)
}

# write beta comparison results into file
write.csv(uclust, file = "M4R_Clustering/Results/uclust.csv",
          row.names = FALSE)

ymax <- max(uclust$rmse)
ymin <- min(uclust$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, uclust[1:10, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, uclust[(10 * i + 1):(10 * (i + 1)), ]$rmse, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(uclust$mae)
ymin <- min(uclust$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, uclust[1:10, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, uclust[(10 * i + 1):(10 * (i + 1)), ]$mae, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(uclust$r2)
ymin <- min(uclust$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, uclust[1:10, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, uclust[(10 * i + 1):(10 * (i + 1)), ]$r2, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(uclust$online)
ymin <- min(uclust$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, uclust[1:10, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, uclust[(10 * i + 1):(10 * (i + 1)), ]$online, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

iclust <- NULL

for (n in n_range) {
  results <- cval_clust(ml100k, 10, n, krange, gen_ups_sim,
                        euc_clust, mean_centered, FALSE)
  results <- cbind(n = rep(n, nk), results)

  iclust <- rbind(iclust, results)
}

# write beta comparison results into file
write.csv(iclust, file = "M4R_Clustering/Results/iclust.csv",
          row.names = FALSE)

ymax <- max(iclust$rmse)
ymin <- min(iclust$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, iclust[1:10, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, iclust[(10 * i + 1):(10 * (i + 1)), ]$rmse, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(iclust$mae)
ymin <- min(iclust$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, iclust[1:10, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, iclust[(10 * i + 1):(10 * (i + 1)), ]$mae, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(iclust$r2)
ymin <- min(iclust$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, iclust[1:10, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, iclust[(10 * i + 1):(10 * (i + 1)), ]$r2, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)

ymax <- max(iclust$online)
ymin <- min(iclust$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, iclust[1:10, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(9)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:8) {
  lines(krange, iclust[(10 * i + 1):(10 * (i + 1)), ]$online, lty = 1,
        type = "l", lwd = 2, col = hue_pal()(9)[i + 1])
}
legend("topright", c("n=2", "n=3", "n=4", "n=5", "n=6",
                     "n=7", "n=8", "n=9", "n=10"),
       col = hue_pal()(9), lty = 1, lwd = 2, cex = 0.8)
