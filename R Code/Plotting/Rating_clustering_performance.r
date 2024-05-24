# load packages
library("scales")

# read in the data
pred_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_u.csv")
clust_u <- read.csv("M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv")
pred_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_i.csv")
clust_i <- read.csv("M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv")

# initialise plotting variables
krange <- krange <- seq(from = 10, to = 300, by = 10)
metric_labels <- c("RMSE", "MAE", "R2", "Online phase time (seconds)")

# prepare results
noclust_u <- pred_u[pred_u$predictor == "mean centred", ][2:6]
full <- rbind(clust_u, noclust_u)

for (i in 1:4) {
  # set plot boundaries
  ymax <- max(full[, i])
  ymin <- min(full[, i])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, noclust_u[, i], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(2)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - ygap, ymax + ygap))
  lines(krange, clust_u[, i], lty = 1, type = "l", lwd = 2,
        col = hue_pal()(2)[2])
  legend("bottom", c("no clustering", "kmeans clustering"),
         col = c(hue_pal()(2)), lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)
}

# prepare results
noclust_i <- pred_i[pred_i$predictor == "mean centred", ][2:6]
full <- rbind(clust_i, noclust_i)

for (i in 1:4) {
  # set plot boundaries
  ymax <- max(full[, i])
  ymin <- min(full[, i])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, noclust_i[, i], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(2)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - ygap, ymax + ygap))
  lines(krange, clust_i[, i], lty = 1, type = "l", lwd = 2,
        col = hue_pal()(2)[2])
  legend("bottom", c("no clustering", "kmeans clustering"),
         col = c(hue_pal()(2)), lty = 1, lwd = 2, cex = 0.8, horiz = TRUE)
}