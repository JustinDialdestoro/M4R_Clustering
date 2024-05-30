# load packages
library("scales")

# read in the data
sim_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_u.csv")
sim_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_i.csv")
pred_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_u.csv")
pred_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_i.csv")

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
metric_labels <- c("RMSE", "MAE", "R2", "Time (seconds)")
index <- c(2, 3, 4, 6)
titles <- c("Average RMSE", "Average MAE", "Average R2", "Online Phase")

for (i in 1:4) {
  ymax <- max(sim_u[, index[i]])
  ymin <- min(sim_u[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, sim_u[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - 1.75 * ygap, ymax + ygap), main = titles[i],
       panel.first = grid(col = "#dddddd", lty = 1))

  for (j in 1:4) {
    lines(krange, sim_u[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 1])
  }

  legend("bottom",
         c("Cosine", "Adjusted Cosine", "PCC", "Euclidean", "Manhattan"),
         col = c(hue_pal()(10)), ncol = 5,
         lty = 1, lwd = 2, cex = 0.7)
}

for (i in 1:4) {
  ymax <- max(sim_i[, index[i]])
  ymin <- min(sim_i[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, sim_i[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - 1.5 * ygap, ymax + ygap), main = titles[i],
       panel.first = grid(col = "#dddddd", lty = 1))

  for (j in 1:4) {
    lines(krange, sim_i[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 1])
  }

  legend("bottom",
         c("Cosine", "Adjusted Cosine", "PCC", "Euclidean", "Manhattan"),
         col = c(hue_pal()(10)), ncol = 5,
         lty = 1, lwd = 2, cex = 0.7)
}

titles <- c("Average RMSE", "Average MAE", "Average R2", "Online Phase")

for (i in 1:4) {
  ymax <- max(pred_u[, index[i]])
  ymin <- min(pred_u[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, pred_u[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[6], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - 1.5 * ygap, ymax + ygap), main = titles[i],
       panel.first = grid(col = "#dddddd", lty = 1))

  for (j in 1:4) {
    lines(krange, pred_u[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 6])
  }

  legend("bottom",
         c("Mean", "Weighted Mean", "Mean Centred", "Z-Score", "Discrete"),
         col = c(hue_pal()(10)[6:10]), ncol = 5,
         lty = 1, lwd = 2, cex = 0.7)
}

for (i in 1:4) {
  ymax <- max(pred_i[, index[i]])
  ymin <- min(pred_i[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, pred_i[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[6], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - 1.5 * ygap, ymax + ygap), main = titles[i],
       panel.first = grid(col = "#dddddd", lty = 1))

  for (j in 1:4) {
    lines(krange, pred_i[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 6])
  }

  legend("bottom",
         c("Mean", "Weighted Mean", "Mean Centred", "Z-Score", "Discrete"),
         col = c(hue_pal()(10)[6:10]), ncol = 5,
         lty = 1, lwd = 2, cex = 0.7)
}