# load colour package
library("scales")

# read in the data
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_u.csv"
fclust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_i.csv"
fclust_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_u.csv"
pred_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv"
clust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv"
clust_i <- read.csv(loc)

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
metric_labels <- c("RMSE", "MAE", "R2", "Online phase time (seconds)")
index <- c(2, 3, 4, 6)
best_n_u <- c(7, 5, 7, 5, 6, 6, 6, 5)
best_n_i <- c(3, 4, 7, 5, 6, 8, 9, 3)

# prepare results
noclust_u <- pred_u[61:90, ]
names(noclust_u)[1] <- "clustering"
noclust_u[1] <- "none"
clust_u <- cbind(clustering = "ratings", clust_u)
names(fclust_u)[1] <- "clustering"
full_u <- rbind(noclust_u, clust_u, fclust_u)

for (i in 1:4) {
  ymax <- max(full_u[, index[i]])
  ymin <- min(full_u[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, full_u[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - ygap, ymax + ygap))

  for (j in 1:10) {
    lines(krange, full_u[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 1])
  }

  legend("topright",
         c("no clustering", "rating clustering", "Gower/PAM", "HL/PAM",
           "K-prototypes", "Mixed K-Means", "MS K-Means", "FAMD", "Mixed RKM",
           "KAMILA"),
         col = c(hue_pal()(10)),
         lty = 1, lwd = 2, cex = 0.5)
}

# prepare results
noclust_i <- pred_i[61:90, ]
names(noclust_i)[1] <- "clustering"
noclust_i[1] <- "none"
clust_i <- cbind(clustering = "ratings", clust_i)
names(fclust_i)[1] <- "clustering"
full_i <- rbind(noclust_i, clust_i, fclust_i)

for (i in 1:4) {
  ymax <- max(full_i[, index[i]])
  ymin <- min(full_i[, index[i]])
  ygap <- 0.1 * (ymax - ymin)

  plot(krange, full_i[1:30, index[i]], lty = 1, type = "l", lwd = 2,
       col = hue_pal()(10)[1], xlab = "k neighbours", ylab = metric_labels[i],
       ylim = c(ymin - ygap, ymax + ygap))

  for (j in 1:10) {
    lines(krange, full_i[(j * 30 + 1):((j + 1) * 30), index[i]],
          lty = 1, type = "l", lwd = 2, col = hue_pal()(10)[j + 1])
  }

  legend("topright",
         c("no clustering", "rating clustering", "Gower/PAM", "HL/PAM",
           "K-prototypes", "Mixed K-Means", "MS K-Means", "FAMD", "Mixed RKM",
           "KAMILA"),
         col = c(hue_pal()(10)),
         lty = 1, lwd = 2, cex = 0.5)
}