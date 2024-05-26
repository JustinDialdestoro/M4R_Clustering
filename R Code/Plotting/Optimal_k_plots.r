# load packages
library("scales")

# read in the data
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_u.csv"
clust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_i.csv"
clust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_u.csv"
mclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_i.csv"
mclust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_u.csv"
fclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_i.csv"
fclust_obj_i <- read.csv(loc)

# initialise plotting variables
n_range <- 2:15

# plot user clustering objective
plot(n_range, clust_obj_u$x, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "n clusters",
     ylab = "Average within-cluster sum of squares")

# plot item clustering objective
plot(n_range, clust_obj_i$x, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(2)[1], xlab = "n clusters",
     ylab = "Average within-cluster sum of squares")

# plot user mixed clustering objectives
for (i in 1:8) {
  plot(2:15, mclust_obj_u[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}

# plot item mixed clustering objectives
for (i in 1:8) {
  plot(2:15, mclust_obj_i[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}

# plot user fuzzy mixed clustering objectives
for (i in 1:7) {
  plot(2:15, fclust_obj_u[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}

# plot item fuzzy mixed clustering objectives
for (i in 1:7) {
  plot(2:15, fclust_obj_i[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}