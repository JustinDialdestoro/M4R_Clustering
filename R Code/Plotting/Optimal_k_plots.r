# load packages
library("scales")

loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_u.csv"
mclust_obj_u <- read.csv(loc)

for (i in 1:8) {
  plot(2:15, mclust_obj_u[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}

loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_i.csv"
mclust_obj_i <- read.csv(loc)

for (i in 1:8) {
  plot(2:15, mclust_obj_i[, i], lty = 1, type = "o", lwd = 2, pch = 20,
       col = hue_pal()(8)[i], xlab = "n clusters",
       ylab = "Objective")
}

plot(2:15, mclust_obj_i[, i], lty = 1, type = "o", lwd = 2, pch = 20,
     col = hue_pal()(8)[i], xlab = "n clusters",
     ylab = "Objective")