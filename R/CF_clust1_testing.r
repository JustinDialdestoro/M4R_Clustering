u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R/CF_clust1.r")
source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

krange <- seq(from = 10, to = 300, by = 10)

cos_scores_c1 <- cross_val_c1(u100k, 10, gen_cos_sim, krange)
cos_scores <- cross_val(u100k, 10, gen_cos_sim, krange)

plot(krange, cos_scores_c1$rmse, type = "l", col = "blue", lwd = 2,
     ylim = c(1.03, 1.25))
lines(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2)
legend("topright", c("No clustering", "User rating clustering"),
       col = c("red", "blue"), lwd = 2)

plot(krange, cos_scores_c1$mae, type = "l", col = "blue", lwd = 2,
     ylim = c(0.83, 1))
lines(krange, cos_scores$mae, type = "l", col = "red", lwd = 2)
legend("topright", c("No clustering", "User rating clustering"),
       col = c("red", "blue"), lwd = 2)

plot(krange, cos_scores_c1$r2, type = "l", col = "blue", lwd = 2,
     ylim = c(0.04, 0.15))
lines(krange, cos_scores$r2, type = "l", col = "red", lwd = 2)
legend("bottomright", c("No clustering", "User rating clustering"),
       col = c("red", "blue"), lwd = 2)