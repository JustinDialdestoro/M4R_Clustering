# read in the data
u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

# call functions
source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)

# evaluate cosines
cos_scores <- cross_val(u100k, 10, gen_cos_sim, krange)
cos2_scores <- cross_val(u100k, 10, gen_cos_sim_2, krange)

plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
     ylim = c(1.005, 1.05))
lines(krange, cos2_scores$rmse, type = "l", col = "blue", lwd = 2)
legend("topright", c("cosine", "modified cosine"),
       col = c("red", "blue"),
       lwd = 2, cex = 0.8)

plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
     ylim = c(0.8, 0.825))
lines(krange, cos2_scores$mae, type = "l", col = "blue", lwd = 2)
legend("topright", c("cosine", "modified cosine"),
       col = c("red", "blue"),
       lwd = 2, cex = 0.8)

plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
     ylim = c(0.17, 0.205))
lines(krange, cos2_scores$r2, type = "l", col = "blue", lwd = 2)
legend("topright", c("cosine", "modified cosine"),
       col = c("red", "blue"),
       lwd = 2, cex = 0.8)