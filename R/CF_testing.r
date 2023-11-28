data <- read.table("M4R_Clustering/Data/u.data",
                   col.names = c("userID", "filmID", "rating", "timestamp"))

data2 <- read.csv("M4R_Clustering/Data/ratings.dat", sep=":",
                  colClasses = c(NA, "NULL"), header = FALSE)

colnames(data2) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

krange <- seq(from = 10, to = 300, by = 10)

cos_scores <- cross_val(data, 10, gen_cos_sim, krange)

plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2)
plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2)
plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2)