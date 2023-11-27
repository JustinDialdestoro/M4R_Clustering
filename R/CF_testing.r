data <- read.table("M4R_Clustering/Data/u.data",
                   col.names = c("userID", "filmID", "rating", "timestamp"))

data2 <- read.csv("M4R_Clustering/Data/ratings.dat", sep=":",
                  colClasses = c(NA, "NULL"), header = FALSE)

colnames(data2) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

krange <- seq(from = 10, to = 300, by = 10)

rmse_cos <- cross_val(data, 10, gen_cos_sim, krange)
rmse_pcc <- cross_val(data, 10, gen_pcc_sim, krange)

plot(krange, rmse_cos / 10, type = "l", col = "red", lwd = 2,
     ylim = c(1.03, 1.3))
lines(krange, rmse_pcc / 10, type = "l", col = "blue", lwd = 2)