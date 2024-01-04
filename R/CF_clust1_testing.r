u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

source("M4R_Clustering/R/CF_clust1.r")
source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")
source("M4R_Clustering/R/Metrics.r")

krange <- seq(from = 10, to = 300, by = 10)

euc_scores_1 <- cross_val_clust1(u100k, 10, krange,
                                 gen_euc_sim, z_score, euc_clust)