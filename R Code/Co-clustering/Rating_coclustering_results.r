# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Co-clustering/Rating_coclustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# initialise evaluation fixed variables
n_range <- 2:8
m_range <- 2:8

# best n and m search
coclust_nm <- NULL

for (n in n_range) {
  for (m in m_range) {
    results <- cval_coclust(ml100k, 10, n, m)
    results <- cbind(n = n, m = m, results)

    coclust_nm <- rbind(coclust_nm, results)
  }
}

# write n and m comparison results into file
write.csv(coclust_nm, file = 
            "M4R_Clustering/Results/Co-clustering/coclust.csv",
          row.names = FALSE)

rmse <- matrix(coclust_nm$rmse, nrow = 7, byrow = TRUE)
mae <- matrix(coclust_nm$mae, nrow = 7, byrow = TRUE)
r2 <- matrix(coclust_nm$r2, nrow = 7, byrow = TRUE)
times <- matrix(coclust_nm$online, nrow = 7, byrow = TRUE)

heatmap(rmse, Rowv = NA, Colv = NA, labRow = c(2:7), labCol = c(2:7),
        xlab = "Number of item clusters", ylab = "Number of user clusters")
heatmap(mae, Rowv = NA, Colv = NA, labRow = c(2:7), labCol = c(2:7),
        xlab = "Number of item clusters", ylab = "Number of user clusters")
heatmap(r2, Rowv = NA, Colv = NA, labRow = c(2:7), labCol = c(2:7),
        xlab = "Number of item clusters", ylab = "Number of user clusters")
heatmap(times, Rowv = NA, Colv = NA, labRow = c(2:7), labCol = c(2:7),
        xlab = "Number of item clusters", ylab = "Number of user clusters")