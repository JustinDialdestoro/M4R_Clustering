source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

t1 <- Sys.time()
f_ind <- t_fold_index(ml100k, 10)
f <- t_fold(ml100k, f_ind)
t2 <- Sys.time()

t1 <- Sys.time()
ui <- gen_ui_matrix(ml100k, f[[1]])
t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_cos_sim((ui))
# t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_acos_sim((ui))
# t2 <- Sys.time()

# t1 <- Sys.time()
# pred_fold(u100k, f_ind[[1]], ui, sim, z_score, 10)
# t2 <- Sys.time()

m <- matrix(NA, 5, 5)
m[1, ] <- c(5, 3, 4, 4, NA)
m[2, ] <- c(3, 1, 2, 3, 3)
m[3, ] <- c(4, 3, 4, 3, 5)
m[4, ] <- c(3, 3, 1, 5, 4)
m[5, ] <- c(1, 5, 5, 2, 1)

m2 <- matrix(NA, 5, 9)
m2[1, ] <- c(1, 2, NA, 3, 2, NA, 2, NA, NA)
m2[2, ] <- c(2, 4, 4, NA, 4, NA, NA, 2, 3)
m2[3, ] <- c(5, 5, NA, 4, NA, 4, 3, NA, 4)
m2[4, ] <- c(5, NA, 5, 4, 4, NA, 4, 4, NA)
m2[5, ] <- c(1, NA, NA, NA, 2, NA, NA, NA, 2)

print(t2 - t1)