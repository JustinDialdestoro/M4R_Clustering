source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")
source("M4R_Clustering/R/Predictors.r")

u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

t1 <- Sys.time()
f_ind <- t_fold_index(u100k, 10)
f <- t_fold(u100k, f_ind)
t2 <- Sys.time()

t1 <- Sys.time()
ui <- gen_ui_matrix(u100k, f[[1]])
t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_cos_sim((ui))
# t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_acos_sim((ui))
# t2 <- Sys.time()

t1 <- Sys.time()
sim <- gen_euc_sim((ui))
t2 <- Sys.time()

m <- matrix(1, 5, 5)
m[1, ] <- c(5, 3, 4, 4, NA)
m[2, ] <- c(3, 1, 2, 3, 3)
m[3, ] <- c(4, 3, 4, 3, 5)
m[4, ] <- c(3, 3, 1, 5, 4)
m[5, ] <- c(1, 5, 5, 2, 1)

print(t2 - t1)