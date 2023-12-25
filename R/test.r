source("M4R_Clustering/R/CF.r")
source("M4R_Clustering/R/Metrics.r")

u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

t1 <- Sys.time()
f_ind <- t_fold_index(u100k, 10)
f <- t_fold(u100k, f_ind)
t2 <- Sys.time()

t1 <- Sys.time()
ui <- gen_ui_matrix(u100k, f[[1]])
t2 <- Sys.time()

t1 <- Sys.time()
sim <- gen_cos_sim_2((ui))
t2 <- Sys.time()

print(t2-t1)