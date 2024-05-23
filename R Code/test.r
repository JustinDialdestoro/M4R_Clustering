# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat <- read.csv("M4R_Clustering/Data/ml100k_feat_a.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Fuzzy_clustering.r")
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# t1 <- Sys.time()
# f_ind <- t_fold_index(ml100k, 10)
# f <- t_fold(ml100k, f_ind)
# t2 <- Sys.time()

# t1 <- Sys.time()
# ui <- gen_ui_matrix(ml100k, f[[1]])
# t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_cos_sim((ui))
# t2 <- Sys.time()

# t1 <- Sys.time()
# sim <- gen_acos_sim((ui))
# t2 <- Sys.time()

# t1 <- Sys.time()
# pred_fold(u100k, f_ind[[1]], ui, sim, z_score, 10)
# t2 <- Sys.time()

# m <- matrix(NA, 5, 5)
# m[1, ] <- c(5, 3, 4, 4, NA)
# m[2, ] <- c(3, 1, 2, 3, 3)
# m[3, ] <- c(4, 3, 4, 3, 5)
# m[4, ] <- c(3, 3, 1, 5, 4)
# m[5, ] <- c(1, 5, 5, 2, 1)

# m2 <- matrix(NA, 5, 9)
# m2[1, ] <- c(1, 2, NA, 3, 2, NA, 2, NA, NA)
# m2[2, ] <- c(2, 4, 4, NA, 4, NA, NA, 2, 3)
# m2[3, ] <- c(5, 5, NA, 4, NA, 4, 3, NA, 4)
# m2[4, ] <- c(5, NA, 5, 4, 4, NA, 4, 4, NA)
# m2[5, ] <- c(1, NA, NA, NA, 2, NA, NA, NA, 2)

# print(t2 - t1)

# pred_acos <- read.csv("M4R_Clustering/Results/pred_acos.csv")

# wsum_acos <- pred_acos[pred_acos$predictor == "weighted sum", ][2:6]
# mcent_acos <- pred_acos[pred_acos$predictor == "mean centred", ][2:6]
# zscore_acos <- pred_acos[pred_acos$predictor == "z score", ][2:6]
# disc_acos <- pred_acos[pred_acos$predictor == "discrete", ][2:6]

# pred_ups <- read.csv("M4R_Clustering/Results/pred_ups.csv")

# wsum_ups <- pred_ups[pred_ups$predictor == "weighted sum", ][2:6]
# mcent_ups <- pred_ups[pred_ups$predictor == "mean centred", ][2:6]
# zscore_ups <- pred_ups[pred_ups$predictor == "z score", ][2:6]
# disc_ups <- pred_ups[pred_ups$predictor == "discrete", ][2:6]

# pref_clust_acos <- read.csv("M4R_Clustering/Results/pref_clust_acos.csv")

# wsum_acos_c <- pref_clust_acos[pref_clust_acos$predictor == "weighted sum", ][2:6]
# mcent_acos_c <- pref_clust_acos[pref_clust_acos$predictor == "mean centred", ][2:6]
# zscore_acos_c <- pref_clust_acos[pref_clust_acos$predictor == "z score", ][2:6]
# disc_acos_c <- pref_clust_acos[pref_clust_acos$predictor == "discrete", ][2:6]

# pref_clust_ups <- read.csv("M4R_Clustering/Results/pref_clust_ups.csv")

# wsum_ups_c <- pref_clust_ups[pref_clust_ups$predictor == "weighted sum", ][2:6]
# mcent_ups_c <- pref_clust_ups[pref_clust_ups$predictor == "mean centred", ][2:6]
# zscore_ups_c <- pref_clust_ups[pref_clust_ups$predictor == "z score", ][2:6]
# disc_ups_c <- pref_clust_ups[pref_clust_ups$predictor == "discrete", ][2:6]

# a <- read.csv("M4R_Clustering/Results/Mixed clustering/One-sided/mclust_obj_u.csv")

# gow_obj_u <- a$gow
# hl_obj_u <- a$hl
# kproto_obj_u <- a$kproto[1:14]
# mk_obj_u <- a$mk[1:14]
# msk_obj_u <- a$msk[1:14]
# kam_obj_u <- a$kam[1:14]

# mclust_u <- read.csv("M4R_Clustering/Results/Mixed clustering/One-sided/mclust_u.csv")
# mclust_u$method <- c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
#                              rep("mk", 30), rep("msk", 30), rep("famd", 30),
#                              rep("mrk", 30), rep("kamila", 30))

# mclust_i <- read.csv("M4R_Clustering/Results/Mixed clustering/One-sided/mclust_i.csv")
# mclust_i$method <- c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
#                              rep("mk", 30), rep("msk", 30), rep("famd", 30),
#                              rep("mrk", 30), rep("kamila", 30))

# m <- cbind(c(1, 1, 2, 2, 3, 3, 4, 4, 9, 9, 10, 10, 8, 8, 9, 9, 4, 7, 8, 5, 5),
#            c(1, 2, 1, 2, 9, 10, 9, 10, 5, 6, 5, 6, 1, 2, 1, 2, 7, 8, 3, 2, 5))
# test <- fuzzy_c_means(m, 2, 2)
# plot(m, type = "n")
# text(m)
# text(test$centroids, col = "red", cex = 1.5)

# test <- fuzzy_c_means(m, 3, 2)
# text(test$centroids, col = "blue", cex = 1.5)

# test <- fuzzy_c_means(m, 4, 2)
# text(test$centroids, col = "green", cex = 1.5)

# df <- data.frame(age = c(20, 21, 20, 45, 50, 48, 20),
#                  gender = c("M", "M", "M", "F", "F", "F", "M"),
#                  occupation = c("student", "student", "student",
#                                 "programmer", "programmer", "programmer",
#                                 "programmer"))

# test <- fuzzy_gow(ml100k_dem, 2, 2)
# print(test$u)
# print(test$centroids)