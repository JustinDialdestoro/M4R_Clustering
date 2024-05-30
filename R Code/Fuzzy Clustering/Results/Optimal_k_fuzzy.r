# read in the data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_functions.r")
source("M4R_Clustering/R Code/Fuzzy Clustering/Fuzzy_clustering.r")

# find best number of clusters
gow_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_gow)
hl_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_hl)
kproto_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 1.2, fuzzy_kproto)
mk_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_mixed_k)
msk_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_mskmeans)
famd_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_famd, TRUE, 3)
mrk_obj_u <- best_n_fuzzy(ml100k_dem, 2:15, 2, fuzzy_mrkmeans, TRUE, 3)

# write results into a file
fclust_obj_u <- cbind(gow_obj_u, hl_obj_u, kproto_obj_u, mk_obj_u, msk_obj_u,
                      famd_obj_u, mrk_obj_u)
colnames(fclust_obj_u) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk")
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_u.csv"
write.csv(fclust_obj_u, file = loc, row.names = FALSE)

# find best number of clusters
gow_obj_i <- best_n_fuzzy(ml100k_feat_b, 2:15, 2, fuzzy_gow, FALSE)
hl_obj_i <- best_n_fuzzy(ml100k_feat_d, 2:15, 2, fuzzy_hl, FALSE)
kproto_obj_i <- best_n_fuzzy(ml100k_feat_c, 2:15, 1.2, fuzzy_kproto, FALSE)
mk_obj_i <- best_n_fuzzy(ml100k_feat_c, 2:15, 2, fuzzy_mixed_k, FALSE)
msk_obj_i <- best_n_fuzzy(ml100k_feat_d, 2:15, 2, fuzzy_mskmeans, FALSE)
famd_obj_i <- best_n_fuzzy(ml100k_feat_d, 2:15, 2, fuzzy_famd, FALSE, 8)
mrk_obj_i <- best_n_fuzzy(ml100k_feat_d, 2:15, 2, fuzzy_mrkmeans, FALSE, 8)

# write results into a file
fclust_obj_i <- cbind(gow_obj_i, hl_obj_i, kproto_obj_i, mk_obj_i, msk_obj_i,
                      famd_obj_i, mrk_obj_i)
colnames(fclust_obj_i) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk")
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_i.csv"
write.csv(fclust_obj_i, file = loc, row.names = FALSE)