# read in the data
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering.r")

# find best number of clusters
gow_obj_u <- best_n(ml100k_dem, 2:15, gow_pam)
hl_obj_u <- best_n(ml100k_dem, 2:15, hl_pam)
kproto_obj_u <- best_n(ml100k_dem, 2:15, kprototypes)
mk_obj_u <- best_n(ml100k_dem, 2:15, mixed_k)
msk_obj_u <- best_n(ml100k_dem, 2:15, mskmeans)
famd_obj_u <- best_n(ml100k_dem, 2:15, famd, TRUE, 3)
mrk_obj_u <- best_n(ml100k_dem, 2:15, mrkmeans, TRUE, 3)
kam_obj_u <- best_n(ml100k_dem, 2:15, kamila_clust)

# write results into a file
mclust_obj_u <- cbind(gow_obj_u, hl_obj_u, kproto_obj_u, mk_obj_u, msk_obj_u,
                      famd_obj_u, mrk_obj_u, kam_obj_u)
colnames(mclust_obj_u) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk", "kam")
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_u.csv"
write.csv(mclust_obj_u, file = loc, row.names = FALSE)

# find best number of clusters
gow_obj_i <- best_n(ml100k_feat_b, 2:15, gow_pam, FALSE)
hl_obj_i <- best_n(ml100k_feat_d, 2:15, hl_pam, FALSE)
kproto_obj_i <- best_n(ml100k_feat_b, 2:15, kprototypes, FALSE)
mk_obj_i <- best_n(ml100k_feat_c, 2:15, mixed_k, FALSE)
msk_obj_i <- best_n(ml100k_feat_d, 2:15, mskmeans, FALSE)
famd_obj_i <- best_n(ml100k_feat_d, 2:15, famd, FALSE, 8)
mrk_obj_i <- best_n(ml100k_feat_d, 2:15, mrkmeans, FALSE, 8)
kam_obj_i <- best_n(ml100k_feat_b, 2:15, kamila_clust, FALSE)

# write results into a file
mclust_obj_i <- cbind(gow_obj_i, hl_obj_i, kproto_obj_i, mk_obj_i, msk_obj_i,
                      famd_obj_i, mrk_obj_i, kam_obj_i)
colnames(mclust_obj_i) <- c("gow", "hl", "kproto", "mk",
                            "msk", "famd", "mrk", "kam")
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_i.csv"
write.csv(mclust_obj_i, file = loc, row.names = FALSE)