# load packages
library("scales")
library("Rtsne")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")
source("M4R_Clustering/R Code/Clustering/Clustering_predictors.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering.r")

# initialise evaluation fixed variables
krange <- seq(from = 10, to = 300, by = 10)
nrange <- 2:10
best_n <- c(7, 5, 7, 5, 6, 5, 6, 5)

# evaluate mixed clustering over a k range
gow_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                          mean_centered, gow_pam)
hl_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                         mean_centered, hl_pam)
kproto_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 7, krange, gen_acos_sim,
                             mean_centered, kprototypes)
mk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                         mean_centered, mixed_k)
msk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 6, krange, gen_acos_sim,
                          mean_centered, mskmeans)
famd_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                           mean_centered, famd)
mrk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 6, krange, gen_acos_sim,
                          mean_centered, mrkmeans)
kam_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                          mean_centered, kamila_clust)

# write user mixed clustering results into file
mclust_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)
mclust_u <- cbind(method = c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
                             rep("mk", 30), rep("msk", 30), rep("famd", 30),
                             rep("mrk", 30), rep("kam", 30)),
                  mclust_u)
write.csv(mclust_u, file =
            "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_u.csv",
          row.names = FALSE)

best_n <- c(3, 4, 7, 5, 6, 8, 9, 3)

# evaluate mixed clustering over a k range
gow_i <- cval_mixed_clust(ml100k, ml100k_feat_b, 10, 3, krange, gen_acos_sim,
                          mean_centered, gow_pam, FALSE)
hl_i <- cval_mixed_clust(ml100k, ml100k_feat_d, 10, 4, krange, gen_acos_sim,
                         mean_centered, hl_pam, FALSE)
kproto_i <- cval_mixed_clust(ml100k, ml100k_feat_c, 10, 7, krange, gen_acos_sim,
                             mean_centered, kprototypes, FALSE)
mk_i <- cval_mixed_clust(ml100k, ml100k_feat_c, 10, 5, krange, gen_acos_sim,
                         mean_centered, mixed_k, FALSE)
msk_i <- cval_mixed_clust(ml100k, ml100k_feat_d, 10, 6, krange, gen_acos_sim,
                          mean_centered, mskmeans, FALSE)
famd_i <- cval_mixed_clust(ml100k, ml100k_feat_d, 10, 8, krange, gen_acos_sim,
                           mean_centered, famd, FALSE)
mrk_i <- cval_mixed_clust(ml100k, ml100k_feat_d, 10, 9, krange, gen_acos_sim,
                          mean_centered, mrkmeans, FALSE)
kam_i <- cval_mixed_clust(ml100k, ml100k_feat_b, 10, 3, krange, gen_acos_sim,
                          mean_centered, kamila_clust, FALSE)

# write user mixed clustering results into file
mclust_i <- rbind(gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)
mclust_i <- cbind(method = c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
                             rep("mk", 30), rep("msk", 30), rep("famd", 30),
                             rep("mrk", 30), rep("kam", 30)),
                  mclust_i)
write.csv(mclust_i, file =
            "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_i.csv",
          row.names = FALSE)