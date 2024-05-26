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
best_n <- c(7, 5, 7, 5, 6, 6, 6, 5)

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
famd_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 6, krange, gen_acos_sim,
                           mean_centered, famd)
mrk_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 6, krange, gen_acos_sim,
                          mean_centered, mrkmeans)
kam_u <- cval_mixed_clust(ml100k, ml100k_dem, 10, 5, krange, gen_acos_sim,
                          mean_centered, kamila_clust)

# write user mixed clustering results into file
mclust_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)
mclust_u <- cbind(method = c(rep("gow", 30), rep("hl", 30), rep("kproto", 30),
                             rep("mk", 30), rep("msk", 30), rep("famd", 30),
                             rep("mrk", 30), rep("kamila", 30)),
                  mclust_u)
write.csv(mclust_u, file =
            "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_u.csv",
          row.names = FALSE)

gow_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, gow_pam, FALSE)
hl_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, hl_pam, FALSE)
kproto_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, kprototypes, FALSE)
mk_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, mixed_k, FALSE)
msk_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, mskmeans, FALSE)
famd_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, famd, FALSE)
mrk_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, mrkmeans, FALSE)
kam_avg_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, average_clust, kamila_clust, FALSE)

mclust_avg_u <- rbind(gow_avg_u, hl_avg_u, kproto_avg_u, mk_avg_u, msk_avg_u,
                      famd_avg_u, mrk_avg_u, kam_avg_u)
mclust_avg_u <- cbind(method = c(rep("gow", 9), rep("hl", 9), rep("kproto", 9),
                                 rep("mk", 9), rep("msk", 9), rep("famd", 9),
                                 rep("mrk", 9), rep("kamila", 9)),
                      mclust_avg_u)

# write user clustering predictor results into file
write.csv(mclust_avg_u,
          "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_avg_u.csv",
          row.names = FALSE)

gow_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, gow_pam, TRUE)
hl_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, hl_pam, TRUE)
kproto_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, kprototypes, TRUE)
mk_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, mixed_k, TRUE)
msk_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, mskmeans, TRUE)
famd_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, famd, TRUE)
mrk_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, mrkmeans, TRUE)
kam_mcent_u <- cval_mixed_clust_pred(ml100k, ml100k_dem, 10, nrange, gen_acos_sim, mean_centered_clust, kamila_clust, TRUE)

mclust_mcent_u <- rbind(gow_mcent_u, hl_mcent_u, kproto_mcent_u, mk_mcent_u,
                        msk_mcent_u, famd_mcent_u, mrk_mcent_u, kam_mcent_u)
mclust_mcent_u <- cbind(method = c(rep("gow", 9), rep("hl", 9),
                                   rep("kproto", 9), rep("mk", 9),
                                   rep("msk", 9), rep("famd", 9),
                                   rep("mrk", 9), rep("kamila", 9)),
                        mclust_mcent_u)

# write user clustering predictor results into file
write.csv(mclust_mcent_u,
          "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_mcent_u.csv",
          row.names = FALSE)

best_n <- c(3, 4, 7, 5, 6, 8, 9, 3)

# evaluate mixed clustering over a k range
gow_i <- cval_mixed_clust(ml100k, ml100k_feat_b, 10, 3, krange, gen_acos_sim,
                          mean_centered, gow_pam, FALSE)
hl_i <- cval_mixed_clust(ml100k, ml100k_feat_d, 10, 4, krange, gen_acos_sim,
                         mean_centered, hl_pam, FALSE)
kproto_i <- cval_mixed_clust(ml100k, ml100k_feat_b, 10, 7, krange, gen_acos_sim,
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
                             rep("mrk", 30), rep("kamila", 30)),
                  mclust_i)
write.csv(mclust_i, file =
            "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_i.csv",
          row.names = FALSE)

gow_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, average_clust, gow_pam, FALSE, FALSE)
hl_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, average_clust, hl_pam, FALSE, FALSE)
kproto_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, average_clust, kprototypes, FALSE, FALSE)
mk_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_c, 10, nrange, gen_acos_sim, average_clust, mixed_k, FALSE, FALSE)
msk_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, average_clust, mskmeans, FALSE, FALSE)
famd_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, average_clust, famd, FALSE, FALSE)
mrk_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, average_clust, mrkmeans, FALSE, FALSE)
kam_avg_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, average_clust, kamila_clust, FALSE, FALSE)

mclust_avg_i <- rbind(gow_avg_i, hl_avg_i, kproto_avg_i, mk_avg_i, msk_avg_i,
                      famd_avg_i, mrk_avg_i, kam_avg_i)
mclust_avg_i <- cbind(method = c(rep("gow", 9), rep("hl", 9), rep("kproto", 9),
                                 rep("mk", 9), rep("msk", 9), rep("famd", 9),
                                 rep("mrk", 9), rep("kamila", 9)),
                      mclust_avg_i)

# write user clustering predictor results into file
write.csv(mclust_avg_i,
          "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_avg_i.csv",
          row.names = FALSE)

gow_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, mean_centered_clust, gow_pam, TRUE, FALSE)
hl_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, mean_centered_clust, hl_pam, TRUE, FALSE)
kproto_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, mean_centered_clust, kprototypes, TRUE, FALSE)
mk_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_c, 10, nrange, gen_acos_sim, mean_centered_clust, mixed_k, TRUE, FALSE)
msk_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, mean_centered_clust, mskmeans, TRUE, FALSE)
famd_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, mean_centered_clust, famd, TRUE, FALSE)
mrk_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_d, 10, nrange, gen_acos_sim, mean_centered_clust, mrkmeans, TRUE, FALSE)
kam_mcent_i <- cval_mixed_clust_pred(ml100k, ml100k_feat_b, 10, nrange, gen_acos_sim, mean_centered_clust, kamila_clust, TRUE, FALSE)

mclust_mcent_i <- rbind(gow_mcent_i, hl_mcent_i, kproto_mcent_i, mk_mcent_i,
                        msk_mcent_i, famd_mcent_i, mrk_mcent_i, kam_mcent_i)
mclust_mcent_i <- cbind(method = c(rep("gow", 9), rep("hl", 9),
                                   rep("kproto", 9), rep("mk", 9),
                                   rep("msk", 9), rep("famd", 9),
                                   rep("mrk", 9), rep("kamila", 9)),
                        mclust_mcent_i)

# write user clustering predictor results into file
write.csv(mclust_mcent_i,
          "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_mcent_i.csv",
          row.names = FALSE)