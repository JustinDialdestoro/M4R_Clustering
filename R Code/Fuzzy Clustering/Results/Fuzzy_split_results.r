# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_functions.r")
source("M4R_Clustering/R Code/Fuzzy Clustering/Fuzzy_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# initialise evaluation fixed variables
krange <- seq(from = 10, to = 300, by = 10)
best_n_u <- c(5, 5, 6, 3, 4, 3, 11)

# evaluate mixed clustering over a k range
gow_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 5, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_gow)
hl_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 5, 2, krange,
                                gen_acos_sim, mean_centered, fuzzy_hl)
kproto_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 6, 1.2, krange,
                                    gen_acos_sim, mean_centered, fuzzy_kproto)
mk_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 3, 2, krange,
                                gen_acos_sim, mean_centered, fuzzy_mixed_k)
msk_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 4, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_mskmeans)
famd_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 3, 2, krange,
                                  gen_acos_sim, mean_centered, fuzzy_famd)
mrk_u <- cval_mixed_fclust_split(ml100k, ml100k_dem, 10, 3, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_mrkmeans)

# write user mixed clustering results into file
fclust_split_u <- rbind(gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u)
fclust_split_u <- cbind(method = c(rep("gow", 30), rep("hl", 30),
                                   rep("kproto", 30), rep("mk", 30),
                                   rep("msk", 30), rep("famd", 30),
                                   rep("mrk", 30)),
                        fclust_split_u)
write.csv(fclust_split_u, file =
            "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_split_u.csv",
          row.names = FALSE)

best_n_i <- c(6, 5, 5, 4, 3, 3, 11)

# evaluate mixed clustering over a k range
gow_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_b, 10, 6, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_gow, FALSE)
hl_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_d, 10, 5, 2, krange,
                                gen_acos_sim, mean_centered, fuzzy_hl, FALSE)
kproto_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_c, 10, 5, 1.2, krange,
                                    gen_acos_sim, mean_centered, fuzzy_kproto,
                                    FALSE)
mk_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_c, 10, 4, 2, krange,
                                gen_acos_sim, mean_centered, fuzzy_mixed_k,
                                FALSE)
msk_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_d, 10, 3, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_mskmeans,
                                 FALSE)
famd_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_d, 10, 8, 2, krange,
                                  gen_acos_sim, mean_centered, fuzzy_famd,
                                  FALSE)
mrk_i <- cval_mixed_fclust_split(ml100k, ml100k_feat_d, 10, 8, 2, krange,
                                 gen_acos_sim, mean_centered, fuzzy_mrkmeans,
                                 FALSE)

# write user mixed clustering results into file
fclust_split_i <- rbind(gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i)
fclust_split_i <- cbind(method = c(rep("gow", 30), rep("hl", 30),
                                   rep("kproto", 30), rep("mk", 30),
                                   rep("msk", 30), rep("famd", 30),
                                   rep("mrk", 30)),
                        fclust_split_i)
write.csv(fclust_split_i, file =
            "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_split_i.csv",
          row.names = FALSE)
