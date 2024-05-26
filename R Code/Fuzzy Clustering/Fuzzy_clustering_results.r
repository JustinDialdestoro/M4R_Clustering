# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

test <- fuzzy_gow(ml100k_feat_b, 2, 2, FALSE)
test <- fuzzy_hl(ml100k_feat_d, 2, 2, FALSE)
test <- fuzzy_kproto(ml100k_feat_c, 2, 2, FALSE)
test <- fuzzy_mixed_k(ml100k_feat_c, 2, 2, FALSE)
test <- fuzzy_mskmeans(ml100k_feat_d, 2, 2, FALSE)
test <- fuzzy_famd(ml100k_feat_d, 2, 2, FALSE)
test <- fuzzy_mrkmeans(ml100k_feat_d, 2, 2, FALSE)