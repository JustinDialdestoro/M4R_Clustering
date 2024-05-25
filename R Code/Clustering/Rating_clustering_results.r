# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Clustering/Clustering_predictors.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# initialise fixed variables
krange <- krange <- seq(from = 10, to = 300, by = 10)
nrange <- 2:10
n <- length(nrange)

# user within cluster sum of squares
clust_obj_u <- best_n(ui, nrange)
write.csv(clust_obj_u,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_u.csv",
          row.names = FALSE)

# evaluate performance using optimum number of clusters
clust_u <- cval_clust(ml100k, 10, 6, krange, gen_acos_sim, mean_centered)
write.csv(clust_u,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv",
          row.names = FALSE)

avg_clust_u <- cval_clust_pred(ml100k, 10, nrange, gen_acos_sim, average_clust,
                               FALSE)
mean_cent_u <- cval_clust_pred(ml100k, 10, nrange, gen_acos_sim,
                               mean_centered_clust)

clust_pred_u <- rbind(avg_clust_u, mean_cent_u)
clust_pred_u <- cbind(predictor = c(rep("average", n), rep("mean centred", n)),
                      clust_pred_u)

# write user clustering predictor results into file
write.csv(clust_pred_u,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_pred_u.csv",
          row.names = FALSE)

# # item within cluster sum of squares
# clust_obj_i <- best_n(ui, nrange, FALSE)
# write.csv(clust_obj_i,
#           "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_i.csv",
#           row.names = FALSE)

# # evaluate performance using optimum number of clusters
# clust_i <- cval_clust(ml100k, 10, 5, krange, gen_ups_sim, mean_centered, FALSE)
# write.csv(clust_i,
#           "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv",
#           row.names = FALSE)

avg_clust_i <- cval_clust_pred(ml100k, 10, nrange, gen_acos_sim, average_clust,
                               FALSE, TRUE)
mean_cent_i <- cval_clust_pred(ml100k, 10, nrange, gen_acos_sim,
                               mean_centered_clust, TRUE, FALSE)

clust_pred_i <- rbind(avg_clust_i, mean_cent_i)
clust_pred_i <- cbind(predictor = c(rep("average", n), rep("mean centred", n)),
                      clust_pred_i)

# write user clustering predictor results into file
write.csv(clust_pred_i,
          "M4R_Clustering/Results/Rating clustering/Crisp/clust_pred_i.csv",
          row.names = FALSE)