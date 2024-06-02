# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

# # evaluate cosine, adjusted cosine, and pcc
cos_u <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum)
acos_u <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum)
pcc_u <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum)
euc_u <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum)
mhat_u <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum)

sim_u <- rbind(cos_u, acos_u, pcc_u, euc_u, mhat_u)

sim_u <- cbind(metric = c(rep("cosine", n), rep("acosine", n),
                          rep("pcc", n), rep("euclidean", n),
                          rep("manhattan", n)), sim_u)

# write user similarity results into file
write.csv(sim_u,
          file = "M4R_Clustering/Results/Collaborative Filtering/sim_u.csv",
          row.names = FALSE)

# # evaluate cosine, adjusted cosine, and pcc
cos_i <- cval(ml100k, 10, krange, gen_cos_sim, weighted_sum, FALSE)
acos_i <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
pcc_i <- cval(ml100k, 10, krange, gen_pcc_sim, weighted_sum, FALSE)
euc_i <- cval(ml100k, 10, krange, gen_euc_sim, weighted_sum, FALSE)
mhat_i <- cval(ml100k, 10, krange, gen_mhat_sim, weighted_sum, FALSE)

sim_i <- rbind(cos_i, acos_i, pcc_i, euc_i, mhat_i)

sim_i <- cbind(metric = c(rep("cosine", n), rep("acosine", n),
                          rep("pcc", n), rep("euclidean", n),
                          rep("manhattan", n)), sim_i)

# write user similarity results into file
write.csv(sim_i,
          file = "M4R_Clustering/Results/Collaborative Filtering/sim_i.csv",
          row.names = FALSE)