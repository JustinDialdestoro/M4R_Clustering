# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# set range of k values to test over
krange <- seq(from = 10, to = 300, by = 10)
n <- length(krange)

# evaluate adjusted cosine predictor performance
mean_u <- cval(ml100k, 10, krange, gen_acos_sim, average)
wsum_u <- cval(ml100k, 10, krange,  gen_acos_sim, weighted_sum)
mcent_u <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered)
zscore_u <- cval(ml100k, 10, krange,  gen_acos_sim, z_score)
disc_u <- cval(ml100k, 10, krange,  gen_acos_sim, discrete)

pred_u <- rbind(mean_u, wsum_u, mcent_u, zscore_u, disc_u)

pred_u <- cbind(predictor = c(rep("mean", n), rep("weighted sum", n),
                              rep("mean centred", n), rep("z score", n),
                              rep("discrete", n)), pred_u)

# write user predictor results into file
write.csv(pred_u,
          file = "M4R_Clustering/Results/Collaborative Filtering/pred_u.csv",
          row.names = FALSE)

# evaluate cosine, adjusted cosine, and pcc
mean_i <- cval(ml100k, 10, krange, gen_acos_sim, average, FALSE)
wsum_i <- cval(ml100k, 10, krange, gen_acos_sim, weighted_sum, FALSE)
mcent_i <- cval(ml100k, 10, krange,  gen_acos_sim, mean_centered, FALSE)
zscore_i <- cval(ml100k, 10, krange,  gen_acos_sim, z_score, FALSE)
disc_i <- cval(ml100k, 10, krange,  gen_acos_sim, discrete, FALSE)

pred_i <- rbind(mean_i, wsum_i, mcent_i, zscore_i, disc_i)

pred_i <- cbind(predictor = c(rep("mean", n), rep("weighted sum", n),
                              rep("mean centred", n), rep("z score", n),
                              rep("discrete", n)), pred_i)

# write item predictor results into file
write.csv(pred_i,
          file = "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv",
          row.names = FALSE)