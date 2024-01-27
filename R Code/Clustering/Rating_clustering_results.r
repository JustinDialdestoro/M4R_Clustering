# read in the data
u100k <- read.csv("M4R_Clustering/Data/u100k.csv")

# call functions
library("viridis")
source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

nrange <- seq(from = 2, to = 10)

# cos_scores <- cval_rating_clust(u100k, 10, 40, nrange,
#                                 gen_cos_sim, weighted_sum)
# acos_scores <- cval_rating_clust(u100k, 10, 30, nrange,
#                                  gen_acos_sim, weighted_sum)
# pcc_scores <- cval_rating_clust(u100k, 10, 40, nrange,
#                                 gen_pcc_sim, weighted_sum)
# jacc_scores <- cval_rating_clust(u100k, 10, 30, nrange,
#                                  gen_jacc_sim, weighted_sum)
# euc_scores <- cval_rating_clust(u100k, 10, 30, nrange,
#                                 gen_euc_sim, weighted_sum)
# mhat_scores <- cval_rating_clust(u100k, 10, 20, nrange,
#                                  gen_mhat_sim, weighted_sum)
# cheb_scores <- cval_rating_clust(u100k, 10, 30, nrange,
#                                  gen_cheb_sim, weighted_sum)

# scores <- rbind(cos_scores, acos_scores, pcc_scores, jacc_scores,
#                 euc_scores, mhat_scores, cheb_scores)

# ymax <- max(scores$rmse)
# ymin <- min(scores$rmse)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "RMSE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
#                         "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# ymax <- max(scores$mae)
# ymin <- min(scores$mae)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "MAE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
#                         "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# ymax <- max(scores$r2)
# ymin <- min(scores$r2)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "R2",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("topright", c("cosine", "adjusted cosine", "pearson's correlation",
#                      "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# cos_scores_iclust <- cval_rating_clust(u100k, 10, 50, nrange,
#                                        gen_cos_sim, weighted_sum, FALSE)
# acos_scores_iclust <- cval_rating_clust(u100k, 10, 40, nrange,
#                                         gen_acos_sim, weighted_sum, FALSE)
# pcc_scores_iclust <- cval_rating_clust(u100k, 10, 90, nrange,
#                                        gen_pcc_sim, weighted_sum, FALSE)
# jacc_scores_iclust <- cval_rating_clust(u100k, 10, 10, nrange,
#                                         gen_jacc_sim, weighted_sum, FALSE)
# euc_scores_iclust <- cval_rating_clust(u100k, 10, 100, nrange,
#                                        gen_euc_sim, weighted_sum, FALSE)
# mhat_scores_iclust <- cval_rating_clust(u100k, 10, 300, nrange,
#                                         gen_mhat_sim, weighted_sum, FALSE)
# cheb_scores_iclust <- cval_rating_clust(u100k, 10, 60, nrange,
#                                         gen_cheb_sim, weighted_sum, FALSE)

# scores <- rbind(cos_scores_iclust, acos_scores_iclust, pcc_scores_iclust,
#                 jacc_scores_iclust, euc_scores_iclust, mhat_scores_iclust,
#                 cheb_scores_iclust)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, cos_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(7)[1], xlab = "n clusters", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, acos_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[2])
lines(nrange, pcc_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[3])
lines(nrange, jacc_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[4])
lines(nrange, euc_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[5])
lines(nrange, mhat_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[6])
lines(nrange, cheb_scores_iclust$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, cos_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(7)[1], xlab = "n clusters", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, acos_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[2])
lines(nrange, pcc_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[3])
lines(nrange, jacc_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[4])
lines(nrange, euc_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[5])
lines(nrange, mhat_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[6])
lines(nrange, cheb_scores_iclust$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, cos_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(7)[1], xlab = "n clusters", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, acos_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[2])
lines(nrange, pcc_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[3])
lines(nrange, jacc_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[4])
lines(nrange, euc_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[5])
lines(nrange, mhat_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[6])
lines(nrange, cheb_scores_iclust$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(7)[7])
legend("bottom", c("cosine", "adjusted cosine", "PCC",
                   "jaccard", "euclidean", "manhattan", "chebyshev"),
       col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 1, horiz = TRUE)