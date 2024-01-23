u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

udem <- read.table("M4R_Clustering/Data/u.user", sep = "|",
                   col.names = c("userID", "age", "gender",
                                 "occupation", "zip"))

ifeat <- read.delim("M4R_Clustering/Data/u.item", sep = "|", header = FALSE,
                    col.names = c("filmID", "title", "date", "null", "imdb",
                                  "unknown", "action", "adventure", "animation",
                                  "children", "comedy", "crime",
                                  "documentary", "drama", "fantasy",
                                  "film-noir", "horror", "musical", "mystery",
                                  "romance", "sci-fi", "thriller", "war",
                                  "western"))

imdb <- read.delim("Data/title.basics.tsv/data.tsv", sep = "\t", header = TRUE)

crew <- read.delim("Data/title.crew.tsv/data.tsv", sep = "\t", header = TRUE)

names <- read.delim("Data/name.basics.tsv/data.tsv", sep = "\t", header = TRUE)

title_info <- function(title) {
  film_title <- gsub("\\s*\\([^\\)]+\\)", "", title)
  film_date <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.",
                    "", title, perl = TRUE)

  return(c(film_title, film_date))
}

for (i in 1:10) {
  film_text <- title_info(ifeat$title[i])
  print(film_text[2])
  print(imdb[imdb$originalTitle == film_text[1] &
               imdb$startYear == film_text[2] &
               imdb$titleType == "movie", ]$primaryTitle)
}


# call functions
library("viridis")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering.r")

nrange <- seq(from = 2, to = 10)

# euc_scores_gow <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
#                                    gen_euc_sim, weighted_sum, gow_pam)
euc_scores_hl <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
                                  gen_euc_sim, weighted_sum, hl_pam)
euc_scores_kproto <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
                                      gen_euc_sim, weighted_sum, kprototypes)
euc_scores_mix <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
                                   gen_euc_sim, weighted_sum, mixed_k)
euc_scores_famd <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
                                    gen_euc_sim, weighted_sum, famd)
euc_scores_kam <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
                                   gen_euc_sim, weighted_sum, kamila_clust)

library("viridis")

scores <- rbind(euc_scores_gow, euc_scores_hl, euc_scores_kproto,
                euc_scores_mix, euc_scores_famd, euc_scores_kam)

ymax <- max(scores$rmse)
ymin <- min(scores$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(6)[1], xlab = "n clusters", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, euc_scores_hl$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[2])
lines(nrange, euc_scores_kproto$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[3])
lines(nrange, euc_scores_mix$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[4])
lines(nrange, euc_scores_famd$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[5])
lines(nrange, euc_scores_kam$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[6])
legend("bottomright", c("Gower PAM", "HL Pam", "K-Prototypes", "Mixed K-Means",
                        "FAMD", "KAMILA"),
       col = viridis(6), lty = 2, pch = 4, lwd = 2, cex = 0.8)

ymax <- max(scores$mae)
ymin <- min(scores$mae)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(6)[1], xlab = "n clusters", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, euc_scores_hl$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[2])
lines(nrange, euc_scores_kproto$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[3])
lines(nrange, euc_scores_mix$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[4])
lines(nrange, euc_scores_famd$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[5])
lines(nrange, euc_scores_kam$mae, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[6])
legend("bottomright", c("Gower PAM", "HL Pam", "K-Prototypes", "Mixed K-Means",
                        "FAMD", "KAMILA"),
       col = viridis(6), lty = 2, pch = 4, lwd = 2, cex = 0.8)

ymax <- max(scores$r2)
ymin <- min(scores$r2)
ygap <- 0.2 * (ymax - ymin)

plot(nrange, euc_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
     col = viridis(6)[1], xlab = "n clusters", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
lines(nrange, euc_scores_hl$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[2])
lines(nrange, euc_scores_kproto$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[3])
lines(nrange, euc_scores_mix$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[4])
lines(nrange, euc_scores_famd$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[5])
lines(nrange, euc_scores_kam$r2, lty = 2, type = "b", pch = 4, lwd = 2,
      col = viridis(6)[6])
legend("bottomright", c("Gower PAM", "HL Pam", "K-Prototypes", "Mixed K-Means",
                        "FAMD", "KAMILA"),
       col = viridis(6), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# cos_scores_gow <- cval_mixed_clust(u100k, udem, 10, 40, nrange,
#                                    gen_cos_sim, weighted_sum, gow_pam)
# acos_scores_gow <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
#                                     gen_acos_sim, weighted_sum, gow_pam)
# pcc_scores_gow <- cval_mixed_clust(u100k, udem, 10, 40, nrange,
#                                    gen_pcc_sim, weighted_sum, gow_pam)
# jacc_scores_gow <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
#                                     gen_jacc_sim, weighted_sum, gow_pam)
# euc_scores_gow <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
#                                    gen_euc_sim, weighted_sum, gow_pam)
# mhat_scores_gow <- cval_mixed_clust(u100k, udem, 10, 20, nrange,
#                                     gen_mhat_sim, weighted_sum, gow_pam)
# cheb_scores_gow <- cval_mixed_clust(u100k, udem, 10, 30, nrange,
#                                     gen_cheb_sim, weighted_sum, gow_pam)

# library("viridis")

# scores <- rbind(cos_scores_gow, acos_scores_gow, pcc_scores_gow,
#                 jacc_scores_gow, euc_scores_gow, mhat_scores_gow,
#                 cheb_scores_gow)

# ymax <- max(scores$rmse)
# ymin <- min(scores$rmse)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "RMSE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores_gow$rmse, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
#                         "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# ymax <- max(scores$mae)
# ymin <- min(scores$mae)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "MAE",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores_gow$mae, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("bottomright", c("cosine", "adjusted cosine", "pearson's correlation",
#                         "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)

# ymax <- max(scores$r2)
# ymin <- min(scores$r2)
# ygap <- 0.2 * (ymax - ymin)

# plot(nrange, cos_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#      col = viridis(7)[1], xlab = "n clusters", ylab = "R2",
#      ylim = c(ymin - ygap, ymax + ygap))
# lines(nrange, acos_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[2])
# lines(nrange, pcc_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[3])
# lines(nrange, jacc_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[4])
# lines(nrange, euc_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[5])
# lines(nrange, mhat_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[6])
# lines(nrange, cheb_scores_gow$r2, lty = 2, type = "b", pch = 4, lwd = 2,
#       col = viridis(7)[7])
# legend("topright", c("cosine", "adjusted cosine", "pearson's correlation",
#                      "jaccard", "euclidean", "manhattan", "chebyshev"),
#        col = viridis(7), lty = 2, pch = 4, lwd = 2, cex = 0.8)