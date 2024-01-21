library("cluster")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")

rating_clust <- function(ui, k, user = TRUE) {
  # create similarity matrix
  sim <- gen_euc_sim(ui, user) # nolint
  sim[is.na(sim)] <- 0

  # k-means clustering
  cluster <- kmeans(sim, k)

  return(cluster$cluster)
}

cval_rating_clust <- function(df, t, k, n_range,
                              metric, pred_func, user = TRUE) {
  m <- length(n_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, m), mae = rep(0, m), r2 = rep(0, m))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  # loop over each fold
  for (i in 1:t) {

    # loop over every n
    for (n in seq_along(n_range)) {
      print(paste("Offline phase for n = ", n_range[n], ", fold", i, ":"))
      t1 <- Sys.time()

      # ui and similarity matrix
      ui <- gen_ui_matrix(df, cval_f[[i]]) # nolint

      # create rating clusters
      clusters <- rating_clust(ui, n_range[n], user)

      # segment user ratings matrix into the n clusters
      uis <- replicate(n_range[n], c())

      if (user == TRUE) {
        for (j in 1:n_range[n]) {
          uis[[j]] <- ui[which(clusters == j), ]
        }
      } else {
        for (j in 1:n_range[n]) {
          uis[[j]] <- ui[, which(clusters == j)]
        }
      }

      # similarity matrix for each segmented ui matrix
      sims <- replicate(n_range[n], c())
      for (j in 1:n_range[n]) {
        sims[[j]] <- metric(uis[[j]], user)
      }

      print(Sys.time() - t1)

      print("Online phase:")
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold_clust(df, cval_f_i[[i]], uis, sims, pred_func, # nolint
                                k, clusters, user)
      r_true <- df$rating[cval_f_i[[i]]]

      # error metrics
      scores$rmse[n] <- scores$rmse[n] + rmse(r_pred, r_true) # nolint
      scores$mae[n] <- scores$mae[n] + mae(r_pred, r_true) # nolint
      scores$r2[n] <- scores$r2[n] + r2(r_pred, r_true) # nolint

      print(Sys.time() - t1)
    }
  }
  return(scores / t)
}