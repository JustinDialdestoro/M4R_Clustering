source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

best_n <- function(df, n_range, clust_func, user = TRUE) {
  scores <- rep(0, length(n_range))

  # loop over each n clusters
  for (i in seq_along(n_range)) {
    print(paste("Computing objective for", n_range[i], "clusters"))
    scores[i] <- scores[i] + clust_func(df, n_range[i], user, TRUE)
  }
  return(scores)
}

best_n_famd <- function(df, n_range, p, user = TRUE) {
  scores <- rep(0, length(n_range))

  # loop over each n clusters
  for (i in seq_along(n_range)) {
    print(paste("Computing objective for", n_range[i], "clusters"))
    scores[i] <- scores[i] + famd(df, n_range[i], p, user, TRUE) # nolint
  }
  return(scores)
}

cval_mixed_clust <- function(df, df_feat, t, k, n_range, metric,
                             pred_func, clust_func) {
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
      clusters <- clust_func(df_feat, n_range[n])

      # segment user ratings matrix into the n clusters
      uis <- replicate(n_range[n], c())
      for (j in 1:n_range[n]) {
        uis[[j]] <- ui[which(clusters == j), ]
      }

      # similarity matrix for each segmented ui matrix
      sims <- replicate(n_range[n], c())
      for (j in 1:n_range[n]) {
        sims[[j]] <- metric(uis[[j]])
      }

      print(Sys.time() - t1)

      print("Online phase:")
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold_clust(df, cval_f_i[[i]], uis, sims, pred_func, # nolint
                                k, clusters)
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