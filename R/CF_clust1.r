user_cluster <- function(ui, clust_metric) {
  # find number of rated items for each user
  n_ratings <- rowSums(!is.na(ui))

  # average user ratings
  u_mean <- rowMeans(ui, na.rm = TRUE)

  # segment users by mean
  o_ind <- which(u_mean >= 4)
  p_ind <- which(u_mean <= 3)

  # find cluster centres
  c_o_ind <- o_ind[which.max(n_ratings[o_ind])]
  c_p_ind <- p_ind[which.max(n_ratings[p_ind])]

  centres <- replicate(3, c())

  centres[[1]] <- ui[c_o_ind, ]
  centres[[2]] <- colMeans(ui, na.rm = TRUE)
  centres[[2]][is.nan(centres[[2]])] <- NA
  centres[[3]] <- ui[c_p_ind, ]

  cluster <- c()

  clust_dist <- clust_metric(ui, centres)

  # assign each user to their closest clustering centre
  for (i in 1:nrow(ui)) { # nolint
    cluster <- c(cluster, which.max(clust_dist[i, ]))
  }

  return(cluster)
}

pred_fold_clust1 <- function(df, df_ind, uis, sims, pred_func, k, clusters) {
  preds <- c()

  # compute rating prediction for every test case
  for (p in df_ind) {
    c <- clusters[p]
    preds <- c(preds, pred_func(df, uis[[c]], sims[[c]], k,
                                which(which(clusters == c) == p), df$filmID[p]))
  }
  return(preds)
}

cross_val_clust1 <- function(df, t, metric, pred_func, clust_metric, k_range) {
  n <- length(k_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, n), mae = rep(0, n), r2 = rep(0, n))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t)
  cval_f <- t_fold(df, cval_f_i)

  # loop over each fold
  for (i in 1:t) {
    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]])

    clusters <- user_cluster(ui, clust_metric)

    uis <- replicate(3, c())
    for (i in 1:3) {
      uis[[i]] <- ui[which(clusters == i), ]
    }

    sims <- replicate(3, c())
    for (i in 1:3) {
      sims[[i]] <- metric(uis[[i]])
    }

    # loop over every k
    for (k in seq_along(k_range)) {
      # predicte on test fold ratings
      r_pred <- pred_fold_clust1(df, cval_f_i[[i]], uis, sims, pred_func,
                                 k_range[k], clusters)
      r_true <- df$rating[cval_f_i[[i]]]

      # error metrics
      scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
      scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
      scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint
    }
  }
  return(scores / t)
}