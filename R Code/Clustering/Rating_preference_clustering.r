pref_clust <- function(ui, clust_metric, alpha, beta) {
  # find number of rated items for each user
  n_ratings <- rowSums(!is.na(ui))

  # average user ratings
  u_mean <- rowMeans(ui, na.rm = TRUE)

  # segment users by mean
  o_ind <- which(u_mean >= beta)
  p_ind <- which(u_mean <= alpha)

  # find cluster centre indexes
  c_o_ind <- o_ind[which.max(n_ratings[o_ind])]
  c_p_ind <- p_ind[which.max(n_ratings[p_ind])]

  centres <- replicate(3, c())

  # cluster centres
  centres[[1]] <- ui[c_o_ind, ]
  centres[[2]] <- colMeans(ui, na.rm = TRUE)
  centres[[2]][is.nan(centres[[2]])] <- NA
  centres[[3]] <- ui[c_p_ind, ]

  cluster <- c()

  # compute user distances to cluster centres
  clust_dist <- clust_metric(ui, centres)

  # assign each user to their closest clustering centre
  for (i in 1:nrow(ui)) { # nolint
    cluster <- c(cluster, which.max(clust_dist[i, ]))
  }

  return(cluster)
}

pred_fold_clust <- function(df, df_ind, uis, sims,
                            pred_func, k, clusters, user = TRUE) {
  preds <- c()

  if (user == TRUE) {
    # compute rating prediction for every test case
    for (p in df_ind) {
      # target prediction id
      userid <- df$userID[p]
      filmid <- df$filmID[p]

      # find users cluster
      c <- clusters[userid]
      # within cluster user index
      userind <- which(which(clusters == c) == userid)

      # prediction
      preds <- c(preds, pred_func(df, uis[[c]], sims[[c]], k, userind, filmid))
    }
  } else {
    # compute rating prediction for every test case
    for (p in df_ind) {
      # target prediction id
      userid <- df$userID[p]
      filmid <- df$filmID[p]

      # find films cluster
      c <- clusters[filmid]
      # within cluster user index
      filmind <- which(which(clusters == c) == filmid)

      # prediction
      preds <- c(preds, pred_func(df, uis[[c]], sims[[c]],
                                  k, userid, filmind, user))
    }
  }
  return(preds)
}

cval_pref_clust <- function(df, t, k_range, metric, pred_func, clust_metric,
                            alpha, beta) {
  n <- length(k_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, n), mae = rep(0, n), r2 = rep(0, n),
                       offline = rep(0, t), online = rep(0, n))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  # loop over each fold
  for (i in 1:t) {
    print(paste("Offline phase for fold", i, ":"))
    t1 <- Sys.time()

    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]]) # nolint

    # create user clusters
    clusters <- pref_clust(ui, clust_metric, alpha, beta)

    # segment user ratings matrix into clusters
    uis <- replicate(3, c())
    uis[[1]] <- ui[which(clusters == 1 | clusters == 2), ]
    uis[[2]] <- ui[which(clusters == 2), ]
    uis[[3]] <- ui[which(clusters == 3 | clusters == 2), ]

    # similarity matrix for each segmented ui matrix
    sims <- replicate(3, c())
    for (l in 1:3) {
      sims[[l]] <- metric(uis[[l]])
    }

    time <- Sys.time() - t1
    print(time)
    scores$offline[i] <- time

    # loop over every k
    for (k in seq_along(k_range)) {
      print(paste("Online phase for k =", k_range[k]))
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold_clust(df, cval_f_i[[i]], uis, sims, pred_func,
                                k_range[k], clusters)
      r_true <- df$rating[cval_f_i[[i]]]

      # error metrics
      scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
      scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
      scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint

      time <- Sys.time() - t1
      print(time)
      scores$online[k] <- scores$online[k] + time
    }
  }
  scores[c(1:3, 5)] <- scores[c(1:3, 5)] / t
  return(scores)
}