best_n_fuzzy <- function(df, n_range, m, clust_func, user = TRUE, p = FALSE) {
  scores <- rep(0, length(n_range))

  if (p == FALSE) {
    # loop over each n clusters
    for (i in seq_along(n_range)) {
      print(paste("Computing objective for", n_range[i], "clusters"))
      scores[i] <- scores[i] + clust_func(df, n_range[i], m, user)$loss
    }
  } else {
    # loop over each n clusters
    for (i in seq_along(n_range)) {
      print(paste("Computing objective for", n_range[i], "clusters"))
      scores[i] <- scores[i] + clust_func(df, n_range[i], m, user, p)$loss
    }
  }

  return(scores)
}

cval_mixed_fclust <- function(df, df_feat, t, n, m, k_range, metric, pred_func,
                              clust_func, user = TRUE) {
  nk <- length(k_range)
  p <- nrow(df_feat)
  # initial scores table
  scores <- data.frame(rmse = rep(0, nk), mae = rep(0, nk), r2 = rep(0, nk),
                       offline = rep(0, t), online = rep(0, nk))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t, user) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  # loop over each fold
  for (i in 1:t) {
    print(paste("Offline phase for fold", i, ":"))
    t1 <- Sys.time()

    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]]) # nolint
    sim <- metric(ui, user)

    # create user clusters
    clusters <- clust_func(df_feat, n, m, user)$clusters

    # find closest cluster to each point
    inds <- NULL
    for (j in 1:p) {
      inds <- c(inds, which(clusters[, j] == max(clusters[, j])))
    }

    for (l in 1:p) {
      sim[l, ] <- sim[l, ] * clusters[inds[l], ]
    }

    # loop over every k
    for (k in seq_along(k_range)) {
      print(paste("Online phase for k =", k_range[k]))
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold(df, cval_f_i[[i]], ui, sim, pred_func, # nolint
                          k_range[k], user)
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

pred_fold_fclust <- function(df, df_ind, uis, sims, pred_func, k, clusters,
                             n, inds, user = TRUE) {
  preds <- c()

  if (user == TRUE) {
    # compute rating prediction for every test case
    for (p in df_ind) {
      preds_i <- 0

      # target prediction id
      userid <- df$userID[p]
      filmid <- df$filmID[p]

      # find users cluster
      clusts <- which(clusters[, userid] > 1 / n)

      for (c in clusts) {
        # within cluster user index
        userind <- which(inds[[c]] == userid)

        # prediction
        preds_i <- preds_i + clusters[c, userid] *
          pred_func(uis[[c]], sims[[c]], k, userind, filmid)
      }
      preds <- c(preds, preds_i / sum(clusters[clusts, userid]))
    }
  } else {
    # compute rating prediction for every test case
    for (p in df_ind) {
      preds_i <- 0

      # target prediction id
      userid <- df$userID[p]
      filmid <- df$filmID[p]

      # find film cluster
      clusts <- which(clusters[, filmid] > 1 / n)

      for (c in clusts) {
        # within cluster film index
        filmind <- which(inds[[c]] == filmid)

        # prediction
        preds_i <- preds_i + clusters[c, filmid] *
          pred_func(uis[[c]], sims[[c]], k, userid, filmind, user)
      }
      preds <- c(preds, preds_i / sum(clusters[clusts, filmid]))
    }
  }
  return(preds)
}

cval_mixed_fclust_split <- function(df, df_feat, t, n, m, k_range, metric,
                                    pred_func, clust_func, user = TRUE) {
  nk <- length(k_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, nk), mae = rep(0, nk), r2 = rep(0, nk),
                       offline = rep(0, t), online = rep(0, nk))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t, user) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  # loop over each fold
  for (i in 1:t) {
    print(paste("Offline phase for fold", i, ":"))
    t1 <- Sys.time()

    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]]) # nolint

    # create user clusters
    clusters <- clust_func(df_feat, n, m, user)$clusters

    # find users belonging to each cluster
    inds <- replicate(n, c())
    for (j in 1:n) {
      inds[[j]] <- which(clusters[j, ] > 1 / n)
    }

    # segment user ratings matrix into the n clusters
    uis <- replicate(n, c())

    if (user == TRUE) {
      for (j in 1:n) {
        uis[[j]] <- ui[inds[[j]], ]
      }
    } else {
      for (j in 1:n) {
        uis[[j]] <- ui[, inds[[j]]]
      }
    }

    # similarity matrix for each segmented ui matrix
    sims <- replicate(n, c())
    for (j in 1:n) {
      sims[[j]] <- metric(uis[[j]], user)
    }

    # loop over every k
    for (k in seq_along(k_range)) {
      print(paste("Online phase for k =", k_range[k]))
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold_fclust(df, cval_f_i[[i]], uis, sims, pred_func, # nolint
                                 k_range[k], clusters, n, inds, user)
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