source("M4R_Clustering/R/CF.r")

knn_c2 <- function(ui, sim, k, userid, filmid, clusters) {
  ind <- which((ui[, filmid] > 0) & (clusters[1, ] == clusters[1, userid]))
  neighbours <- na.omit(ind[order(-sim[userid, ][ind])[2: (k + 1)]])

  nclust <- dim(clusters)[1]
  i <- 2
  while (length(neighbours) < k) {
    if (i > nclust) {
      break
    }

    ind <- which((ui[, filmid] > 0) & (clusters[i, ] == clusters[i, userid]))
    neighbours <- c(neighbours,
                    na.omit(ind[order(-sim[userid, ][ind])[2: (k + 1)]]))
    i <- i + 1
  }

  return(neighbours)
}

pred_ratings_c2 <- function(df, predid, ui, sim, k, clusters) {
  userid <- df$userID[predid]
  filmid <- df$filmID[predid]

  neighbours <- knn_c2(ui, sim, k, userid, filmid, clusters)

  num <- sim[neighbours, userid] %*% ui[neighbours, filmid]
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(num / denom)
}

pred_fold_c2 <- function(df, df_ind, ui, sim, k, clusters) {
  preds <- c()
  for (p in df_ind) {
    preds <- c(preds, pred_ratings_c2(df, p, ui, sim, k, clusters))
  }
  return(preds)
}

vary_k_c2 <- function(df, ui, sim, test_ind, k_range, scores, clusters) {
  for (k in seq_along(k_range)) {

    r_pred <- pred_fold_c2(df, test_ind, ui, sim, k_range[k], clusters)
    r_true <- df$rating[test_ind]

    scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
    scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
    scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint
  }
  return(scores)
}

cross_val_c2 <- function(df, t, metric, k_range, clust_method, features, k) {
  n <- length(k_range)
  scores <- data.frame(rmse = rep(0, n), mae = rep(0, n), r2 = rep(0, n))
  cval_f_i <- t_fold_index(df, t) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  for (i in 1:t) {
    ui <- gen_ui_matrix(df, cval_f_i[[i]]) # nolint
    sim <- metric(ui)

    c <- clust_method(features, k)

    scores <- vary_k_c2(df, ui, sim, cval_f_i[[i]], k_range, scores, c)
  }
  return(scores / t)
}