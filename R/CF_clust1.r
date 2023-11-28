source("M4R_Clustering/R/CF.r")

user_cluster <- function(ui, sim) {
  n_ratings <- rowSums(!is.na(ui))

  u_mean <- rowMeans(ui, na.rm = TRUE)

  o_ind <- which(u_mean >= 4)
  p_ind <- which(u_mean <= 3)

  c_o <- o_ind[which.max(n_ratings[o_ind])]
  c_p <- p_ind[which.max(n_ratings[p_ind])]

  last <- nrow(sim)

  cluster <- c()

  for (i in 1:nrow(ui)) { # nolint
    cluster <- c(cluster, which.max(sim[i, c(c_o, last, c_p)]))
  }
  return(cluster)
}

cluster_knn <- function(ui, sim, k, userid, filmid, clusters) {
  if (clusters[userid] == 1) {
    ind <- which((ui[, filmid] > 0) & (clusters != 3))
  } else if (clusters[userid] == 3) {
    ind <- which((ui[, filmid] > 0) & (clusters != 1))
  } else {
    ind <- which((ui[, filmid] > 0) & (clusters == 2))
  }

  neighbours <- ind[order(-sim[userid,][ind])[2: (k + 1)]]

  return(neighbours)
}

pred_ratings_cluster <- function(df, predid, ui, sim, k, clusters) {
  userid <- df$userID[predid]
  filmid <- df$filmID[predid]

  neighbours <- cluster_knn(ui, sim, k, userid, filmid, clusters)

  num <- sim[neighbours, userid] %*% ui[neighbours, filmid]
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(num/denom)
}

pred_fold_cluster <- function(df, df_ind, ui, sim, k, clusters) {
  preds <- c()
  for (p in df_ind) {
    preds <- c(preds, pred_ratings_cluster(df, p, ui, sim, k, clusters))
  }
  return(preds)
}

vary_k_cluster <- function(df, ui, sim, test_ind, k_range, scores, clusters) {
  for (k in seq_along(k_range)) {

    r_pred <- pred_fold_cluster(df, test_ind, ui, sim, k_range[k], clusters)
    r_true <- df$rating[test_ind]

    scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
    scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
    scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint
  }
  return(scores)
}

cross_val_cluster <- function(df, t, metric, k_range) {
  n <- length(k_range)
  scores <- data.frame(rmse = rep(0, n), mae = rep(0, n), r2 = rep(0, n))
  cval_f_i <- t_fold_index(df, t) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  for (i in 1:t) {
    ui <- gen_ui_matrix(cval_f[[i]], df) # nolint
    sim <- metric(ui)

    c_n <- colMeans(ui, na.rm = TRUE)

    c <- user_cluster(ui, metric(rbind(ui, c_n)))

    scores <- vary_k_cluster(df, ui, sim, cval_f_i[[i]], k_range, scores, c)
  }
  return(scores / t)
}