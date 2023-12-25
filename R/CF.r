source("M4R_Clustering/R/Metrics.r")
library("recommenderlab")

t_fold_index <- function(df, t) {
  set.seed(1)
  # empty vector to contain each fold
  fold_ind <- replicate(t, c())
  # iterate through each unique user id to partition each users rating evenly
  for (i in unique(df$userID)) {
    # find the indices of user i's ratings and permute them
    i_perm <- sample(which(df$userID == i))

    # number of ratings in each fold
    n <- length(i_perm)
    k <- n %/% t
    r <- n %% t

    # add the partitioned indices into each fold
    for (j in 1:t) {
      fold_ind[[j]] <- c(fold_ind[[j]], i_perm[((j - 1) * k + 1):(j * k)])
    }

    # randomly assign remaining indices (from division) to a fold
    if (r > 0) {
      f <- sample(1:t, 1)
      fold_ind[[f]] <- c(fold_ind[[f]], i_perm[(t * k + 1):length(i_perm)])
    }
  }
  return(fold_ind)
}

t_fold <- function(df, indexes) {
  # number of folds
  t <- length(indexes)

  # list to contain each training set dataframe
  folds <- replicate(t, c())

  # generate folds by removing test folds
  for (i in 1:t) {
    folds[[i]] <- df[-indexes[[i]], ]
  }
  return(folds)
}

gen_ui_matrix <- function(df_o, df) {
  # size of ui matrix
  nrows <- max(df_o$userID)
  ncols <- max(df_o$filmID)

  # skeleton matrix of NA
  ui <- matrix(0, nrow = nrows, ncol = ncols)

  # input ratings
  ui[cbind(df$userID, df$filmID)] <- df$rating

  return(ui)
}

find_knn <- function(ui, sim, k, userid, filmid) {
  # indices of users who have rated the film
  ind <- which(ui[, filmid] > 0)
  # nearest neighbours
  neighbours <- ind[order(-sim[userid, ][ind])[1:k]]

  # omit NA when not enough neighbours found
  return(na.omit(neighbours))
}

pred_ratings <- function(df, predid, ui, sim, k) {
  # id of target prediction
  userid <- df$userID[predid]
  filmid <- df$filmID[predid]

  # find nearest neighbours
  neighbours <- find_knn(ui, sim, k, userid, filmid)

  # compute rating prediction
  num <- sim[neighbours, userid] %*% ui[neighbours, filmid]
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(num / denom)
}

pred_fold <- function(df, df_ind, ui, sim, k) {
  preds <- c()

  # compute rating prediction for every test case
  for (p in df_ind) {
    preds <- c(preds, pred_ratings(df, p, ui, sim, k))
  }
  return(preds)
}

vary_k <- function(df, ui, sim, test_ind, k_range, scores) {
  # loop over every k
  for (k in seq_along(k_range)) {
    # predicte on test fold ratings
    r_pred <- pred_fold(df, test_ind, ui, sim, k_range[k])
    r_true <- df$rating[test_ind]

    # error metrics
    scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
    scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
    scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint
  }
  return(scores)
}

cross_val <- function(df, t, metric, k_range) {
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
    sim <- metric(ui)

    # error metrics
    scores <- vary_k(df, ui, sim, cval_f_i[[i]], k_range, scores)
  }
  return(scores / t)
}