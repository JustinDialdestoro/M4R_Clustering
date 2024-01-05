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
  ui <- matrix(NA, nrow = nrows, ncol = ncols)

  # input ratings
  ui[cbind(df$userID, df$filmID)] <- df$rating

  return(ui)
}

pred_fold <- function(df, df_ind, ui, sim, pred_func, k) {
  preds <- c()

  # compute rating prediction for every test case
  for (p in df_ind) {
    # target prediction id
    userid <- df$userID[p]
    filmid <- df$filmID[p]

    # prediction
    preds <- c(preds, pred_func(df, ui, sim, k, userid, filmid))
  }
  return(preds)
}

rmse <- function(pred, true) {
  ind <- !is.na(pred)
  r <- pred[ind] - true[ind]
  n <- length(pred[ind])
  return(sqrt(sum(r**2) / n))
}

mae <- function(pred, true) {
  ind <- !is.na(pred)
  r <- abs(pred[ind] - true[ind])
  n <- length(pred[ind])
  return(sum(r) / n)
}

r2 <- function(pred, true) {
  ind <- !is.na(pred)
  return(cor(pred[ind], true[ind])**2)
}

cval <- function(df, t, k_range, metric, pred_func) {
  n <- length(k_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, n), mae = rep(0, n), r2 = rep(0, n))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t)
  cval_f <- t_fold(df, cval_f_i)

  # loop over each fold
  for (i in 1:t) {
    print("Offline phase:")
    t1 <- Sys.time()

    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]])
    sim <- metric(ui)

    print(Sys.time() - t1)

    # loop over every k
    for (k in seq_along(k_range)) {
      print("Online phase:")
      t1 <- Sys.time()

      # predicte on test fold ratings
      r_pred <- pred_fold(df, cval_f_i[[i]], ui, sim, pred_func, k_range[k])
      r_true <- df$rating[cval_f_i[[i]]]

      # error metrics
      scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
      scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
      scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint

      print(Sys.time() - t1)
    }
  }
  return(scores / t)
}