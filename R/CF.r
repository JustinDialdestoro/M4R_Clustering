data <- read.table("M4R_Clustering/Data/u.data",
                   col.names = c("userID", "filmID", "rating", "timestamp"))

data2 <- read.csv("M4R_Clustering/Data/ratings.dat", sep=":",
                  colClasses = c(NA, "NULL"), header = FALSE)
colnames(data2) <- c("userID", "filmID", "rating", "timestamp")

t_fold_index <- function(df, t) {
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

  for (i in 1:t) {
    folds[[i]] <- df[-indexes[[i]], ]
  }
  return(folds)
}

gen_ui_matrix <- function(df, df_o) {
  ui <- matrix(NA, nrow = max(unique(df_o$userID)),
               ncol = max(unique(df_o$filmID)))
  for (i in seq_along(df$userID)) {
    row <- df[i, 1:3]
    ui[row$userID, row$filmID] <- row$rating
  }
  return(ui)
}

find_knn <- function(ui, sim, k, userid, filmid) {
  ind <- which(ui[, filmid] > 0)
  neighbours <- ind[order(-sim[userid,][ind])[2: (k + 1)]]

  return(neighbours)
}

library("recommenderlab")

gen_cos_sim <- function(ui) {
  ui0 <- ui
  ui0[is.na(ui0)] <- 0
  sim <- ui0 %*% t(ui0)
  denom <- sqrt(diag(sim))
  return(t(sim/denom)/denom)
}

gen_pcc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "pearson",
                    which = "users")
  return(as(sim, "matrix"))
}

gen_jacc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "jaccard",
                    which = "users")
  return(as(sim, "matrix"))
}

pred_ratings <- function(df, predid, ui, sim, k) {
  userid <- df$userID[predid]
  filmid <- df$filmID[predid]

  neighbours <- find_knn(ui, sim, k, userid, filmid)

  num <- sim[neighbours, userid] %*% ui[neighbours, filmid]
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(num/denom)
}

pred_fold <- function(df, df_ind, ui, sim, k) {
  preds <- c()
  for (p in df_ind) {
    preds <- c(preds, pred_ratings(df, p, ui, sim, k))
  }
  return(preds)
}

rmse <- function(pred, true) {
  ind <- !is.na(pred)
  r <- pred[ind] - true[ind]
  n <- length(pred[ind])
  return(sqrt(sum(r**2)/n))
}

krange <- seq(from = 10, to = 300, by = 10)

cross_val <- function(df, t, metric, k_range) {
  cval_f_i <- t_fold_index(df, t)
  cval_f <- t_fold(df, cval_f_i)

  rmse_l  <- replicate(30, c(0))

  for (i in 1:10) {
    ui <- gen_ui_matrix(cval_f[[i]], data)
    sim <- 1 - metric(ui)

    for (k in seq_along(k_range)) {
      p <- pred_fold(cval_f[[i]], cval_f_i[[i]], ui, sim, k_range[k])
      t <- data$rating[cval_f_i[[i]]]

      rmse_l[[k]] <- rmse_l[[k]] + rmse(p, t)
    }
  }
  return(rmse_l)
}