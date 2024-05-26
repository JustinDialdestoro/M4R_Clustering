euc_dsim <- function(df, v, c) {
  n <- nrow(df)

  dsim_mat <- matrix(NA, nrow = c, ncol = n)

  for (i in 1:c) {
    for (j in 1:n) {
      ind <- which(!is.na(v[i, ]) & !is.na(df[j, ]))
      if (length(ind) == 0) {
        dsim_mat[i, j] <- NA
      } else {
        dsim_mat[i, j] <- norm(v[i, ][ind] - df[j, ][ind], type = "2")
      }
    }
  }
  return(dsim_mat)
}

fuzzy_c_means <- function(df, c, m, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)
  p <- ncol(df)

  # set NA to 0 in df for v^(l) computation
  df0 <- df
  df0[is.na(df0)] <- 0

  inds <- replicate(p, c())
  for (i in 1:p) {
    inds[[i]] <- which(!is.na(df[, i]))
  }

  # initialise cluster memberships w^(0)
  w_old <- matrix(runif(n * c), nrow = c)
  w_old <- t(t(w_old) / colSums(w_old, na.rm = TRUE))

  # initialise centroids c^(1)
  w_sum <- NULL
  for (i in 1:p) {
    if (length(inds[[i]]) == 1) {
      w_sum <- cbind(w_sum, w_old[, inds[[i]]]**m)
    } else {
      w_sum <- cbind(w_sum, rowSums(w_old[, inds[[i]]]**m))
    }
  }
  c_new <- (w_old**m %*% df0) / w_sum

  # update u^(1)
  d <- euc_dsim(df, c_new, c)**(2 / (m - 1))
  w_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  count <- 0
  # iterate until convergence
  while (norm(w_old - w_new, type = "F") > e) {
    w_old <- w_new

    # update v^(l+1)
    w_sum <- NULL
    for (i in 1:p) {
      if (length(inds[[i]]) == 1) {
        w_sum <- cbind(w_sum, w_new[, inds[[i]]]**m)
      } else {
        w_sum <- cbind(w_sum, rowSums(w_new[, inds[[i]]]**m))
      }
    }
    c_new <- (w_new**m %*% df0) / w_sum

    # update u^(l+1)
    d <- euc_dsim(df, c_new, c)**(2 / (m - 1))
    w_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

    count <- count + 1
    if (count > 50) {
      break
    }
  }

  # compute loss
  loss <- sum(diag(w_new**m %*% t(d**2)))
  return(list(clusters = w_new, loss = c_new, centroids = loss))
}