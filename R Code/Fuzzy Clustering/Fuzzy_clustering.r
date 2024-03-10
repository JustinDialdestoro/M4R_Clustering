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

  # set NA to 0 in df for v^(l) computation
  df0 <- df
  df0[is.na(df0)] <- 0

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # initialise centroids v^(1)
  v_new <- (u_old**m %*% df0) / rowSums(u_old**m, na.rm = TRUE)

  # update u^(1)
  d <- euc_dsim(df, v_new, c)**(2 / (m - 1))
  u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # update v^(l+1)
    v_new <- (u_new**m %*% df0) / rowSums(u_new**m, na.rm = TRUE)

    # update u^(l+1)
    d <- euc_dsim(df, v_new, c)**(2 / (m - 1))
    u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))
  }

  # compute loss
  loss <- sum(diag(u_new**m %*% t(d**2)))
  return(list(u = u_new, centroids = v_new, loss = loss))
}