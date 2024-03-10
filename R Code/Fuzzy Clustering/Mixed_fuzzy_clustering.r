source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

fuzzy_gow <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- as.matrix(gow_df(df, user)) # nolint

  # function to compute loss
  loss <- function(v) {
    # reshape parameters into matrix
    v <- t(matrix(v, ncol = c))

    # compute dissimilarity matrix
    d <- as.matrix(daisy(rbind(x, v), metric = "gower"))**(1 / (m - 1)) # nolint

    return(sum(u_old**m %*% d[1:n, (n + 1):(n + c)]))
  }

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # numerically find minimisers of loss and set to v^(1)
  v_old <- matrix(t(x[sample(n, c), ]), nrow = 1)
  v_new <- t(matrix(optim(v_old, loss)$par, ncol = c))

  # compute gower disimilarity matrix
  d <- as.matrix(daisy(rbind(x, v_new), metric = "gower"))** # nolint
    (1 / (m - 1))

  # update u^(1)
  u_new <- 1 / t(t(d[(n + 1):(n + c), 1:n]) *
                   colSums(1 / d[(n + 1):(n + c), 1:n], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # numerically find minimisers of loss to update v^(l+1)
    v_old <- matrix(t(v_new), nrow = 1)
    v_new <- t(matrix(optim(v_old, loss)$par, ncol = c))

    # compute gower disimilarity matrix
    d <- as.matrix(daisy(rbind(x, v_new), metric = "gower"))** # nolint
      (1 / (m - 1))

    # update u^(l+1)
    u_new <- 1 / t(t(d[(n + 1):(n + c), 1:n]) *
                     colSums(1 / d[(n + 1):(n + c), 1:n], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }

  # compute loss
  final_loss <- loss(matrix(t(v_new), nrow = 1))
  return(list(u = u_new, centroids = v_new, loss = sum(final_loss)))
}

fuzzy_hl <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- as.matrix(hl_df(df, user)) # nolint

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # initialise centroids v^(1)
  v_new <- (u_old**m %*% x) / rowSums(u_old**m, na.rm = TRUE)

  # update u^(1)
  d <- euc_dsim(x, v_new, c)**(2 / (m - 1)) # nolint
  u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # update v^(l+1)
    v_new <- (u_new**m %*% x) / rowSums(u_new**m, na.rm = TRUE)

    # update u^(l+1)
    d <- euc_dsim(x, v_new, c)**(2 / (m - 1)) # nolint
    u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))
  }

  # compute loss
  loss <- sum(diag(u_new**m %*% t(d**2)))
  return(list(u = u_new, centroids = v_new, loss = sum(loss)))
}