source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

fuzzy_gow <- function(df, c, m, e = 1e-5, user = TRUE) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- gow_df(df, user) # nolint

  # compute gower disimilarity matrix
  d <- as.matrix(daisy(x, metric = "gower"))**(1 / (m - 1)) # nolint

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # compute loss
  loss <- u_old**m %*% d
  # find minimisers of loss and set to v^(1)
  v_new <- NULL
  ind <- c()
  for (i in 1:c) {
    ind <- c(ind, which(loss[i, ] == min(loss[i, ]))[1])
    v_new <- rbind(v_new, x[ind, ])
  }

  # update u^(1)
  u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # compute loss
    loss <- u_new**m %*% d**2
    # find minimisers of loss and set to v^(l)
    v_new <- NULL
    ind <- c()
    for (i in 1:c) {
      ind <- c(ind, which(loss[i, ] == min(loss[i, ]))[1])
      v_new <- rbind(v_new, x[ind, ])
    }

    # update u^(1)
    u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }
  # compute loss
  loss <- u_new**m %*% d**2
  return(list(u = u_new, centroids = v_new, loss = loss))
}