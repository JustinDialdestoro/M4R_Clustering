source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

fuzzy_gow <- function(df, c, m, user = TRUE, e = 1e-5) {
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
  ind <- which(loss[1, ] == min(loss[1, ]))[1]
  v_new <- x[ind, ]
  for (i in 2:c) {
    # ensure centroids are unique
    inds <- which(loss[i, ] == min(loss[i, -ind]))
    ind <- c(ind, inds[which(!(inds %in% ind))[1]])
    v_new <- rbind(v_new, x[ind[i], ])
  }

  # update u^(1)
  u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # compute loss
    loss <- u_new**m %*% d
    # find minimisers of loss and set to v^(1)
    ind <- which(loss[1, ] == min(loss[1, ]))[1]
    v_new <- x[ind, ]
    for (i in 2:c) {
      # ensure centroids are unique
      inds <- which(loss[i, ] == min(loss[i, -ind]))
      ind <- c(ind, inds[which(!(inds %in% ind))[1]])
      v_new <- rbind(v_new, x[ind[i], ])
    }

    # update u^(1)
    u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }
  # compute loss
  loss <- u_new**m %*% d**2
  return(list(u = u_new, centroids = v_new, loss = loss))
}

fuzzy_hl <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- hl_df(df, user) # nolint

  # compute gower disimilarity matrix
  d <- as.matrix(daisy(x, metric = "euclidean"))**(1 / (m - 1)) # nolint

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # compute loss
  loss <- u_old**m %*% d
  # find minimisers of loss and set to v^(1)
  ind <- which(loss[1, ] == min(loss[1, ]))[1]
  v_new <- x[ind, ]
  for (i in 2:c) {
    # ensure centroids are unique
    inds <- which(loss[i, ] == min(loss[i, -ind]))
    ind <- c(ind, inds[which(!(inds %in% ind))[1]])
    v_new <- rbind(v_new, x[ind[i], ])
  }

  # update u^(1)
  u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # compute loss
    loss <- u_new**m %*% d
    # find minimisers of loss and set to v^(1)
    ind <- which(loss[1, ] == min(loss[1, ]))[1]
    v_new <- x[ind, ]
    for (i in 2:c) {
      # ensure centroids are unique
      inds <- which(loss[i, ] == min(loss[i, -ind]))
      ind <- c(ind, inds[which(!(inds %in% ind))[1]])
      v_new <- rbind(v_new, x[ind[i], ])
    }

    # update u^(1)
    u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }
  # compute loss
  loss <- u_new**m %*% d**2
  return(list(u = u_new, centroids = v_new, loss = loss))
}