library("cluster")

cosine <- function(x, y) {
  # find non NA entries
  ind <- which(!is.na(x) & !is.na(y))
  if (length(ind) == 0) {
    return(0)
  }
  # compute cosine similarity
  return((x[ind] %*% y[ind]) /
           (norm(x[ind], type = "2") * norm(y[ind], type = "2") + 1e-9))
}

gen_cos_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    # construct similarity matrix skeleton
    n <- nrow(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui[i, ], ui[j, ])
      }
    }

    # fill remainder of matrix
    sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]

  } else {
    # construct similarity matrix skeleton
    n <- ncol(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui[, i], ui[, j])
      }
    }

    # fill remainder of matrix
    sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  }

  return(sim)
}

gen_acos_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    # construct similarity matrix skeleton
    n <- nrow(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # nromalise by column means
    mean <- rowMeans(t(ui), na.rm = TRUE)
    ui0 <- t(t(ui) - mean)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui0[i, ], ui0[j, ])
      }
    }
  } else {
    # construct similarity matrix skeleton
    n <- ncol(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # nromalise by row means
    mean <- rowMeans(ui, na.rm = TRUE)
    ui0 <- ui - mean

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui0[, i], ui0[, j])
      }
    }
  }

  # fill remainder of matrix
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]

  # transform into similarity matrix
  return((1 + sim) / 2)
}

gen_pcc_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    # construct similarity matrix skeleton
    n <- nrow(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # nromalise by row means
    mean <- rowMeans(ui, na.rm = TRUE)
    ui0 <- ui - mean

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui0[i, ], ui0[j, ])
      }
    }
  } else {
    # construct similarity matrix skeleton
    n <- ncol(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # nromalise by column means
    mean <- rowMeans(t(ui), na.rm = TRUE)
    ui0 <- t(t(ui) - mean)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        sim[i, j] <- cosine(ui0[, i], ui0[, j])
      }
    }
  }

  # fill remainder of matrix
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]

  # transform into similarity matrix
  return((1 + sim) / 2)
}

gen_euc_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    sim <- as(dist(ui, "euclidean"), "matrix")
  } else {
    sim <- as(dist(t(ui), "euclidean"), "matrix")
  }
  sim[is.na(sim)] <- Inf

  # transform into similarity matrix
  return(1 / (1 + sim))
}

gen_mhat_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    sim <- as(dist(ui, "manhattan"), "matrix")
  } else {
    sim <- as(dist(t(ui), "manhattan"), "matrix")
  }
  sim[is.na(sim)] <- Inf

  # transform into similarity matrix
  return(1 / (1 + sim))
}

euc_clust <- function(ui, centres, n, user = TRUE) { # nolint
  if (user == TRUE) {
    nu <- nrow(ui)
    clust_dist <- matrix(NA, nrow = nu, ncol = n)

    for (i in 1:nu) {
      for (j in 1:n) {
        ind <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
        if (length(ind) == 0) {
          clust_dist[i, j] <- Inf
        } else {
          clust_dist[i, j] <- norm(ui[i, ][ind] - centres[[j]][ind], type = "2")
        }
      }
    }
  } else {
    ni <- ncol(ui)
    clust_dist <- matrix(NA, nrow = ni, ncol = n)

    for (i in 1:ni) {
      for (j in 1:n) {
        ind <- which(!is.na(ui[, i]) & !is.na(centres[[j]]))
        if (length(ind) == 0) {
          clust_dist[i, j] <- Inf
        } else {
          clust_dist[i, j] <- norm(ui[, i][ind] - centres[[j]][ind], type = "2")
        }
      }
    }
  }
  return(1 / (1 + clust_dist))
}