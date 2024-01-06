library("cluster")

cosine <- function(x, y) {
  # find non NA entries
  ind <- which(!is.na(x) & !is.na(y))
  if (length(ind) == 0) {
    return(0)
  }
  # compute cosine similarity
  return((x[ind] %*% y[ind]) /
           (norm(x[ind], type = "2") * norm(y[ind], type = "2")))
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

gen_pcc_sim <- function(ui) {
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

gen_jacc_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    # construct similarity matrix skeleton
    n <- nrow(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        # find non NA entries intersection and union
        num <- which(!is.na(ui[i, ]) & !is.na(ui[j, ]))
        denom <- which(!is.na(ui[i, ]) | !is.na(ui[j, ]))

        # compute jaccard index
        sim[i, j] <- length(num) / length(denom)
      }
    }
  } else {
    # construct similarity matrix skeleton
    n <- ncol(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        # find non NA entries intersection and union
        num <- which(!is.na(ui[, i]) & !is.na(ui[, j]))
        denom <- which(!is.na(ui[, i]) | !is.na(ui[, j]))

        # compute jaccard index
        sim[i, j] <- length(num) / length(denom)
      }
    }
  }

  # fill remainder of matrix
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return(sim)
}

gen_euc_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    sim <- as(dist(ui, "euclidean"), "matrix")
  } else {
    sim <- as(dist(t(ui), "euclidean"), "matrix")
  }

  # transform into similarity matrix
  return(1 / (1 + sim))
}

gen_mhat_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    sim <- as(dist(ui, "manhattan"), "matrix")
  } else {
    sim <- as(dist(t(ui), "manhattan"), "matrix")
  }

  # transform into similarity matrix
  return(1 / (1 + sim))
}

gen_cheb_sim <- function(ui, user = TRUE) {
  if (user == TRUE) {
    # construct similarity matrix skeleton
    n <- nrow(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        # find non NA entries
        ind <- which(!is.na(ui[i, ]) & !is.na(ui[j, ]))

        # compute chebyshev distance
        sim[i, j] <- max(abs(ui[i, ][ind] - ui[j, ][ind]))
      }
    }
  } else {
    # construct similarity matrix skeleton
    n <- ncol(ui)
    sim <- matrix(NA, nrow = n, ncol = n)

    # fill upper triangle of sim matrix
    for (i in 1:n) {
      for (j in i:n) {
        # find non NA entries
        ind <- which(!is.na(ui[, i]) & !is.na(ui[, j]))

        # compute chebyshev distance
        sim[i, j] <- max(abs(ui[, i][ind] - ui[, j][ind]))
      }
    }
  }

  # fill remainder of matrix
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]

  # transform into similarity matrix
  return(1 / (1 + sim))
}

gen_ups_sim <- function(ui) {
  # construct similarity matrix skeleton
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  # user rating means
  mean <- rowMeans(ui, na.rm = TRUE)

  # fill upper triangle of sim matrix
  for (i in 1:n) {
    for (j in i:n) {
      # find non NA entries intersection and union
      ind <- which(!is.na(ui[i, ]) & !is.na(ui[j, ]))
      denom <- which(!is.na(ui[i, ]) | !is.na(ui[j, ]))

      # compute UPS
      num <- exp(-mean(abs(ui[i, ][ind] - ui[j, ][ind]))
                 * abs(mean[i] - mean[j])) * length(ind)
      sim[i, j] <- num / length(denom)
    }
  }

  # fill remainder of matrix
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return(sim)
}

cos_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  for (i in 1:n) {
    for (j in 1:3) {
      clust_dist[i, j] <- cosine(ui[i, ], centres[[j]])
    }
  }
  return(clust_dist)
}

acos_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  mean <- rowMeans(t(ui), na.rm = TRUE)
  ui0 <- t(t(ui) - mean)

  for (i in 1:n) {
    for (j in 1:3) {
      clust_dist[i, j] <- cosine(ui0[i, ], centres[[j]] - mean)
    }
  }
  return(clust_dist)
}

pcc_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  mean <- rowMeans(ui, na.rm = TRUE)
  ui0 <- ui - mean

  for (i in 1:n) {
    for (j in 1:3) {
      clust_dist[i, j] <- cosine(ui0[i, ], centres[[j]] -
                                   mean(centres[[j]], na.rm = TRUE))
    }
  }
  return(clust_dist)
}

jacc_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  for (i in 1:n) {
    for (j in 1:3) {
      num <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
      denom <- which(!is.na(ui[i, ]) | !is.na(centres[[j]]))
      clust_dist[i, j] <- length(num) / length(denom)
    }
  }
  return(clust_dist)
}

euc_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  for (i in 1:n) {
    for (j in 1:3) {
      ind <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
      if (length(ind) == 0) {
        clust_dist[i, j] <- Inf
      } else {
        clust_dist[i, j] <- norm(ui[i, ][ind] - centres[[j]][ind], type = "2")
      }
    }
  }
  return(1 / (1 + clust_dist))
}

mhat_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  for (i in 1:n) {
    for (j in 1:3) {
      ind <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
      if (length(ind) == 0) {
        clust_dist[i, j] <- Inf
      } else {
        clust_dist[i, j] <- sum(abs(ui[i, ][ind] - centres[[j]][ind]))
      }
    }
  }
  return(1 / (1 + clust_dist))
}

cheb_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  for (i in 1:n) {
    for (j in 1:3) {
      ind <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
      if (length(ind) == 0) {
        clust_dist[i, j] <- Inf
      } else {
        clust_dist[i, j] <- max(abs(ui[i, ][ind] - centres[[j]][ind]))
      }
    }
  }
  return(1 / (1 + clust_dist))
}

ups_clust <- function(ui, centres) {
  n <- nrow(ui)
  clust_dist <- matrix(NA, nrow = n, ncol = 3)

  mean <- rowMeans(ui, na.rm = TRUE)

  for (i in 1:n) {
    for (j in 1:3) {
      ind <- which(!is.na(ui[i, ]) & !is.na(centres[[j]]))
      denom <- which(!is.na(ui[i, ]) | !is.na(centres[[j]]))

      num <- exp(-mean(abs(ui[i, ][ind] - centres[[j]][ind]))
                 * abs(mean[i] - mean(centres[[j]], na.rm = TRUE)))
      clust_dist[i, j] <- num * length(ind) / length(denom)
    }
  }
  return(clust_dist)
}