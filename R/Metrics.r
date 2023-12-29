library("cluster")

cosine <- function(x, y) {
  ind <- which(!is.na(x) & !is.na(y))
  if (length(ind) == 0) {
    return(0)
  }
  return((x[ind] %*% y[ind]) /
           (norm(x[ind], type = "2") * norm(y[ind], type = "2")))
}

gen_cos_sim <- function(ui) {
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      sim[i, j] <- cosine(ui[i, ], ui[j, ])
    }
  }
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return(sim)
}

gen_acos_sim <- function(ui) {
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  mean <- rowMeans(t(ui), na.rm = TRUE)
  ui0 <- t(t(ui) - mean)

  for (i in 1:n) {
    for (j in i:n) {
      sim[i, j] <- cosine(ui0[i, ], ui0[j, ])
    }
  }
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return((1 + sim) / 2)
}

gen_pcc_sim <- function(ui) {
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  mean <- rowMeans(ui, na.rm = TRUE)
  ui0 <- ui - mean

  for (i in 1:n) {
    for (j in i:n) {
      sim[i, j] <- cosine(ui0[i, ], ui0[j, ])
    }
  }
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return((1 + sim) / 2)
}

gen_jacc_sim <- function(ui) {
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      num <- which(!is.na(ui[i, ]) & !is.na(ui[j, ]))
      denom <- which(!is.na(ui[i, ]) | !is.na(ui[j, ]))
      sim[i, j] <- length(num) / length(denom)
    }
  }
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return(sim)
}

gen_euc_sim <- function(ui) {
  sim <- as(dist(ui, "euclidean"), "matrix")
  return(1 / (1 + sim))
}

gen_mhat_sim <- function(ui) {
  sim <- as(dist(ui, "manhattan"), "matrix")
  return(1 / (1 + sim))
}

gen_cheb_sim <- function(ui) {
  n <- nrow(ui)
  sim <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      ind <- which(!is.na(ui[i, ]) & !is.na(ui[j, ]))
      sim[i, j] <- max(abs(ui[i, ][ind] - ui[j, ][ind]))
    }
  }
  sim[lower.tri(sim, diag = FALSE)] <- t(sim)[lower.tri(t(sim), diag = FALSE)]
  return(1 / (1 + sim))
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