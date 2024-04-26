source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

fuzzy_gow <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- gow_df(df, user) # nolint

  # compute gower disimilarity matrix
  d <- as.matrix(daisy(x, metric = "gower"))**(1 / (m - 1)) # nolint

  # initalise v^(0)
  ind <- sample.int(n, c)

  # update u^(1)
  u_old <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_old[is.na(u_old)] <- 1

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

  # update u^(2)
  u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # compute loss
    loss <- u_new**m %*% d
    # find minimisers of loss and set to v^(l)
    ind <- which(loss[1, ] == min(loss[1, ]))[1]
    v_new <- x[ind, ]
    for (i in 2:c) {
      # ensure centroids are unique
      inds <- which(loss[i, ] == min(loss[i, -ind]))
      ind <- c(ind, inds[which(!(inds %in% ind))[1]])
      v_new <- rbind(v_new, x[ind[i], ])
    }

    # update u^(l+1)
    u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }
  # compute loss
  loss <- u_new**m %*% d[, ind]**2

  return(list(u = u_new, centroids = v_new, loss = sum(diag(loss))))
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
  loss <- sum(diag(u_new**m %*% t(d**((m - 1) / 2))))
  return(list(u = u_new, centroids = v_new, loss = sum(loss)))
}

kproto_dsim <- function(df, v, c, pr, lambda) {
  n <- nrow(df)
  p <- ncol(df)

  dsim_mat <- matrix(NA, nrow = c, ncol = n)

  for (i in 1:c) {
    dsim_mat[i, ] <- rowSums(t(t(as.matrix(df[, 1:pr])) -
                                 as.numeric(v[i, 1:pr]))**2) +
      lambda * rowSums(t(!(t(as.matrix(df[, (pr + 1):p]) ==
                               as.vector(v[i, (pr + 1):p])))))
  }

  return(dsim_mat)
}

fuzzy_kproto <- function(df, c, m, user = TRUE, e = 1e-5) { # nolint
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- kproto_df(df, user) # nolint

  # initialise number of continuous and categorical variables
  p <- ncol(x)
  if (user == TRUE) {
    pr <- 1
  } else {
    pr <- 21
    # swap columns to have continuous variables first
    x <- x[, c(2, 3, 6:24, 1, 4, 5)]
  }

  # initialise variable weights
  lambda <- lambdaest(x) # nolint

  # initalise v^(0)
  ind <- sample.int(n, c)

  # update u^(1)
  d <- kproto_dsim(x, x[ind, ], c, pr, lambda)**(2 / (m - 1)) # nolint
  u_old <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  # initialise centroids v^(1) continuous variables
  v_con <- (u_old**m %*% as.matrix(x[, 1:pr])) /
    rowSums(u_old**m, na.rm = TRUE)

  # initialise centroids v^(1) categorical variables
  v_cat <- NULL
  # loop over each categorical variable
  for (i in (pr + 1):p) {
    # compute loss when each level is used
    loss <- NULL
    for (level in unique(x[, i])) {
      loss <- cbind(loss, u_old**m %*% as.numeric(!(x[, i] == level)))
    }
    # find best level
    best_level <- NULL
    for (j in 1:c) {
      best_level <- rbind(best_level,
                          as.character(unique(x[, i])
                                       [which(loss[j, ] == min(loss[j, ]))]))
    }
    v_cat <- cbind(v_cat, best_level)
  }
  v_new <- data.frame(v_con, v_cat)

  # update u^(2)
  d <- kproto_dsim(x, v_new, c, pr, lambda)**(2 / (m - 1))
  u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # update centroids v^(l+1) continuous variables
    v_con <- (u_old**m %*% as.matrix(x[, 1:pr])) /
      rowSums(u_old**m, na.rm = TRUE)

    # update centroids v^(l+1) categorical variables
    v_cat <- NULL
    # loop over each categorical variable
    for (i in (pr + 1):p) {
      # compute loss when each level is used
      loss <- NULL
      for (level in unique(x[, i])) {
        loss <- cbind(loss, u_old**m %*% as.numeric(!(x[, i] == level)))
      }
      # find best level
      best_level <- NULL
      for (j in 1:c) {
        best_level <- rbind(best_level,
                            as.character(unique(x[, i])
                                         [which(loss[j, ] == min(loss[j, ]))]))
      }
      v_cat <- cbind(v_cat, best_level)
    }
    v_new <- data.frame(v_con, v_cat)

    # update u^(2)
    d <- kproto_dsim(x, v_new, c, pr, lambda)**(2 / (m - 1))
    u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))
  }
  # compute loss
  loss <- u_new**m %*% t(d**((m - 1) / 2))

  return(list(u = u_new, centroids = v_new, loss = sum(diag(loss))))
}

fuzzy_mixed_k <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- mixed_k_df(df, user) # nolint

  if (user == TRUE) {
    # compute gower disimilarity matrix
    d <- as.matrix(distmix(x, method = "ahmad", idnum = 1, # nolint
                           idbin = 2, idcat = 3))**(1 / (m - 1))
  } else {
    # compute gower disimilarity matrix
    d <- as.matrix(distmix(x, method = "ahmad", idnum = 2:3, # nolint
                           idbin = 6:24, idcat = c(1, 4, 5)))**(1 / (m - 1))
  }

  # initalise v^(0)
  ind <- sample.int(n, c)

  # update u^(1)
  u_old <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_old[is.na(u_old)] <- 1

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

  # update u^(2)
  u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # compute loss
    loss <- u_new**m %*% d
    # find minimisers of loss and set to v^(l)
    ind <- which(loss[1, ] == min(loss[1, ]))[1]
    v_new <- x[ind, ]
    for (i in 2:c) {
      # ensure centroids are unique
      inds <- which(loss[i, ] == min(loss[i, -ind]))
      ind <- c(ind, inds[which(!(inds %in% ind))[1]])
      v_new <- rbind(v_new, x[ind[i], ])
    }

    # update u^(l+1)
    u_new <- 1 / t(t(d[ind, ]) * colSums(1 / d[ind, ], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }
  # compute loss
  loss <- u_new**m %*% d[, ind]**2

  return(list(u = u_new, centroids = v_new, loss = sum(diag(loss))))
}

mskmeans_dsim <- function(df, v, c, pr, gamma, ratio = FALSE) {
  n <- nrow(df)
  p <- ncol(df)

  if (ratio == TRUE) {
    # continuous dissimilarities
    dsim_mat_con <- rowSums(t(t(df[, 1:pr]) - v[1:pr])**2)

    # categorical dissimilarities
    dsim_mat_cat <- gamma * as.matrix(df[, (pr + 1):p]) %*%
      as.matrix(v[(pr + 1):p]) /
      (sqrt(rowSums(as.matrix(df[, (pr + 1):p])**2)) *
         sqrt(rowSums(t(as.matrix(v[(pr + 1):p])**2))))

    return(list(con = dsim_mat_con, cat = as.vector(dsim_mat_cat)))

  } else {
    # continuous dissimilarities
    dsim_mat_con <- matrix(NA, nrow = c, ncol = n)
    for (i in 1:c) {
      dsim_mat_con[i, ] <- rowSums(t(t(as.matrix(df[, 1:pr])) -
                                       as.numeric(v[i, 1:pr]))**2)
    }

    # categorical dissimilarities
    dsim_mat_cat <- gamma * as.matrix(df[, (pr + 1):p]) %*%
      t(as.matrix(v[, (pr + 1):p])) /
      outer(sqrt(rowSums(as.matrix(df[, (pr + 1):p])**2)),
            sqrt(rowSums(as.matrix(v[, (pr + 1):p])**2)))

    return(dsim_mat_con + t(dsim_mat_cat))
  }
}

fuzzy_mskmeans <- function(df, c, m, user = TRUE, e = 1e-5,
                           gammas = seq(0.125, 1, 0.125)) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- mskmeans_df(df, user) # nolint

  # initialise number of continuous and categorical variables
  p <- ncol(x)
  if (user == TRUE) {
    pr <- 1
  } else {
    pr <- 2
  }

  # initialise full data centroids
  cbar <- c(colMeans(as.matrix(x[, 1:pr])),
            colSums(as.matrix(x[, (pr + 1):p])) /
              sqrt(sum(colSums(as.matrix(x[, (pr + 1):p])**2))))

  # initialise best q
  best_q <- Inf

  for (gamma in gammas) {
    # initialise cluster memberships u^(0)
    u_old <- matrix(runif(n * c), nrow = c)
    u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

    # initialise centroids v^(1) continuous variables
    v_con <- (u_old**m %*% as.matrix(x[, 1:pr])) /
      rowSums(u_old**m, na.rm = TRUE)

    v_cat <- t(t(u_old**m) / sqrt(rowSums(x[, (pr + 1):p]**2))) %*%
      as.matrix(x[, (pr + 1):p])
    v_cat <- v_cat / sqrt(rowSums(v_cat**2))
    v_new <- data.frame(v_con, v_cat)

    # update u^(2)
    d <- mskmeans_dsim(x, v_new, c, pr, gamma)**(1 / (m - 1))
    u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

    # iterate until convergence
    while (norm(u_old - u_new, type = "F") > e) {
      u_old <- u_new

      # update centroids v^(l+1) continuous variables
      v_con <- (u_old**m %*% as.matrix(x[, 1:pr])) /
        rowSums(u_old**m, na.rm = TRUE)

      v_cat <- t(t(u_old**m) / sqrt(rowSums(x[, (pr + 1):p]**2))) %*%
        as.matrix(x[, (pr + 1):p])
      v_cat <- v_cat / sqrt(rowSums(v_cat**2))
      v_new <- data.frame(v_con, v_cat)

      # update u^(l+1)
      d <- mskmeans_dsim(x, v_new, c, pr, gamma)**(1 / (m - 1))
      u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))
    }

    # compute within cluster distortion
    gam <- diag(u_new**m %*% t(d**((m - 1) / 2)))

    # compute between cluster distortion
    del_d <- mskmeans_dsim(x, cbar, 1, pr, gamma, TRUE)

    q <- gam[1] / (sum(del_d$con) - gam[1]) *
      gam[2] / (sum(del_d$cat) - gam[2])

    if (q < best_q) {
      best_q <- q
      out <- list(u = u_new, centroids = v_new, loss = sum(gam), gamma = gamma)
    }
  }

  return(out)
}

fuzzy_famd <- function(df, c, m, user = TRUE, e = 1e-5, p = 3) {
  # initialise number of data points
  n <- nrow(df)

  # transform data into lower dimensional space
  x <- FAMD(famd_df(df, user), p, graph = FALSE)$ind$coord # nolint

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
  loss <- sum(diag(u_new**m %*% t(d**((m - 1) / 2))))

  return(list(u = u_new, centroids = v_new, loss = sum(loss)))
}

optim_uv <- function(u, b, x, c, m, e) {
  u_new <- u
  u_old <- 0

  while (norm(u_new - u_old, type = "F") > e) {
    u_old <- u_new
    v <- solve(u_old %*% t(u_old)) %*% u_old %*% x %*% b

    u_new <- NULL
    for (i in 1:c) {
      d <- t(b) %*% (t(x) - b %*% t(v) %*% u_old)
      u <- 1 / colSums(((rep(1, c) %*% t(v[i, ]) %*% d) /
                          (v %*% d))**(1 / (m - 1)))
      u_new <- rbind(u_new, u)
    }
  }

  return(u_new)
}

fuzzy_mrkmeans <- function(df, c, m, user = TRUE, e = 1e-2, p = 3) {
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- as.matrix(mrkmeans_df(df, user)) # nolint

  # initialise loss
  loss_old <- Inf

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))**m

  # initalise projection matrix p^(0)
  p_new <- t(u_old) %*% solve(u_old %*% t(u_old)) %*% u_old

  # initalise loadings matrix b^(0)
  b_new <- eigen(t(x) %*% p_new %*% x, symmetric = TRUE)$vectors[, 1:p]

  # update u^(1)
  u_new <- fuzzy_c_means(x %*% b_new, c, m)$u**m # nolint

  # compute loss
  loss_new <- norm(x - p_new %*% x %*% b_new %*% t(b_new), type = "F")

  # iterate until convergence
  while (abs(loss_new - loss_old) > e) {
    u_old <- u_new
    loss_old <- loss_new

    # update projection matrix p^(l+1)
    p_new <- t(u_new) %*% solve(u_new %*% t(u_new)) %*% u_new

    # update loadings matrix b^(l+1)
    b_new <- eigen(t(x) %*% p_new %*% x, symmetric = TRUE)$vectors[, 1:p]

    # update u^(l+1)
    u_new <- fuzzy_c_means(x %*% b_new, c, m)$u**m # nolint

    # update loss
    loss_new <- norm(x - p_new %*% x %*% b_new %*% t(b_new), type = "F")
  }

  # compute final centroids
  v <- (u_new / rowSums(u_new)) %*% x %*% b_new

  return(list(u = u_new**0.5, centroids = v, loss = loss_new))
}