source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

fuzzy_gow <- function(df, c, m, user = TRUE, e = 1e-5) {
  # initialise number of data points
  n <- nrow(df)
  p <- ncol(df)

  pr <- 1

  # transform data
  x <- gow_df(df, user) # nolint

  # function to compute loss
  loss_fn <- function(v) {
    # reshape parameters into matrix
    v <- t(matrix(v, ncol = c))
    colnames(v) <- colnames(x[1:pr])

    # compute dissimilarity matrix
    d <- as.matrix(daisy(rbind(x[1:pr], v), metric = "gower"))**(1 / (m - 1)) # nolint

    return(sum(u_old**m %*% d[1:n, (n + 1):(n + c)]))
  }

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # initialise centroids v^(1) continuous variables
  v_old <- matrix(t(x[sample(n, c), ][1:pr]), nrow = 1)
  v_con <- t(matrix(optim(v_old, loss_fn)$par, ncol = c))
  colnames(v_con) <- colnames(x[1:pr])

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
  colnames(v_cat) <- colnames(x[(pr + 1):p])
  v_new <- cbind(v_con, v_cat)

  # compute gower disimilarity matrix
  d <- as.matrix(daisy(gow_df(rbind(x, v_new)), metric = "gower"))** # nolint
    (1 / (m - 1))

  # update u^(1)
  u_new <- 1 / t(t(d[(n + 1):(n + c), 1:n]) *
                   colSums(1 / d[(n + 1):(n + c), 1:n], na.rm = TRUE))
  u_new[is.na(u_new)] <- 1

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # numerically find minimisers of loss to update v^(l+1)
    v_con <- matrix(t(v_con), nrow = 1)
    v_con <- t(matrix(optim(v_con, loss_fn)$par, ncol = c))
    colnames(v_con) <- colnames(x[1:pr])

    # initialise centroids v^(1) categorical variables
    v_cat <- NULL
    # loop over each categorical variable
    for (i in (pr + 1):p) {
      # compute loss when each level is used
      loss <- NULL
      for (level in unique(x[, i])) {
        loss <- cbind(loss, u_new**m %*% as.numeric(!(x[, i] == level)))
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
    colnames(v_cat) <- colnames(x[(pr + 1):p])
    v_new <- cbind(v_con, v_cat)

    # compute gower disimilarity matrix
    d <- as.matrix(daisy(gow_df(rbind(x, v_new)), metric = "gower"))** # nolint
      (1 / (m - 1))

    # update u^(1)
    u_new <- 1 / t(t(d[(n + 1):(n + c), 1:n]) *
                     colSums(1 / d[(n + 1):(n + c), 1:n], na.rm = TRUE))
    u_new[is.na(u_new)] <- 1
  }

  return(list(u = u_new, centroids = v_new))
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
  loss <- sum(diag(u_new**m %*% t(d**((m - 1) / 2)))) # nolint
  return(list(u = u_new, centroids = v_new, loss = sum(loss)))
}

kproto_dsim <- function(df, v, c, pr, lambda) {
  n <- nrow(df)
  p <- ncol(df)

  dsim_mat <- matrix(NA, nrow = c, ncol = n)

  for (i in 1:c) {
    for (j in 1:n) {
      dsim_mat[i, j] <- norm(v[i, 1:pr] - df[j, 1:pr], type = "2")**2 +
        lambda * sum(!(v[i, (pr + 1):p] == as.numeric(df[j, (pr + 1):p])))
    }
  }
  return(dsim_mat)
}

fuzzy_kproto <- function(df, c, m, user = TRUE, e = 1e-5) { # nolint
  # initialise number of data points
  n <- nrow(df)

  # transform data
  x <- kproto_df(df, user) # nolint

  # initialise number of continuous and categorical variables
  if (user == TRUE) {
    pr <- 1
  } else {
    pr <- 2
    # swap columns to have continuous variables first
    x[, c(1, 2, 3)] <- x[, c(2, 3, 1)]
    colnames(x) <- c("year", "runtime", "titleType", colnames(x[, 4:24]))
  }
  p <- ncol(x)

  # initialise variable weights
  lambda <- lambdaest(x) # nolint

  # initialise cluster memberships u^(0)
  u_old <- matrix(runif(n * c), nrow = c)
  u_old <- t(t(u_old) / colSums(u_old, na.rm = TRUE))

  # initialise centroids v^(1) continuous variables
  v_cont <- (u_old**m %*% as.matrix(x[, 1:pr])) /
    rowSums(u_old**m, na.rm = TRUE)

  # initialise centroids v^(1) categorical variables
  v_cat <- NULL
  # loop over each categorical variable
  for (i in (pr + 1):(pr + 3)) {
    # compute loss when each level is used
    loss <- NULL
    for (level in unique(x[, i])) {
      loss <- cbind(loss, u_old**m %*% as.numeric(!(x[, i] == level)))
    }
    # find best level
    best_level <- NULL
    for (j in 1:c) {
      best_level <- rbind(best_level,
                          unique(x[, i])[which(loss[j, ] == min(loss[j, ]))])
    }
    v_cat <- cbind(v_cat, best_level)
  }
  # loop over each genre variable
  for (i in (pr + 4):p) {
    count <- table(x[, i])
    scale <- count[2] / count[1]
    # compute loss when each level is used
    loss <- u_old**m %*% as.numeric(!(x[, i] == TRUE))
    loss <- cbind(loss, u_old**m %*%
                    (scale * as.numeric(x[, i] == FALSE)))
    # find best level
    best_level <- NULL
    for (j in 1:c) {
      best_level <- rbind(best_level,
                          unique(x[, i])[which(loss[j, ] == min(loss[j, ]))])
    }
    v_cat <- cbind(v_cat, best_level)
  }
  v_new <- cbind(v_cont, v_cat)

  # update u^(1)
  d <- kproto_dsim(x, v_new, c, pr, lambda)**(1 / (m - 1)) # nolint
  u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))

  # iterate until convergence
  while (norm(u_old - u_new, type = "F") > e) {
    u_old <- u_new

    # update continuous variables of v^(l+1)
    v_cont <- (u_new**m %*% as.matrix(x[, 1:pr])) /
      rowSums(u_new**m, na.rm = TRUE)

    # initialise centroids v^(1) categorical variables
    v_cat <- NULL
    # loop over each categorical variable
    for (i in (pr + 1):(pr + 3)) {
      # compute loss when each level is used
      loss <- NULL
      for (level in unique(x[, i])) {
        loss <- cbind(loss, u_old**m %*% as.numeric(!(x[, i] == level)))
      }
      # find best level
      best_level <- NULL
      for (j in 1:c) {
        best_level <- rbind(best_level,
                            unique(x[, i])[which(loss[j, ] == min(loss[j, ]))])
      }
      v_cat <- cbind(v_cat, best_level)
    }
    # loop over each genre variable
    for (i in (pr + 4):p) {
      count <- table(x[, i])
      scale <- count[2] / count[1]
      # compute loss when each level is used
      loss <- u_old**m %*% as.numeric(!(x[, i] == TRUE))
      loss <- cbind(loss, u_old**m %*%
                      (scale * as.numeric(x[, i] == FALSE)))
      # find best level
      best_level <- NULL
      for (j in 1:c) {
        best_level <- rbind(best_level,
                            unique(x[, i])[which(loss[j, ] == min(loss[j, ]))])
      }
      v_cat <- cbind(v_cat, best_level)
    }
    v_new <- cbind(v_cont, v_cat)

    # update u^(l+1)
  d <- kproto_dsim(x, v_new, c, pr, lambda)**(1 / (m - 1)) # nolint
  u_new <- 1 / t(t(d) * colSums(1 / d, na.rm = TRUE))
  }

  # compute final loss
  loss <- sum(diag(u_new**m %*% t(d)))
  return(list(u = u_new, centroids = v_new, loss = sum(loss)))
}