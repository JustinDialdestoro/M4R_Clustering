coclust <- function(ui, n, m) {
  # initialise row and column count
  nu <- nrow(ui)
  ni <- ncol(ui)

  # initial clustering labels
  labels_u <- sample.int(n, nu, replace = TRUE)
  labels_i <- sample.int(m, ni, replace = TRUE)

  # matrix to store means
  old_means <- cluster_means(n, m, ui, labels_u, labels_i)

  # update user labels
  labels_u <- rep(0, nu)
  for (i in 1:nu) {
    labels_u[i] <- nearest_clust(ui, i, old_means, labels_i, n, m, TRUE)
  }

  # update means
  means <- cluster_means(n, m, ui, labels_u, labels_i)

  # update item labels using newly updated user labels
  for (j in 1:ni) {
    labels_i[j] <- nearest_clust(ui, j, means, labels_u, n, m, FALSE)
  }

  # update means
  means <- cluster_means(n, m, ui, labels_u, labels_i)

  # while clustering has not converged
  while (max(abs(means - old_means)) > 1e-1) {
    old_old_means <- old_means
    old_means <- means
    # find new user labels
    for (i in 1:nu) {
      labels_u[i] <- nearest_clust(ui, i, means, labels_i, n, m, TRUE)
    }
    # update means
    means <- cluster_means(n, m, ui, labels_u, labels_i)

    # find new item labels
    for (j in 1:ni) {
      labels_i[j] <- nearest_clust(ui, j, means, labels_u, n, m, FALSE)
    }
    # update means
    means <- cluster_means(n, m, ui, labels_u, labels_i)
    # end loop if iterations are repeating
    if (all(old_old_means == means)) {
      break
    }
  }

  clusters <- rep(c(), 3)
  clusters[[1]] <- labels_u
  clusters[[2]] <- labels_i
  clusters[[3]] <- means

  return(clusters)
}

cluster_means <- function(n, m, ui, labels_u, labels_i) {
  means <- matrix(0, nrow = n, ncol = m)
  # compute cluster means
  for (i in 1:n) {
    for (j in 1:m) {
      means[i, j] <- mean(ui[labels_u == i, labels_i == j],
                          na.rm = TRUE)
    }
  }
  means[is.na(means)] <- 0
  return(means)
}

nearest_clust <- function(ui, ind, means, old_labels, n, m, user) {
  if (user == TRUE) {
    user_means <- rep(0, m)
    # compute users mean restricted to each item cluster
    for (j in 1:m) {
      user_means[j] <- mean(ui[ind, old_labels == j], na.rm = TRUE)
    }

    dist <- rep(0, n)
    # compute distances from users means to each cluster mean
    for (i in 1:n) {
      dist[i] <- norm(means[i, ] - user_means, type = "2")
    }

  } else {
    item_means <- rep(0, n)
    # compute items mean restricted to each user cluster
    for (i in 1:n) {
      item_means[i] <- mean(ui[old_labels == i, ind], na.rm = TRUE)
    }

    dist <- rep(0, m)
    # compute distances from items means to each cluster mean
    for (j in 1:m) {
      dist[j] <- norm(means[, j] - item_means, type = "2")
    }
  }
  if (length(which.min(dist)) == 0) {
    return(c(1))
  }
  # find best user cluster
  return(which.min(dist))
}

pred_fold_clust <- function(df, df_ind, clusters) {
  preds <- c()
  for (p in df_ind) {
    # target prediction id
    userid <- df$userID[p]
    filmid <- df$filmID[p]

    # prediction
    preds <- c(preds,
               clusters[[3]][clusters[[1]][userid], clusters[[2]][filmid]])
  }
  return(preds)
}

cval_coclust <- function(df, t, n, m) {
  # initial scores table
  scores <- data.frame(rmse = rep(0, 1), mae = rep(0, 1), r2 = rep(0, 1),
                       offline = rep(0, 1), online = rep(0, 1))

  # t-fold creation
  cval_f_i <- t_fold_index(df, t) # nolint
  cval_f <- t_fold(df, cval_f_i) # nolint

  # loop over each fold
  for (i in 1:t) {
    print(paste("Offline phase for fold", i, ":"))
    t1 <- Sys.time()

    # ui and similarity matrix
    ui <- gen_ui_matrix(df, cval_f[[i]]) # nolint

    # create user clusters
    clusters <- coclust(ui, n, m)

    time <- Sys.time() - t1
    print(time)
    scores$offline <- scores$offline + time

    print(paste("Online phase for k =", i, ":"))
    t1 <- Sys.time()

    # predict on test fold ratings
    r_pred <- pred_fold_clust(df, cval_f_i[[i]], clusters)
    r_true <- df$rating[cval_f_i[[i]]]

    # error metrics
    scores$rmse <- scores$rmse + rmse(r_pred, r_true) # nolint
    scores$mae <- scores$mae + mae(r_pred, r_true) # nolint
    scores$r2 <- scores$r2 + r2(r_pred, r_true) # nolint

    time <- Sys.time() - t1
    print(time)
    scores$online <- scores$online + time
  }
  scores <- scores / t
  return(scores)
}
