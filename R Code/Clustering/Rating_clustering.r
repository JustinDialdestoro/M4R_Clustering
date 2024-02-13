library("cluster")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Clustering/Rating_preference_clustering.r")

rating_clust <- function(ui, n, clust_metric, user = TRUE) {
  if (user == TRUE) {
    # choose n random points
    nu <- nrow(ui)
    sample <- sample.int(nu, n)

    # initialise n random centres
    centres <- rep(c(), n)
    for (i in 1:n) {
      centres[[i]] <- ui[sample[i], ]
    }

    old_labels <- rep(0, nu)

    # compute distance from each point to each centre
    dist <- clust_metric(ui, centres, n)
    # assign each point to closest centre
    new_labels <- max.col(dist)

    # run kmeans until labels do not change
    while (!identical(old_labels, new_labels)) {
      # set current labels to be old
      old_labels <- new_labels

      # compute new centres as cluster means
      centres <- rep(c(), n)
      for (i in 1:n) {
        centres[[i]] <- colMeans(ui[which(old_labels == i), ], na.rm = TRUE)
      }
      # compute distance from each point to each centre
      dist <- clust_metric(ui, centres, n)
      # assign each point to closest centre
      new_labels <- max.col(dist)
    }
    return(new_labels)
  } else {
    # choose n random points
    ni <- ncol(ui)
    sample <- sample.int(ni, n)

    # initialise n random centres
    centres <- rep(c(), n)
    for (i in 1:n) {
      centres[[i]] <- ui[, sample[i]]
    }

    old_labels <- rep(0, ni)

    # compute distance from each point to each centre
    dist <- clust_metric(ui, centres, n, user)
    # assign each point to closest centre
    new_labels <- max.col(dist)

    # run kmeans until labels do not change
    while (!identical(old_labels, new_labels)) {
      # set current labels to be old
      old_labels <- new_labels

      # compute new centres as cluster means
      centres <- rep(c(), n)
      for (i in 1:n) {
        centres[[i]] <- rowMeans(ui[, which(old_labels == i)], na.rm = TRUE)
      }
      # compute distance from each point to each centre
      dist <- clust_metric(ui, centres, n, user)
      # assign each point to closest centre
      new_labels <- max.col(dist)
    }
    return(new_labels)
  }
}

cval_clust <- function(df, t, n, k_range, metric, clust_metric,
                       pred_func, user = TRUE) {
  nk <- length(k_range)
  # initial scores table
  scores <- data.frame(rmse = rep(0, nk), mae = rep(0, nk), r2 = rep(0, nk),
                       offline = rep(0, t), online = rep(0, nk))

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
    clusters <- rating_clust(ui, n, clust_metric, user)

    # segment user ratings matrix into the n clusters
    uis <- replicate(n, c())

    if (user == TRUE) {
      for (j in 1:n) {
        uis[[j]] <- ui[which(clusters == j), ]
      }
    } else {
      for (j in 1:n) {
        uis[[j]] <- ui[, which(clusters == j)]
      }
    }

    # similarity matrix for each segmented ui matrix
    sims <- replicate(n, c())
    for (j in 1:n) {
      sims[[j]] <- metric(uis[[j]], user)
    }

    time <- Sys.time() - t1
    print(time)
    scores$offline[i] <- time

    # loop over every k
    for (k in seq_along(k_range)) {
      print(paste("Online phase for k =", k_range[k]))
      t1 <- Sys.time()

      # predict on test fold ratings
      r_pred <- pred_fold_clust(df, cval_f_i[[i]], uis, sims, pred_func, # nolint
                                k_range[k], clusters, user)
      r_true <- df$rating[cval_f_i[[i]]]

      # error metrics
      scores$rmse[k] <- scores$rmse[k] + rmse(r_pred, r_true) # nolint
      scores$mae[k] <- scores$mae[k] + mae(r_pred, r_true) # nolint
      scores$r2[k] <- scores$r2[k] + r2(r_pred, r_true) # nolint

      time <- Sys.time() - t1
      print(time)
      scores$online[k] <- scores$online[k] + time
    }
  }
  scores[c(1:3, 5)] <- scores[c(1:3, 5)] / t
  return(scores)
}
