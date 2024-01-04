find_knn <- function(ui, sim, k, userid, filmid) {
  # indices of users who have rated the film
  ind <- which(ui[, filmid] > 0)
  # nearest neighbours
  neighbours <- ind[order(-sim[userid, ][ind])[1:k]]

  # omit NA when not enough neighbours found
  return(na.omit(neighbours))
}

weighted_sum <- function(df, ui, sim, k, userid, filmid) {
  # find nearest neighbours
  neighbours <- find_knn(ui, sim, k, userid, filmid)

  # compute rating prediction
  num <- sim[neighbours, userid] %*% ui[neighbours, filmid]
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(num / denom)
}

mean_centered <- function(df, ui, sim, k, userid, filmid) {
  # find nearest neighbours
  neighbours <- find_knn(ui, sim, k, userid, filmid)

  # compute target user rating mean
  mu_u <- mean(ui[userid, ], na.rm = TRUE)

  # compute neighbour users rating mean
  if (length(neighbours) < 2) {
    mu_v <- mean(ui[neighbours, ], na.rm = TRUE)
  } else {
    mu_v <- rowMeans(ui[neighbours, ], na.rm = TRUE)
  }

  # compute rating prediction
  num <- sim[neighbours, userid] %*% (ui[neighbours, filmid] - mu_v)
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(mu_u + num / denom)
}

z_score <- function(df, ui, sim, k, userid, filmid) {
  # find nearest neighbours
  neighbours <- find_knn(ui, sim, k, userid, filmid)

  # compute target user rating mean and standard deviation
  mu_u <- mean(ui[userid, ], na.rm = TRUE)
  sig_u <- sd(ui[userid, ], na.rm = TRUE)

  # compute neighbour users rating mean and standard deviation
  if (length(neighbours) < 2) {
    mu_v <- mean(ui[neighbours, ], na.rm = TRUE)
    sig_v <- sd(ui[neighbours, ], na.rm = TRUE)
  } else {
    mu_v <- rowMeans(ui[neighbours, ], na.rm = TRUE)
    sig_v <- apply(ui[neighbours, ], 1, sd, na.rm = TRUE)
  }

  # compute rating prediction
  num <- sim[neighbours, userid] %*% ((ui[neighbours, filmid] - mu_v) / sig_v)
  denom <- sum(abs(sim[neighbours, userid])) + 1e-9

  return(mu_u + sig_u * num / denom)
}

discrete <- function(df, ui, sim, k, userid, filmid) {
  # find nearest neighbours
  neighbours <- find_knn(ui, sim, k, userid, filmid)

  # count ratings of neighbours
  rating_count <- table(ui[neighbours, filmid])
  # find most common rating
  top_count <- max(rating_count)
  # compute rating prediction
  top_ratings <- as.numeric(names(rating_count[rating_count == top_count]))

  return(mean(top_ratings))
}