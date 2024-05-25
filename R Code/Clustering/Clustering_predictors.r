mean_centered_clust <- function(ui, userid, filmid, sim, user = TRUE) {
  if (user == TRUE) {
    # compute target user rating mean
    mu_u <- mean(ui[userid, ], na.rm = TRUE)

    # compute neighbour users rating mean
    mu_v <- rowMeans(ui[-userid, ], na.rm = TRUE)

    # find indices with ratings
    ind <- which(!is.na(ui[-userid, filmid]))
    if (length(ind) == 0) {
      return(mu_u)
    }

    # compute rating prediction
    num <- sim[-userid, userid][ind] %*% (ui[-userid, filmid] - mu_v)[ind]
    denom <- sum(abs(sim[-userid, userid][ind])) + 1e-9

    return(mu_u + num / denom)

  } else {
    # compute target film rating mean
    mu_i <- mean(ui[, filmid], na.rm = TRUE)

    # compute neighbour films rating mean
    mu_j <- colMeans(ui[, -filmid], na.rm = TRUE)

    # find indices with ratings
    ind <- which(!is.na(ui[userid, -filmid]))
    if (length(ind) == 0) {
      return(mu_i)
    }

    # compute rating prediction
    num <- sim[filmid, -filmid][ind] %*% (ui[userid, -filmid] - mu_j)[ind]
    denom <- sum(abs(sim[-filmid, filmid][ind])) + 1e-9

    return(mu_i + num / denom)
  }
}

average_clust <- function(ui, userid, filmid, user = TRUE) {
  if (user == TRUE) {
    # compute rating prediction
    pred <- mean(ui[-userid, filmid], na.rm = TRUE)
  } else {
    # compute rating prediction
    pred <- mean(ui[userid, -filmid], na.rm = TRUE)
  }

  return(pred)
}