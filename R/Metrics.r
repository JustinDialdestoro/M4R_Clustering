library("lsa")

gen_cos_sim <- function(ui) {
  # ui0 <- ui
  # ui0[is.na(ui0)] <- 0
  sim <- ui %*% t(ui)
  denom <- sqrt(diag(sim))
  return(t(sim / denom) / denom)
}

cosine <- function(x, y) {
  ind <- which(x > 0 & y > 0)
  if (length(ind) == 0) {
    return(0)
  }
  return((x %*% y) / (norm(x[ind], type = "2") * norm(y[ind], type = "2")))
}

gen_cos_sim_2 <- function(ui) {
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

rmse <- function(pred, true) {
  ind <- !is.na(pred)
  r <- pred[ind] - true[ind]
  n <- length(pred[ind])
  return(sqrt(sum(r**2) / n))
}

mae <- function(pred, true) {
  ind <- !is.na(pred)
  r <- abs(pred[ind] - true[ind])
  n <- length(pred[ind])
  return(sum(r) / n)
}

r2 <- function(pred, true) {
  ind <- !is.na(pred)
  return(cor(pred[ind], true[ind])**2)
}