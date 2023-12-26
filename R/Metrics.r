library("recommenderlab")

gen_cos_sim_test <- function(ui) {
  ui0 <- ui
  ui0[is.na(ui0)] <- 0
  sim <- ui0 %*% t(ui0)
  denom <- sqrt(diag(sim))
  return(t(sim / denom) / denom)
}

gen_pcc_sim_test <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "pearson",
                    which = "users")
  return(as(sim, "matrix"))
}

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
  return(sim)
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