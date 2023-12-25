gen_cos_sim <- function(ui) {
  ui0 <- ui
  ui0[is.na(ui0)] <- 0
  sim <- ui0 %*% t(ui0)
  denom <- sqrt(diag(sim))
  return(t(sim / denom) / denom)
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