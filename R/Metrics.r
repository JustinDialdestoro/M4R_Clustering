library("recommenderlab")

gen_cos_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "cosine",
                    which = "users")
  return(1 - as(sim, "matrix"))
}

gen_pcc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "pearson",
                    which = "users")
  return(1 - as(sim, "matrix"))
}

gen_jacc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "jaccard",
                    which = "users")
  return(as(sim, "matrix"))
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