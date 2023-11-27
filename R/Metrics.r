library("recommenderlab")

#gen_cos_sim <- function(ui) {
#  ui0 <- ui
#  ui0[is.na(ui0)] <- 0
#  sim <- ui0 %*% t(ui0)
#  denom <- sqrt(diag(sim))
#  return(t(sim / denom) / denom)
#}

gen_cos_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "cosine",
                    which = "users")
  return(1 - as(sim, "matrix"))
}

gen_pcc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "pearson",
                    which = "users")
  return(1-as(sim, "matrix"))
}

gen_jacc_sim <- function(ui) {
  sim <- similarity(as(ui, "realRatingMatrix"), method = "jaccard",
                    which = "users")
  return(as(sim, "matrix"))
}

r2 <- function(true, pred) {
  return(cor(true, pred)^2)
}