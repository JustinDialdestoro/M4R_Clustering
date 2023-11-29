library("cluster")
library("fastDummies")
library("fpc")

range_normalise <- function(x) {
  # normalise variable to a [0,1] range
  return((x - min(x)) / (max(x) - min(x)))
}

gow_pam <- function(df, k) {
  # remove id and zip variable
  df$userID <- NULL
  df$zip <- NULL
  # range normalise age variable
  df$age <- range_normalise(df$age)
  # binarise gender variable
  df$gender <- as.numeric(df$gender == "M")
  # dummy code occupation variable
  df <- dummy_cols(df, select_columns = "occupation")
  df$occupation <- NULL

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "gower")

  return(pam(dsim, k = k)$clustering)
}

hl_pam <- function(df, k) {
  # remove id and zip variable
  n_u <- length(df$userID)
  df$userID <- NULL
  df$zip <- NULL
  # range normalise age variable
  df$age <- range_normalise(df$age)
  # binarise gender variable
  df$gender <- as.numeric(df$gender == "M")
  # dummy code occupation variable
  df <- dummy_cols(df, select_columns = "occupation")
  n_cat <- length(unique(df$occupation))
  df$occupation <- NULL

  # compute categorical scaling factor
  fac <- distancefactor(n_cat, n_u)
  df[3:(2 + n_cat)] <- df[3:(2 + n_cat)] * fac

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  return(pam(dsim, k = k)$clustering)
}
