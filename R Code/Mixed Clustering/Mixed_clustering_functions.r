library("cluster")
library("fastDummies")
library("fpc")
library("clustMixType")
library("kmed")
library("kamila")
library("FactoMineR")
library("clustrd")

range_normalise <- function(x) {
  # normalise variable to a [0,1] range
  return((x - min(x)) / (max(x) - min(x)))
}

gow_pam <- function(df, k, user = TRUE) {
  if (user == TRUE) {
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
  } else {
    # include only genre variables
    df[1:5] <- NULL
  }

  # gower dissimilarity matrix
  dsim <- daisy(df, metric = "gower")

  return(pam(dsim, k = k)$clustering)
}

hl_pam <- function(df, k, user = TRUE) {
  n_u <- nrow(df)

  if (user == TRUE) {
    # remove id and zip variable
    df$userID <- NULL
    df$zip <- NULL

    # range normalise age variable
    df$age <- range_normalise(df$age)

    # binarise gender variable
    df$gender <- as.numeric(df$gender == "M")
    # compute gender scaling factor
    gender_fac <- distancefactor(2, n_u)
    # scale occupation variable
    df$gender <- df$gender * gender_fac

    # dummy code occupation variable
    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL
    # compute occupation scaling factor
    n_occ <- length(unique(df$occupation))
    occ_fac <- distancefactor(n_occ, n_u)
    # scale occupation variable
    df[3:(2 + n_occ)] <- df[3:(2 + n_occ)] * occ_fac

  } else {
    # include only genre variables
    df[1:5] <- NULL
    # compute genre scaling factor
    n_gen <- ncol(df)
    gen_fac <- distancefactor(n_gen, n_u)
    # scale genre variables
    df <- df * gen_fac
  }

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  return(pam(dsim, k = k)$clustering)
}

kprototypes <- function(df, k, user = TRUE) {
  if (user == TRUE) {
    # remove id and zip variable
    df$userID <- NULL
    df$zip <- NULL

    # binarise gender variable
    df$gender <- as.numeric(df$gender == "M")

    # dummy code occupation variable
    df$occupation <- as.factor(df$occupation)

    return(kproto(df, k)$cluster)
  } else {
    # include only genre variables
    df[1:5] <- NULL

    return(kmeans(df, 2))
  }
}

mixed_k <- function(df, k) {
  # remove id and zip variable
  df$userID <- NULL
  df$zip <- NULL
  # binarise gender variable
  df$gender <- as.numeric(df$gender == "M")
  # factorise occupation variable
  df$occupation <- as.factor(df$occupation)

  dist <- distmix(df, method = "ahmad", idnum = 1, idbin = 2, idcat = 3)

  return(fastkmed(dist, k)$cluster)
}

mskmeans <- function(df, k) {
  # remove zip variable
  df$zip <- NULL

  # dummy code gender and occupation variable
  df <- dummy_cols(df, select_columns = "gender")
  df$gender <- NULL

  df <- dummy_cols(df, select_columns = "occupation")
  df$occupation <- NULL

  df$userID <- df$age

  return(gmsClust(df[1:2], df[3:25], k)$results$cluster)
}

famd <- function(df, k) {
  # remove id and zip variable
  df$userID <- NULL
  df$zip <- NULL

  pca <- FAMD(df, k, graph = FALSE)$ind$coord

  return(kmeans(pca, k)$cluster)
}

mrkmeans <- function(df, k) {
  # remove id and zip variable
  df$userID <- NULL
  df$zip <- NULL

  # dummy code gender and occupation variable
  df <- dummy_cols(df, select_columns = "gender")
  df$gender <- NULL

  df <- dummy_cols(df, select_columns = "occupation")
  df$occupation <- NULL

  return(cluspca(df, k)$cluster)
}

kamila_clust <- function(df, k) {
  # remove id and zip variable
  df$userID <- NULL
  df$zip <- NULL

  # binarise gender variable
  df$gender <- as.factor(df$gender)
  # factorise occupation variable
  df$occupation <- as.factor(df$occupation)

  return(kamila(df[1], df[2:3], k, 10)$finalMemb)
}