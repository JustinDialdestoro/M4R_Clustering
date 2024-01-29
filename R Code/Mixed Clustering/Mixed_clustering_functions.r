library("cluster")
library("fastDummies")
library("fpc")
library("clustMixType")
library("klaR")
library("kmed")
library("kamila")
library("FactoMineR")
library("clustrd")

range_normalise <- function(x) {
  # numericise variable
  xnew <- as.numeric(x)

  # normalise variable to a [0,1] range
  x <- (xnew - min(xnew)) / (max(xnew) - min(xnew))

  return(x)
}

unit_var_normalise <- function(x) {
  # subtract mean from original variable
  xnew <- x - mean(x)

  # divide by standard deviation
  xnew <- xnew / sd(x)

  return(xnew)
}

gow_pam <- function(df, k, user = TRUE) {
  if (user == TRUE) {
    # remove zip variable
    df$zip <- NULL

    # binarise gender variable
    df$gender <- as.numeric(df$gender == "M")

    # dummy code occupation variable
    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL
  } else {
    # range normalise continuous variables
    df$year <- range_normalise(df$year)
    df$runtime <- range_normalise(df$runtime)

    # factorise nominal variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
  }

  # gower dissimilarity matrix
  dsim <- daisy(df, metric = "gower")

  return(pam(dsim, k = k)$clustering)
}

hl_pam <- function(df, k, user = TRUE) {
  if (user == TRUE) {
    n_u <- nrow(df)

    # remove zip variable
    df$zip <- NULL

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
    n_i <- nrow(df)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

    # compute scaling factor for genre
    genre_fac <- distancefactor(19, n_i)
    df[6:24] <- df[6:24] * genre_fac

    # dummy code title type
    df <- dummy_cols(df, select_columns = "titleType")
    # compute scaling factor
    n_type <- length(unique(df$titleType))
    df$titleType <- NULL
    type_fac <- distancefactor(n_type, n_i)
    df[24:29] <- df[24:29] * type_fac

    # dummy code director type
    df <- dummy_cols(df, select_columns = "director")
    # compute scaling factor
    n_dir <- length(unique(df$director))
    df$director <- NULL
    dir_fac <- distancefactor(n_dir, n_i)
    df[29:59] <- df[29:59] * dir_fac

    # dummy code writer type
    df <- dummy_cols(df, select_columns = "writer")
    # compute scaling factor
    n_wri <- length(unique(df$writer))
    df$writer <- NULL
    wri_fac <- distancefactor(n_wri, n_i)
    df[59:84] <- df[59:84] * wri_fac
  }

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  return(pam(dsim, k = k)$clustering)
}

kprototypes <- function(df, k, user = TRUE) {
  if (user == TRUE) {
    # remove zip variable
    df$zip <- NULL

    # binarise gender variable
    df$gender <- as.numeric(df$gender == "M")

    # dummy code occupation variable
    df$occupation <- as.factor(df$occupation)

    return(kproto(df, k)$cluster)
  } else {
    # include only genre variables
    df[1:5] <- NULL

    return(kmodes(df, 2)$cluster)
  }
}

mixed_k <- function(df, k) {
  # remove zip variable
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

  return(gmsClust(df[1:2], df[4:24], k)$results$cluster)
}

famd <- function(df, k) {
  # remove zip variable
  df$zip <- NULL

  pca <- FAMD(df, k, graph = FALSE)$ind$coord

  return(kmeans(pca, k)$cluster)
}

mrkmeans <- function(df, k, user = TRUE) {
  if (user == TRUE) {
    # remove zip variable
    df$zip <- NULL

    # dummy code gender and occupation variable
    df <- dummy_cols(df, select_columns = "gender")
    df$gender <- NULL

    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL
  } else {
    # include only genre variables
    df[1:5] <- NULL
  }

  return(cluspca(df, k)$cluster)
}

kamila_clust <- function(df, k) {
  # remove zip variable
  df$zip <- NULL

  # binarise gender variable
  df$gender <- as.factor(df$gender)
  # factorise occupation variable
  df$occupation <- as.factor(df$occupation)

  return(kamila(df[1], df[2:3], k, 10)$finalMemb)
}