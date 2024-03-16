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

gow_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # range normalise age
    df$age <- range_normalise(df$age)

    # binarise gender variable
    df$gender <- as.factor(df$gender == "M")

    # dummy code occupation variable
    df$occupation <- as.factor(df$occupation)

  } else {
    # range normalise continuous variables
    df$year <- range_normalise(df$year)
    df$runtime <- range_normalise(df$runtime)

    # factorise nominal variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
  }
  return(df)
}

gow_pam <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- gow_df(df, user)

  # gower dissimilarity matrix
  dsim <- daisy(df, metric = "gower")

  if (obj == TRUE) {
    return(pam(dsim, k = k)$objective[2])
  } else {
    return(pam(dsim, k = k)$clustering)
  }
}

hl_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    n_u <- nrow(df)

    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # dummy code gender
    df <- dummy_cols(df, select_columns = "gender")
    # compute scaling factor
    df$gender <- NULL
    gender_fac <- distancefactor(2, n_u)
    # scale occupation variable
    df[3:4] <- df[3:4] * gender_fac

    # dummy code occupation
    df <- dummy_cols(df, select_columns = "occupation")
    # compute scaling factor
    n_occ <- length(unique(df$occupation))
    df$occupation <- NULL
    occ_fac <- distancefactor(n_occ, n_u)
    # scale occupation variable
    df[4:24] <- df[4:24] * occ_fac

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
    # scale title type variable
    df[24:29] <- df[24:29] * type_fac

    # dummy code director
    df <- dummy_cols(df, select_columns = "director")
    # compute scaling factor
    n_dir <- length(unique(df$director))
    df$director <- NULL
    dir_fac <- distancefactor(n_dir, n_i)
    # scale director variable
    df[29:59] <- df[29:59] * dir_fac

    # dummy code writer
    df <- dummy_cols(df, select_columns = "writer")
    # compute scaling factor
    n_wri <- length(unique(df$writer))
    df$writer <- NULL
    wri_fac <- distancefactor(n_wri, n_i)
    # scale writer variable
    df[59:84] <- df[59:84] * wri_fac
  }
  return(df)
}

hl_pam <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- hl_df(df, user)

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  if (obj == TRUE) {
    return(pam(dsim, k = k)$objective[2])
  } else {
    return(pam(dsim, k = k)$clustering)
  }
}

kproto_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # binarise gender variable
    df$gender <- as.factor(df$gender == "M")

    # dummy code occupation variable
    df$occupation <- as.factor(df$occupation)

  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)
  }
  return(df)
}

kprototypes <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- kproto_df(df, user)

  if (obj == TRUE) {
    return(kproto(df, k)$tot.withinss)
  } else {
    return(kproto(df, k)$cluster)
  }
}

mixed_k_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # numericise gender variable
    df$gender <- as.factor(df$gender == "M")

    # factorise occupation variable
    df$occupation <- as.factor(df$occupation)

  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)
  }
  return(df)
}

mixed_k <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- mixed_k(df, user)

  if (user == TRUE) {
    dist <- distmix(df, method = "ahmad", idnum = 1, idbin = 2, idcat = 3)

  } else {
    dist <- distmix(df, method = "ahmad",
                    idnum = 2:3, idbin = 6:24, idcat = c(1, 4, 5))
  }

  if (obj == TRUE) {
    clust <- fastkmed(dist, k)
    withinss <- 0
    for (i in 1:k) {
      withinss <- withinss + sum(dist[clust$cluster == i, clust$medoid[i]])
    }
    return(withinss)
  } else {
    return(fastkmed(dist, k)$cluster)
  }
}

mskmeans_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # dummy code gender and occupation variable
    df <- dummy_cols(df, select_columns = "gender")
    df$gender <- NULL

    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL

    # normalise categorical variables
    df[2:24] <- df[2:24] / rowSums(df[2:24])

  } else {
    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

    # dummy code title type
    df <- dummy_cols(df, select_columns = "titleType")
    df$titleType <- NULL

    # dummy code director type
    df <- dummy_cols(df, select_columns = "director")
    df$director <- NULL

    # dummy code writer type
    df <- dummy_cols(df, select_columns = "writer")
    df$writer <- NULL

    # normalise categorical variables
    df[3:84] <- df[3:84] / rowSums(df[3:84])
  }
  return(df)
}

mskmeans <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- mskmeans_df(df, user)

  if (user == TRUE) {
    if (obj == TRUE) {
      return(gmsClust(df[1:2], df[3:24], k)$results$tot.withinss)
    } else {
      return(gmsClust(df[1:2], df[3:24], k)$results$cluster)
    }

  } else {
    if (obj == TRUE) {
      return(gmsClust(df[c(1, 2)], df[3:84], k)$results$tot.withinss)
    } else {
      return(gmsClust(df[c(1, 2)], df[3:84], k)$results$cluster)
    }
  }
}

famd_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # factorise categorical variables
    df$gender <- as.factor(df$gender == "M")
    df$occupation <- as.factor(df$occupation)

  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)
  }
  return(df)
}

famd <- function(df, k, user = TRUE, obj = FALSE, var = FALSE, p = 3) {
  # transform data
  df <- famd_df(df, user)

  pca <- FAMD(df, p, graph = FALSE)

  if (var == TRUE) {
    return(pca$eig)
  }

  if (obj == TRUE) {
    return(kmeans(pca$ind$coord, k)$tot.withinss)
  } else {
    return(kmeans(pca$ind$coord, k)$cluster)
  }
}

mrkmeans_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # dummy code gender and occupation variable
    df <- dummy_cols(df, select_columns = "gender")
    df$gender <- NULL

    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL

    for (i in 1:ncol(df)) { # nolint
      df[, i] <- unit_var_normalise(df[, i])
    }

  } else {
    # dummy code title type
    df <- dummy_cols(df, select_columns = "titleType")
    df$titleType <- NULL

    # dummy code director type
    df <- dummy_cols(df, select_columns = "director")
    df$director <- NULL

    # dummy code writer type
    df <- dummy_cols(df, select_columns = "writer")
    df$writer <- NULL

    for (i in 1:ncol(df)) { # nolint
      df[, i] <- unit_var_normalise(df[, i])
    }
  }
  return(df)
}

mrkmeans <- function(df, k, user = TRUE, obj = FALSE, p = 3) {
  # transform data
  df <- mrkmeans_df(df, user)

  if (obj == TRUE) {
    return(cluspca(df, k, p)$criterion)
  } else {
    return(cluspca(df, k, p)$cluster)
  }
}

kamila_df <- function(df, user = TRUE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # factorise categorical variables
    df$gender <- as.factor(df$gender)
    df$occupation <- as.factor(df$occupation)

  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)
  }
  return(df)
}

kamila_clust <- function(df, k, user = TRUE, obj = FALSE) {
  # transform data
  df <- kamila_df(df, user)

  if (user == TRUE) {
    if (obj == TRUE) {
      return(kamila(df[1], df[2:3], k, 10)$finalObj)
    } else{
      return(kamila(df[1], df[2:3], k, 10)$finalMemb)
    }
  } else {
    if (obj == TRUE) {
      return(kamila(df[c(2, 3, 6:24)], df[c(1, 4, 5)], k, 10)$finalObj)
    } else {
      return(kamila(df[c(2, 3, 6:24)], df[c(1, 4, 5)], k, 10)$finalMemb)
    }
  }
}