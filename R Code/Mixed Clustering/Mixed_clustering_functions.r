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

gow_pam <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {
    # range normalise age
    df$age <- range_normalise(df$age)

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

  if (obj == TRUE) {
    return(pam(dsim, k = k)$objective[2])
  } else {
    return(pam(dsim, k = k)$clustering)
  }
}

hl_pam <- function(df, k, user = TRUE, obj = FALSE) {
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

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  if (obj == TRUE) {
    return(pam(dsim, k = k)$objective[2])
  } else {
    return(pam(dsim, k = k)$clustering)
  }
}

kprototypes <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # binarise gender variable
    df$gender <- as.numeric(df$gender == "M")

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

  if (obj == TRUE) {
    return(kproto(df, k)$tot.withinss)
  } else {
    return(kproto(df, k)$cluster)
  }
}

mixed_k <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # numericise gender variable
    df$gender <- as.numeric(df$gender == "M")

    # factorise occupation variable
    df$occupation <- as.factor(df$occupation)

    dist <- distmix(df, method = "ahmad", idnum = 1, idbin = 2, idcat = 3)

  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

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

mskmeans <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {

    # dummy code gender and occupation variable
    df <- dummy_cols(df, select_columns = "gender")
    df$gender <- NULL

    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL

    if (obj == TRUE) {
      return(gmsClust(df[1:2], df[3:24], k)$results$tot.withinss)
    } else {
      return(gmsClust(df[1:2], df[3:24], k)$results$cluster)
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

    if (obj == TRUE) {
      return(gmsClust(df[c(1, 2)], df[3:84], k)$results$tot.withinss)
    } else {
      return(gmsClust(df[c(1, 2)], df[3:84], k)$results$cluster)
    }
  }
}

famd <- function(df, k, p, user = TRUE, obj = FALSE, var = FALSE) {
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

mrkmeans <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # dummy code gender and occupation variable
    df <- dummy_cols(df, select_columns = "gender")
    df$gender <- NULL

    df <- dummy_cols(df, select_columns = "occupation")
    df$occupation <- NULL

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
  }

  if (obj == TRUE) {
    return(cluspca(df, k, k - 1)$criterion)
  } else {
  return(cluspca(df, k, k - 1)$cluster) # nolint
  }
}

kamila_clust <- function(df, k, user = TRUE, obj = FALSE) {
  if (user == TRUE) {
    # variance normalise age
    df$age <- unit_var_normalise(df$age)

    # factorise categorical variables
    df$gender <- as.factor(df$gender)
    df$occupation <- as.factor(df$occupation)

    if (obj == TRUE) {
      return(kamila(df[1], df[2:3], k, 10)$finalObj)
    } else{
      return(kamila(df[1], df[2:3], k, 10)$finalMemb)
    }
  } else {
    # factorise categorical variables
    df$titleType <- as.factor(df$titleType)
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

    if (obj == TRUE) {
      return(kamila(df[c(2, 3, 6:24)], df[c(1, 4, 5)], k, 10)$finalObj)
    } else {
      return(kamila(df[c(2, 3, 6:24)], df[c(1, 4, 5)], k, 10)$finalMemb)
    }
  }
}