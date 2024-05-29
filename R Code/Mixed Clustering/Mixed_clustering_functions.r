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
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
    df[5:23] <- lapply(df[5:23], as.factor)

  }
  return(df)
}

# dataset B
gow_pam <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- gow_df(df, user)

  # gower dissimilarity matrix
  dsim <- daisy(df, metric = "gower")

  out <- pam(dsim, k = k)

  return(list(full = out, clusters = out$clustering, loss = out$objective[2],
              centroids = df[out$medoids, ]))
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
    df[3:21] <- df[3:21] * genre_fac

  }
  return(df)
}

# dataset D
hl_pam <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- hl_df(df, user)

  # euclidean dissimilarity matrix
  dsim <- daisy(df, metric = "euclidean")

  out <- pam(dsim, k = k)

  return(list(full = out, clusters = out$clustering, loss = out$objective[2],
              centroids = df[out$medoids, ]))
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
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
    df$genre <- as.factor(df$genre)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

  }
  return(df)
}

# dataset B
kprototypes <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- kproto_df(df, user)
  lambda <- lambdaest(df)

  out <- kproto(df, k, lambda)
  return(list(full = out, clusters = out$cluster, loss = out$tot.withinss,
              centroids = out$centers))
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
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
    df$genre <- as.factor(df$genre)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

  }
  return(df)
}

# dataset C
mixed_k <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- mixed_k_df(df, user)

  if (user == TRUE) {
    dist <- distmix(df, method = "ahmad", idnum = 1, idbin = 2, idcat = 3)

  } else {
    dist <- distmix(df, method = "ahmad", idnum = 1:2, idcat = 3:5)
  }

  out <- fastkmed(dist, k)
  withinss <- 0
  for (i in 1:k) {
    withinss <- withinss + sum(dist[out$cluster == i, out$medoid[i]])
  }

  return(list(full = out, clusters = out$cluster, loss = withinss,
              centroids = df[out$medoid, ]))
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

    # normalise categorical variables
    df[3:24] <- df[3:21] / rowSums(df[3:21])
  }
  return(df)
}

# dataset D
mskmeans <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- mskmeans_df(df, user)

  if (user == TRUE) {
    out <- gmsClust(df[1:2], df[3:24], k)
  } else {
    out <- gmsClust(df[1:2], df[3:21], k)
  }
  results <- out$results

  return(list(full = out, clusters = results$cluster,
              loss = results$tot.withinss,
              centroids = cbind(results$conCenters, results$catCenters)))
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
    df[3:21] <- lapply(df[3:21], as.factor)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)
  }
  return(df)
}

# dataset D
famd <- function(df, k, user = TRUE, p = k - 1) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- famd_df(df, user)
  # conduct PCA step
  pca <- FAMD(df, p, graph = FALSE)

  out <- kmeans(pca$ind$coord, k)

  return(list(full = out, clusters = out$cluster, loss = out$tot.withinss,
              centroids = out$centers, variance = pca$eig))
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
    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

    # variance normalise categorical variables
    for (i in 3:21) { # nolint
      df[, i] <- (df[, i] - mean(df[, i])) / sqrt(sum(df[, i]))
    }
  }
  return(df)
}

# dataset D
mrkmeans <- function(df, k, user = TRUE, p = k - 1) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- mrkmeans_df(df, user)

  out <- cluspca(df, k, p)

  return(list(full = out, clusters = out$cluster, loss = out$criterion,
              centroids = out$centroid))
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
    df$director <- as.factor(df$director)
    df$writer <- as.factor(df$writer)
    df[5:23] <- lapply(df[5:23], as.factor)

    # variance normalise continuous variables
    df$year <- unit_var_normalise(df$year)
    df$runtime <- unit_var_normalise(df$runtime)

  }
  return(df)
}

# dataset B
kamila_clust <- function(df, k, user = TRUE) {
  # set seed
  set.seed(01848521)

  # transform data
  df <- kamila_df(df, user)

  if (user == TRUE) {
    out <- kamila(df[1], df[2:3], k, 10)
  } else {
    out <- kamila(df[c(1, 2)], df[c(3:23)], k, 10)
  }

  return(list(clusters = out$finalMemb, loss = out$finalObj,
              centroids = out$finalCenters))
}