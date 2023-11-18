data <- read.table("M4R_Clustering/Data/u.data",
                   col.names = c("userID", "filmID", "rating", "timestamp"))

data2 <- read.csv("M4R_Clustering/Data/ratings.dat", sep=":", 
                  colClasses = c(NA, "NULL"), header=FALSE)
colnames(data2) <- c("userID", "filmID", "rating", "timestamp")

t_fold_index <- function(df, t) {
  # empty vector to contain each fold
  fold_ind <- replicate(t, c())
  # iterate through each unique user id to partition each users rating evenly
  for (i in unique(df$userID)) {
    # find the indices of user i's ratings and permute them
    i_perm <- sample(which(df$userID == i))

    # number of ratings in each fold
    k <- length(i_perm) %/% t

    # add the partitioned indices into each fold
    for (j in 1:t) {
      fold_ind[[j]] <- c(fold_ind[[j]], i_perm[((j - 1) * k + 1):(j * k)])
    }

    # randomly assign remaining indices (from division) to a fold
    f <- sample(1:t, 1)
    fold_ind[[f]] <- c(fold_ind[[f]], i_perm[(t * k + 1):length(i_perm)])

  }
  return(fold_ind)
}

t_fold <- function(df, indexes) {
  # number of folds
  t <- length(indexes)

  # list to contain each training set dataframe
  folds <- replicate(t, c())

  for (i in 1:t) {
    folds[[i]] <- df[-indexes[i], ]
  }
  return(folds)
}