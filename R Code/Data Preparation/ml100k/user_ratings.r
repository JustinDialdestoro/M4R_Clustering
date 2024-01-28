# read 100k rating data
u100k <- read.table("Data/ml-100k/ml-100k/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

# read 100k item feature data
u100k_feat <- read.delim("Data/ml-100k/ml-100k/u.item", sep = "|",
                         header = FALSE,
                         col.names = c("filmID", "title", "date", "null",
                                       "imdb", "unknown", "action", "adventure",
                                       "animation", "children", "comedy",
                                       "crime", "documentary", "drama",
                                       "fantasy", "film-noir", "horror",
                                       "musical", "mystery", "romance",
                                       "sci-fi", "thriller", "war", "western"))

# skeleton dataframe to be cleaned
u100knew <- u100k

# find repeated titles
repeats <- u100k_feat$title[duplicated(u100k_feat$title)]
r <- length(repeats)

# find indices of repeated films
repeats_ind <- matrix(NA, r, 2)

for (i in 1:r) { # nolint
  # indexes of repeated films
  repeats_ind[i, ] <- which(u100k_feat$title == repeats[i])
  # change filmID of to the first one to remove repeats
  u100knew$filmID[u100k$filmID == repeats_ind[i, 2]] <- repeats_ind[i, 1]
}

for (i in 1:(r - 1)) {
  # reorder filmID so there are no gaps
  u100knew$filmID[u100knew$filmID > repeats_ind[i, 2] &
                    u100knew$filmID < repeats_ind[i + 1, 2]] <-
    u100knew$filmID[u100knew$filmID > repeats_ind[i, 2] &
                    u100knew$filmID < repeats_ind[i + 1, 2]] - i
}
u100knew$filmID[u100knew$filmID > repeats_ind[r, 2]] <-
  u100knew$filmID[u100knew$filmID > repeats_ind[r, 2]] - r

# remove entries of film 267
u100knew <- u100knew[u100knew$filmID != 267, ]
# accordingly adjust filmID's
u100knew$filmID[u100knew$filmID > 267] <-
  u100knew$filmID[u100knew$filmID > 267] - 1

# write cleaned 100k rating data into file
write.csv(u100knew, file = "M4R_Clustering/Data/u100k.csv", row.names = FALSE)