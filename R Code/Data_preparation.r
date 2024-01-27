library(stringr)

# clean ml 100k rating data ----------------------------------------------------

# read 100k rating data
u100k <- read.table("Data/ml-100k/ml-100k/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

# read 100k item feature data
ifeat <- read.delim("Data/ml-100k/ml-100k/u.item", sep = "|", header = FALSE,
                    col.names = c("filmID", "title", "date", "null", "imdb",
                                  "unknown", "action", "adventure", "animation",
                                  "children", "comedy", "crime",
                                  "documentary", "drama", "fantasy",
                                  "film-noir", "horror", "musical", "mystery",
                                  "romance", "sci-fi", "thriller", "war",
                                  "western"))

# skeleton dataframe to be cleaned
u100knew <- u100k

# find repeated titles
repeats <- ifeat$title[duplicated(ifeat$title)]
r <- length(repeats)

# find indices of repeated films
repeats_ind <- matrix(NA, r, 2)

for (i in 1:r) { # nolint
  # indexes of repeated films
  repeats_ind[i, ] <- which(ifeat$title == repeats[i])
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

# clean ml 100k user demographic data ------------------------------------------

# read 100k user demographic data
udem <- read.table("Data/ml-100k/ml-100k/u.user", sep = "|",
                   col.names = c("userID", "age", "gender",
                                 "occupation", "zip"))

udem$userID <- NULL

# write cleaned 100k user demographic data into file
write.csv(udem, file = "M4R_Clustering/Data/u100k_dem.csv", row.names = FALSE)

# clean ml 100k item feature data ----------------------------------------------

# remove repeated rows
ifeatnew <- ifeat[-repeats_ind[, 2], ]

# remove entries of film 267
ifeatnew <- ifeatnew[-c(267), ]

# reorder film ID's
ifeatnew$filmID <- 1:nrow(ifeatnew) # nolint

# write cleaned 100k iteam feature data into file
write.csv(ifeatnew, file = "M4R_Clustering/Data/u100k_feat.csv",
          row.names = FALSE)

# make new data set of item features -------------------------------------------

# read imdb title data
imdb <- read.delim("Data/title.basics.tsv/data.tsv", sep = "\t", header = TRUE)

# function to decompose movielens titles
title_info <- function(title) {
  # remove all parentheses text
  film_title <- str_remove_all(title, "\\s*\\([^\\)]+\\)")
  # move ", The" string if present
  if (grepl(", The", film_title)) {
    film_title <- paste("The", str_remove(film_title, ", The"), sep = " ")
  }

  if (grepl(", A", film_title)) {
    film_title <- paste("A", str_remove(film_title, ", A"), sep = " ")
  }

  # get all parenthesised text
  extra_text <- str_extract_all(title, "(?<=\\()\\w+(?=\\))")[[1]]

  if (length(extra_text) == 2) {
    return(c(film_title, extra_text[1], extra_text[2]))
  } else if (length(extra_text) == 1) {
    return(c(film_title, "", extra_text))
  } else {
    return(c(film_title, "", ""))
  }
}

idlist <- c()

# find tconst in imdb data for each 100k movielens film
for (i in 1:nrow(ifeatnew)) { # nolint
  print(ifeatnew$title[i])
  film_text <- title_info(ifeatnew$title[i])
  found_rows <- imdb[imdb$primaryTitle == film_text[1] &
                       imdb$startYear == film_text[3] &
                       imdb$titleType == "movie", ]

  if (nrow(found_rows) != 1) {
    year <- as.numeric(film_text[3])
    film_text[1] <- str_remove(film_text[1], ":")
    film_text[2] <- str_remove(film_text[2], ":")

    found_rows <- imdb[(imdb$primaryTitle == film_text[1] |
                          imdb$primaryTitle == film_text[2] |
                          imdb$originalTitle == film_text[1] |
                          imdb$originalTitle == film_text[2]) &
                         (imdb$startYear == as.character(year + 1) |
                            imdb$startYear == film_text[3] |
                            imdb$startYear == as.character(year - 1)) &
                         imdb$titleType == "movie", ]
  }

  if (nrow(found_rows) == 1) {
    idlist <- c(idlist, found_rows$tconst)
  } else {
    idlist <- c(idlist, "Not found")
  }
}

# write currently found imdb tconsts into file
write.csv(idlist, file = "M4R_Clustering/Data/u100k_tconst_missing.csv")

# construct new data frame of not found movielens films
not_found_mlid <- which(idlist == "Not found")
u100k_nf_id <- data.frame(not_found_mlid, ifeat$title[not_found_mlid])
names(u100k_nf_id) <- c("ML filmID", "ML title")

# write movielens not found ids into file
write.csv(u100k_nf_id, file = "M4R_Clustering/Data/u100k_nf_id.csv")

crew <- read.delim("Data/title.crew.tsv/data.tsv", sep = "\t", header = TRUE)

names <- read.delim("Data/name.basics.tsv/data.tsv", sep = "\t", header = TRUE)

# clean ml 1m rating data ------------------------------------------------------

# read 1m rating data
u1m <- read.csv("Data/ml-1m/ml-1m/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

# column names for 1m dataset
colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")