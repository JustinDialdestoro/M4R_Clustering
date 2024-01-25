library(stringr)

# read 100k rating data
u100k <- read.table("Data/ml-100k/ml-100k/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

# remove entries of film 267
u100knew <- u100k[u100k$filmID != 267, ]
# accordingly adjust filmID's
u100knew[u100knew$filmID > 267, ]$filmID <-
  u100knew[u100knew$filmID > 267, ]$filmID - 1

# write cleaned 100k rating data into file
write.csv(u100knew, file = "M4R_Clustering/Data/u100k.csv")

# read 1m rating data
u1m <- read.csv("Data/ml-1m/ml-1m/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

# column names for 1m dataset
colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

# read 100k user demographic data
udem <- read.table("Data/ml-100k/ml-100k/u.user", sep = "|",
                   col.names = c("userID", "age", "gender",
                                 "occupation", "zip"))

# read item data
ifeat <- read.delim("Data/ml-100k/ml-100k/u.item", sep = "|", header = FALSE,
                    col.names = c("filmID", "title", "date", "null", "imdb",
                                  "unknown", "action", "adventure", "animation",
                                  "children", "comedy", "crime",
                                  "documentary", "drama", "fantasy",
                                  "film-noir", "horror", "musical", "mystery",
                                  "romance", "sci-fi", "thriller", "war",
                                  "western"))

imdb <- read.delim("Data/title.basics.tsv/data.tsv", sep = "\t", header = TRUE)

crew <- read.delim("Data/title.crew.tsv/data.tsv", sep = "\t", header = TRUE)

names <- read.delim("Data/name.basics.tsv/data.tsv", sep = "\t", header = TRUE)

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

for (i in 1:nrow(ifeat)) { # nolint
  print(ifeat$title[i])
  film_text <- title_info(ifeat$title[i])
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

# write imdb ids into file
write.csv(idlist, file = "M4R_Clustering/Data/u100k_tconst.csv")

# write movielens not found ids into file
not_found_mlid <- which(idlist == "Not found")
write.csv(not_found_mlid, file = "M4R_Clustering/Data/u100k_nf_id.csv")

for (id in not_found_mlid) {
  print(paste(id, ifeat$title[id]))
}