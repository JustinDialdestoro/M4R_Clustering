library(stringr)

# read 100k item feature data
u100k_feat <- read.csv("M4R_Clustering/Data/u100k_feat.csv")

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
for (i in 1:nrow(u100k_feat)) { # nolint
  print(u100k_feat$title[i])
  film_text <- title_info(u100k_feat$title[i])
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
u100k_nf_id <- data.frame(not_found_mlid, u100k_feat$title[not_found_mlid])
names(u100k_nf_id) <- c("ML filmID", "ML title")

# write movielens not found ids into file
write.csv(u100k_nf_id, file = "M4R_Clustering/Data/u100k_nf_id.csv",
          row.names = FALSE)

# read completed imdb tconst data
u100k_tconst <- read.csv("M4R_Clustering/Data/u100k_tconst.csv")

# read imdb crew data
crew <- read.delim("Data/title.crew.tsv/data.tsv", sep = "\t", header = TRUE)

# create new dataframe for item features
u100k_full_feat <- data.frame(matrix(ncol = 25, nrow = 0))
names(u100k_full_feat) <- c("titleType", "year", "runtime", "adult", "director",
                            "writer", "unknown", "action", "adventure",
                            "animation", "children", "comedy", "crime",
                            "documentary", "drama", "fantasy", "film-noir",
                            "horror", "musical", "mystery", "romance", "sci-fi",
                            "thriller", "war", "western")

# fill rows with data from imdb and ml datasets
for (i in 1:nrow(u100k_feat)) { # nolint
  print(paste("filmID:", i))
  imdb_row <- imdb[imdb$tconst == u100k_tconst[i, ], ]
  crew_row <- crew[crew$tconst == u100k_tconst[i, ], ]
  genre_row <- u100k_feat[u100k_tconst == u100k_tconst[i, ]]
  u100k_full_feat[i, ] <- c(imdb_row$titleType, imdb_row$startYear,
                            imdb_row$runtimeMinutes, imdb_row$isAdult,
                            crew_row$directors, crew_row$writers,
                            genre_row[6:24])
}

# write new item features data into file
write.csv(u100k_full_feat, file = "M4R_Clustering/Data/u100k_full_feat.csv",
          row.names = FALSE)