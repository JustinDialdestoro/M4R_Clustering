library(stringr)

ifeat <- read.delim("M4R_Clustering/Data/u.item", sep = "|", header = FALSE,
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

for (i in 1:1682) {
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

print(which(idlist == "Not found"))