# read 100k item full feature data
u100k_full_feat <- read.csv("M4R_Clustering/Data/u100k_full_feat.csv")

# remove adult variable
u100k_full_feat$adult <- NULL

# indices of items with multiple directors and no director
ind_dir <- which(nchar(u100k_full_feat$director) > 9)
ind_na <- which(u100k_full_feat$director == "\\N")

# list of directors credited as sole director
directors <- u100k_full_feat$director[-c(ind_dir, ind_na)]

# add all directors credited as a co-director
for (i in ind_dir) {
  directors <- c(directors, strsplit(u100k_full_feat$director[i], ",")[[1]])
}

# find the most common directors
top_directors <- names(table(directors)[table(directors) > 4])

for (i in 1:nrow(u100k_full_feat)) { # nolint
  d <- u100k_full_feat$director[i]

  # check if single director
  if (nchar(d) == 9) {
    # check if a top director
    if (d %in% top_directors) {
      # leave as is
    } else {
      # remove director entry
      u100k_full_feat$director[i] <- "\\N"
    }

    # check if multiple directors
  } else if (nchar(d) > 9) {
    # split into separate directors
    d_list <- strsplit(d, ",")[[1]]
    # check if any are a top director
    if (any(d_list %in% top_directors)) {
      # update director to the most prolific director
      u100k_full_feat$director[i] <-
        top_directors[min(which(top_directors %in% d_list))]
    } else {
      # remove director entry
      u100k_full_feat$director[i] <- "\\N"
    }
  }
}

# indices of items with multiple writers and no writer
ind_wri <- which(nchar(u100k_full_feat$writer) > 9)
ind_na <- which(u100k_full_feat$writer == "\\N")

# list of writers credited as sole writer
writers <- u100k_full_feat$writer[-c(ind_wri, ind_na)]

# add all writers credited as a co-writer
for (i in ind_wri) {
  writers <- c(writers, strsplit(u100k_full_feat$writer[i], ",")[[1]])
}

# find the most common directors
top_writers <- names(table(writers)[table(writers) > 5])

for (i in 1:nrow(u100k_full_feat)) { # nolint
  w <- u100k_full_feat$writer[i]

  # check if single writer
  if (nchar(w) == 9) {
    # check if a top writer
    if (w %in% top_writers) {
      # leave as is
    } else {
      # remove writer entry
      u100k_full_feat$writer[i] <- "\\N"
    }

    # check if multiple writers
  } else if (nchar(w) > 9) {
    # split into separate writers
    w_list <- strsplit(w, ",")[[1]]
    # check if any are a top writer
    if (any(w_list %in% top_writers)) {
      # update writer to the most prolific writer
      u100k_full_feat$writer[i] <-
        top_writers[min(which(top_writers %in% w_list))]
    } else {
      # remove writer entry
      u100k_full_feat$writer[i] <- "\\N"
    }
  }
}

range_normalise <- function(x) {
  # normalise variable to a [0,1] range
  return((x - min(x)) / (max(x) - min(x)))
}

# range normalise year
u100k_full_feat$year <- range_normalise(as.numeric(u100k_full_feat$year))

# indices of items with no runtime
ind_na <- which(u100k_full_feat$runtime == "\\N")

# range normalise run time
u100k_full_feat$runtime[-ind_na] <-
  range_normalise(as.numeric(u100k_full_feat$runtime[-ind_na]))

# fill in missing values
u100k_full_feat$runtime[ind_na] <- 0

# write new item features data into file
write.csv(u100k_full_feat, file = "M4R_Clustering/Data/u100k_feat_a.csv",
          row.names = FALSE)