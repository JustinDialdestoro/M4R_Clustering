# read 100k item full feature data
u100k_feat_full <- read.csv("M4R_Clustering/Data/u100k_full_feat.csv")

# remove adult variable
u100k_feat_full$adult <- NULL

# indices of items with multiple directors and no director
ind_dir <- which(nchar(u100k_feat_full$director) > 9)
ind_na <- which(u100k_feat_full$director == "\\N")

# list of directors credited as sole director
directors <- u100k_feat_full$director[-c(ind_dir, ind_na)]

# add all directors credited as a co-director
for (i in ind_dir) {
  directors <- c(directors, strsplit(u100k_feat_full$director[i], ",")[[1]])
}

# find the most common directors
top_directors <- names(table(directors)[table(directors) > 4])

for (i in 1:nrow(u100k_feat_full)) { # nolint
  d <- u100k_feat_full$director[i]

  # check if single director
  if (nchar(d) == 9) {
    # check if a top director
    if (d %in% top_directors) {
      # leave as is
    } else {
      # remove director entry
      u100k_feat_full$director[i] <- "\\N"
    }

    # check if multiple directors
  } else if (nchar(d) > 9) {
    # split into separate directors
    d_list <- strsplit(d, ",")[[1]]
    # check if any are a top director
    if (any(d_list %in% top_directors)) {
      # update director to the most prolific director
      u100k_feat_full$director[i] <-
        top_directors[min(which(top_directors %in% d_list))]
    } else {
      # remove director entry
      u100k_feat_full$director[i] <- "\\N"
    }
  }
}

# indices of items with multiple writers and no writer
ind_wri <- which(nchar(u100k_feat_full$writer) > 9)
ind_na <- which(u100k_feat_full$writer == "\\N")

# list of writers credited as sole writer
writers <- u100k_feat_full$writer[-c(ind_wri, ind_na)]

# add all writers credited as a co-writer
for (i in ind_wri) {
  writers <- c(writers, strsplit(u100k_feat_full$writer[i], ",")[[1]])
}

# find the most common directors
top_writers <- names(table(writers)[table(writers) > 5])

for (i in 1:nrow(u100k_feat_full)) { # nolint
  w <- u100k_feat_full$writer[i]

  # check if single writer
  if (nchar(w) == 9) {
    # check if a top writer
    if (w %in% top_writers) {
      # leave as is
    } else {
      # remove writer entry
      u100k_feat_full$writer[i] <- "\\N"
    }

    # check if multiple writers
  } else if (nchar(w) > 9) {
    # split into separate writers
    w_list <- strsplit(w, ",")[[1]]
    # check if any are a top writer
    if (any(w_list %in% top_writers)) {
      # update writer to the most prolific writer
      u100k_feat_full$writer[i] <-
        top_writers[min(which(top_writers %in% w_list))]
    } else {
      # remove writer entry
      u100k_feat_full$writer[i] <- "\\N"
    }
  }
}