# read 100k item full feature data
ml100k_full_feat <- read.csv("M4R_Clustering/Data/ml100k_full_feat.csv")

# remove adult variable
ml100k_full_feat$adult <- NULL

# indices of items with multiple directors and no director
ind_dir <- which(nchar(ml100k_full_feat$director) > 9)
ind_na <- which(ml100k_full_feat$director == "\\N")

# list of directors credited as sole director
directors <- ml100k_full_feat$director[-c(ind_dir, ind_na)]

# add all directors credited as a co-director
for (i in ind_dir) {
  directors <- c(directors, strsplit(ml100k_full_feat$director[i], ",")[[1]])
}

# find the most common directors
top_directors <- names(table(directors)[table(directors) > 4])

for (i in 1:nrow(ml100k_full_feat)) { # nolint
  d <- ml100k_full_feat$director[i]

  # check if single director
  if (nchar(d) == 9) {
    # check if a top director
    if (d %in% top_directors) {
      # leave as is
    } else {
      # remove director entry
      ml100k_full_feat$director[i] <- "\\N"
    }

    # check if multiple directors
  } else if (nchar(d) > 9) {
    # split into separate directors
    d_list <- strsplit(d, ",")[[1]]
    # check if any are a top director
    if (any(d_list %in% top_directors)) {
      # update director to the most prolific director
      ml100k_full_feat$director[i] <-
        top_directors[min(which(top_directors %in% d_list))]
    } else {
      # remove director entry
      ml100k_full_feat$director[i] <- "\\N"
    }
  }
}

# indices of items with multiple writers and no writer
ind_wri <- which(nchar(ml100k_full_feat$writer) > 9)
ind_na <- which(ml100k_full_feat$writer == "\\N")

# list of writers credited as sole writer
writers <- ml100k_full_feat$writer[-c(ind_wri, ind_na)]

# add all writers credited as a co-writer
for (i in ind_wri) {
  writers <- c(writers, strsplit(ml100k_full_feat$writer[i], ",")[[1]])
}

# find the most common directors
top_writers <- names(table(writers)[table(writers) > 5])

for (i in 1:nrow(ml100k_full_feat)) { # nolint
  w <- ml100k_full_feat$writer[i]

  # check if single writer
  if (nchar(w) == 9) {
    # check if a top writer
    if (w %in% top_writers) {
      # leave as is
    } else {
      # remove writer entry
      ml100k_full_feat$writer[i] <- "\\N"
    }

    # check if multiple writers
  } else if (nchar(w) > 9) {
    # split into separate writers
    w_list <- strsplit(w, ",")[[1]]
    # check if any are a top writer
    if (any(w_list %in% top_writers)) {
      # update writer to the most prolific writer
      ml100k_full_feat$writer[i] <-
        top_writers[min(which(top_writers %in% w_list))]
    } else {
      # remove writer entry
      ml100k_full_feat$writer[i] <- "\\N"
    }
  }
}

# impute missing values in runtime
ind_na <- which(ml100k_full_feat$runtime == "\\N")
ml100k_full_feat$runtime[ind_na] <-
  mean(as.numeric(ml100k_full_feat$runtime[-ind_na]))

# write new item features data into file
write.csv(ml100k_full_feat, file = "M4R_Clustering/Data/ml100k_feat_a.csv",
          row.names = FALSE)