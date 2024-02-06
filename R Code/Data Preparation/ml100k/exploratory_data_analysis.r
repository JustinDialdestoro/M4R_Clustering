# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# load CF functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")

ui <- gen_ui_matrix(ml100k, ml100k)

# average user ratings
u_mean <- rowMeans(ui, na.rm = TRUE)

hist(u_mean, breaks = 20)