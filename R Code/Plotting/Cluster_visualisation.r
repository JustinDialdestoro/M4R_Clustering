# load packages
library("scales")
library("Rtsne")
library("ggplot2")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Clustering/Rating_clustering.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

# initialise plotting variables
titles <- c("k-Prototypes", "Gower", "Hennig-Liao", "Mixed k-Means",
            "Modha-Spangler k-Means", "FAMD", "Mixed Reduced k-Means", "KAMILA")
colors <- c(hue_pal()(7)[1])
for (i in 2:7) {
  colors <- c(colors, hue_pal()(7)[i])
}

# create user item matrix
ui <- gen_ui_matrix(ml100k, ml100k)

# cluster users
clust_labels <- rating_clust(ui, 6)
# generate TSNE points using appropriate similarity
sim <- gen_euc_sim(ui)
sim[is.na(sim)] <- 0
# TSNE plot
tsne <- Rtsne(sim, check_duplicates = FALSE, partial_pca = TRUE,
              is.distance = TRUE)
print(ggplot(data.frame(tsne$Y), aes(x = X1, y = X2)) +
        geom_point(color = colors[clust_labels]) + xlab("First Dimension") +
        ylab("Second Dimension") + ggtitle("User Rating Clustering TSNE"))

# cluster items
clust_labels <- rating_clust(ui, 5, FALSE)
# generate TSNE points using appropriate similarity
sim <- gen_euc_sim(ui, FALSE)
sim[is.na(sim)] <- 0
# TSNE plot
tsne <- Rtsne(sim, check_duplicates = FALSE, partial_pca = TRUE,
              is.distance = TRUE)
print(ggplot(data.frame(tsne$Y), aes(x = X1, y = X2)) +
        geom_point(color = colors[clust_labels]) + xlab("First Dimension") +
        ylab("Second Dimension") + ggtitle("Item Rating Clustering TSNE"))

# cluster users
clust_labels_u <- rep(c(), 8)
clust_labels_u[[1]] <- kprototypes(ml100k_dem, 5)
clust_labels_u[[2]] <- gow_pam(ml100k_dem, 7)
clust_labels_u[[3]] <- hl_pam(ml100k_dem, 5)
clust_labels_u[[4]] <- mixed_k(ml100k_dem, 4)
clust_labels_u[[5]] <- mskmeans(ml100k_dem, 7)
clust_labels_u[[6]] <- famd(ml100k_dem, 7)
clust_labels_u[[7]] <- mrkmeans(ml100k_dem, 4)
clust_labels_u[[8]] <- kamila_clust(ml100k_dem, 5)

# variance normalise age
ml100k_dem$age <- unit_var_normalise(ml100k_dem$age)
# dummy code gender and occupation variable
ml100k_dem <- dummy_cols(ml100k_dem, select_columns = "gender")
ml100k_dem$gender <- NULL
ml100k_dem <- dummy_cols(ml100k_dem, select_columns = "occupation")
ml100k_dem$occupation <- NULL

# TSNE plots
tsne <- Rtsne(as.matrix(ml100k_dem), check_duplicates = FALSE)
for (i in 1:8) {
  print(ggplot(data.frame(tsne$Y), aes(x = X1, y = X2)) +
          geom_point(color = colors[clust_labels_u[[i]]$clusters],
                     alpha = 0.4) +
          xlab("First Dimension") + ylab("Second Dimension") +
          ggtitle(titles[i]))
}

# cluster items
clust_labels_i <- rep(c(), 8)
clust_labels_i[[1]] <- kprototypes(ml100k_feat_c, 5, FALSE)
clust_labels_i[[2]] <- gow_pam(ml100k_feat_b, 3, FALSE)
clust_labels_i[[3]] <- hl_pam(ml100k_feat_d, 4, FALSE)
clust_labels_i[[4]] <- mixed_k(ml100k_feat_c, 6, FALSE)
clust_labels_i[[5]] <- mskmeans(ml100k_feat_d, 5, FALSE)
clust_labels_i[[6]] <- famd(ml100k_feat_d, 7, FALSE)
clust_labels_i[[7]] <- mrkmeans(ml100k_feat_d, 5, FALSE)
clust_labels_i[[8]] <- kamila_clust(ml100k_feat_b, 5, FALSE)

# variance normalise continuous variables
ml100k_feat_d$year <- unit_var_normalise(ml100k_feat_d$year)
ml100k_feat_d$runtime <- unit_var_normalise(ml100k_feat_d$runtime)

# TSNE plots
tsne <- Rtsne(as.matrix(ml100k_feat_d), check_duplicates = FALSE)
for (i in 1:8) {
  print(ggplot(data.frame(tsne$Y), aes(x = X1, y = X2)) +
          geom_point(color = colors[clust_labels_i[[i]]$clusters],
                     alpha = 0.4) +
          xlab("First Dimension") + ylab("Second Dimension") +
          ggtitle(titles[i]))
}