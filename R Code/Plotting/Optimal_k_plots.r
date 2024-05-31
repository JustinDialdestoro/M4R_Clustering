# load packages
library("scales")
library("ggplot2")

# read in the data
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_u.csv"
clust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_i.csv"
clust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_u.csv"
mclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_i.csv"
mclust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_u.csv"
fclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_obj_i.csv"
fclust_obj_i <- read.csv(loc)

# initialise plotting variables
nrange <- 2:15
colors <- c(hue_pal()(10)[1])
for (i in 2:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

clust_obj_u <- data.frame(n = nrange, clust_obj_u)
clust_obj_i <- data.frame(n = nrange, clust_obj_i)

print(ggplot(clust_obj_u, aes(x = n, y = x)) +
        geom_line(color = colors[2]) +
        geom_point(color = colors[2]) +
        theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
        ggtitle("User Cluster Elbow"))

print(ggplot(clust_obj_i, aes(x = n, y = x)) +
        geom_line(color = colors[2]) +
        geom_point(color = colors[2]) +
        theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
        ggtitle("Item Cluster Elbow"))

mclust_obj_u$n <- nrange
mclust_obj_i$n <- nrange

labels <- c("kproto", "gow", "hl", "mk", "msk", "famd", "mrk", "kam")
titles <- c("k-Prototypes", "Gower", "Hennig-Liao", "Mixed k-Means",
            "Modha-Spangler k-Means", "FAMD", "Mixed Reduced k-Means", "KAMILA")
obj <- c("Loss", "Loss", "Loss", "Loss", "Loss", "WCSS", "WCSS",
         "Log-Likelihood")

for (i in 1:8) {
  print(ggplot(mclust_obj_u, aes(x = n, y = .data[[labels[i]]])) +
          geom_line(color = colors[i + 2]) +
          geom_point(color = colors[i + 2]) +
          theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[i]) +
          ggtitle(titles[i]))
}

for (i in 1:8) {
  print(ggplot(mclust_obj_i, aes(x = n, y = .data[[labels[i]]])) +
          geom_line(color = colors[i + 2]) +
          geom_point(color = colors[i + 2]) +
          theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[i]) +
          ggtitle(titles[i]))
}

fclust_obj_u$n <- nrange
fclust_obj_i$n <- nrange

for (i in 1:7) {
  print(ggplot(fclust_obj_u, aes(x = n, y = .data[[labels[i]]])) +
          geom_line(color = colors[i + 2]) +
          geom_point(color = colors[i + 2]) +
          theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[i]) +
          ggtitle(titles[i]))
}

for (i in 1:7) {
  print(ggplot(fclust_obj_i, aes(x = n, y = .data[[labels[i]]])) +
          geom_line(color = colors[i + 2]) +
          geom_point(color = colors[i + 2]) +
          theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[i]) +
          ggtitle(titles[i]))
}