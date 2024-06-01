# load colour package
library("scales")
library("ggplot2")

# read in the data
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_split_u.csv"
fclust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fclust_split_i.csv"
fclust_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_u.csv"
pred_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
# loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv"
# clust_u <- read.csv(loc)
# loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv"
# clust_i <- read.csv(loc)

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
labels <- c("None", "k-Prototypes", "Gower", "Hennig-Liao",
            "Mixed k-Means", "Modha-Spangler k-Means", "FAMD",
            "Mixed Reduced k-Means", "KAMILA")
limits <- c("none", "kproto", "gow", "hl", "mk", "msk", "famd",
            "mrk", "kam")
colors <- c(hue_pal()(10)[1])
for (i in 3:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

# prepare results
noclust_u <- pred_u[61:90, ]
names(noclust_u)[1] <- "clustering"
noclust_u[1] <- "none"
# clust_u <- cbind(clustering = "ratings", clust_u)
names(fclust_u)[1] <- "clustering"
full_u <- rbind(noclust_u, fclust_u)
full_u$k <- rep(krange, 9)

# prepare results
noclust_i <- pred_i[61:90, ]
names(noclust_i)[1] <- "clustering"
noclust_i[1] <- "none"
# clust_i <- cbind(clustering = "ratings", clust_i)
names(fclust_i)[1] <- "clustering"
full_i <- rbind(noclust_i, fclust_i)
full_i$k <- rep(krange, 9)

for (f in list(full_u, full_i)) {
  print(ggplot(f, aes(x = k, y = rmse, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "Clustering Method") +
          xlab("N Neighbours") + ylab("RMSE") + ggtitle("Average RMSE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(f, aes(x = k, y = mae, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "Clustering Method") +
          xlab("N Neighbours") + ylab("MAE") + ggtitle("Average MAE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(f, aes(x = k, y = r2, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "Clustering Method") +
          xlab("N Neighbours") + ylab("R2") + ggtitle("Average R2") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(f, aes(x = k, y = online, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "Clustering Method") +
          xlab("N Neighbours") + ylab("Time (seconds)") +
          ggtitle("Online Phase") +
          scale_color_manual(labels = labels, limits = limits, values = colors))
}