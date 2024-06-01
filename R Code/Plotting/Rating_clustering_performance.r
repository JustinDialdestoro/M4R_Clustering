# load packages
library("scales")
library("ggplot2")

# read in the data
pred_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_u.csv")
clust_u <- read.csv("M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv")
pred_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_i.csv")
clust_i <- read.csv("M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv")

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
labels <- c("Standard CF", "k-Means Clustering")
colors <- c(hue_pal()(10)[1], hue_pal()(10)[2])

# prepare results
noclust_u <- pred_u[pred_u$predictor == "mean centred", ][2:6]
full_u <- rbind(noclust_u, clust_u)
full_u$clustering <- c(rep("none", 30), rep("ratings", 30))
full_u$k <- c(krange, krange)

# prepare results
noclust_i <- pred_i[pred_i$predictor == "mean centred", ][2:6]
full_i <- rbind(noclust_i, clust_i)
full_i$clustering <- c(rep("none", 30), rep("ratings", 30))
full_i$k <- c(krange, krange)

for (f in list(full_u, full_i)) {
  print(ggplot(f, aes(x = k, y = rmse, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "") +
          xlab("N Neighbours") + ylab("RMSE") + ggtitle("Average RMSE") +
          scale_color_manual(labels = labels, values = colors))

  print(ggplot(f, aes(x = k, y = mae, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "") +
          xlab("N Neighbours") + ylab("MAE") + ggtitle("Average MAE") +
          scale_color_manual(labels = labels, values = colors))

  print(ggplot(f, aes(x = k, y = r2, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "") +
          xlab("N Neighbours") + ylab("R2") + ggtitle("Average R2") +
          scale_color_manual(labels = labels, values = colors))

  print(ggplot(f, aes(x = k, y = online, color = clustering)) +
          geom_line(linewidth = 0.8) + labs(color = "") +
          xlab("N Neighbours") + ylab("Time (seconds)") +
          ggtitle("Online Phase") +
          scale_color_manual(labels = labels, values = colors))
}