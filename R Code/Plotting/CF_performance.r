# load packages
library("scales")
library("ggplot2")

# read in the data
sim_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_u.csv")
sim_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/sim_i.csv")
pred_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_u.csv")
pred_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_i.csv")

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
labels <- c("Cosine", "Adjusted Cosine", "PCC", "Euclidean", "Manhattan")
limits <- c("cosine", "acosine", "pcc", "euclidean", "manhattan")
colors <- c(hue_pal()(10)[1])
for (i in 2:5) {
  colors <- c(colors, hue_pal()(10)[i])
}

sim_u$k <- rep(krange, 5)
sim_i$k <- rep(krange, 5)

for (s in list(sim_u, sim_i)) {
  print(ggplot(s, aes(x = k, y = rmse, color = metric)) +
          geom_line(linewidth = 0.8) + labs(color = "Similarity") +
          xlab("N Neighbours") + ylab("RMSE") + ggtitle("Average RMSE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = mae, color = metric)) +
          geom_line(linewidth = 0.8) + labs(color = "Similarity") +
          xlab("N Neighbours") + ylab("MAE") + ggtitle("Average MAE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = r2, color = metric)) +
          geom_line(linewidth = 0.8) + labs(color = "Similarity") +
          xlab("N Neighbours") + ylab("R2") + ggtitle("Average R2") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = online, color = metric)) +
          geom_line(linewidth = 0.8) + labs(color = "Similarity") +
          xlab("N Neighbours") + ylab("Time (seconds)") +
          ggtitle("Online Phase") +
          scale_color_manual(labels = labels, limits = limits, values = colors))
}

labels <- c("Mean", "Weighted Mean", "Mean-Centred", "Z-Score", "Discrete")
limits <- c("mean", "weighted sum", "mean centred", "z score", "discrete")
colors <- c(hue_pal()(10)[6])
for (i in 7:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

pred_u$k <- rep(krange, 5)
pred_i$k <- rep(krange, 5)

for (s in list(pred_u, pred_i)) {
  print(ggplot(s, aes(x = k, y = rmse, color = predictor)) +
          geom_line(linewidth = 0.8) + labs(color = "Predictor") +
          xlab("N Neighbours") + ylab("RMSE") + ggtitle("Average RMSE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = mae, color = predictor)) +
          geom_line(linewidth = 0.8) + labs(color = "Predictor") +
          xlab("N Neighbours") + ylab("MAE") + ggtitle("Average MAE") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = r2, color = predictor)) +
          geom_line(linewidth = 0.8) + labs(color = "Predictor") +
          xlab("N Neighbours") + ylab("R2") + ggtitle("Average R2") +
          scale_color_manual(labels = labels, limits = limits, values = colors))

  print(ggplot(s, aes(x = k, y = online, color = predictor)) +
          geom_line(linewidth = 0.8) + labs(color = "Predictor") +
          xlab("N Neighbours") + ylab("Time (seconds)") +
          ggtitle("Online Phase") +
          scale_color_manual(labels = labels, limits = limits, values = colors))
}