# load packages
library("scales")
library("ggplot2")
library("ggpubr")

# read in the data
pred_u <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_u.csv")
pred_i <- read.csv("M4R_Clustering/Results/Collaborative Filtering/pred_i.csv")
clust_u <- read.csv("M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv")
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

clust_u_mae <- ggplot(full_u, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

clust_u_r2 <- ggplot(full_u, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

clust_u_online <- ggplot(full_u, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

clust_i_mae <- ggplot(full_i, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

clust_i_r2 <- ggplot(full_i, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

clust_i_online <- ggplot(full_i, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, values = colors)

# dimensions 15 x 8
print(ggarrange(clust_u_mae, clust_u_r2, clust_u_online,
                clust_i_mae, clust_i_r2, clust_i_online,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
                font.label = c(size = 15)))