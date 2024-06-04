# load packages
library("scales")
library("ggplot2")
library("ggpubr")

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

sim_u_mae <- ggplot(sim_u, aes(x = k, y = mae, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

sim_u_r2 <- ggplot(sim_u, aes(x = k, y = r2, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

sim_u_online <- ggplot(sim_u, aes(x = k, y = online, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

sim_i_mae <- ggplot(sim_i, aes(x = k, y = mae, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

sim_i_r2 <- ggplot(sim_i, aes(x = k, y = r2, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

sim_i_online <- ggplot(sim_i, aes(x = k, y = online, color = metric)) +
  geom_line(linewidth = 0.8) + labs(color = "Similarity:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

# dimensions 15 x 8
print(ggarrange(sim_u_mae, sim_u_r2, sim_u_online,
                sim_i_mae, sim_i_r2, sim_i_online,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
                font.label = c(size = 15)))

pred_u$k <- rep(krange, 5)
pred_i$k <- rep(krange, 5)

labels <- c("Mean", "Weighted Mean", "Mean-Centred", "Z-Score", "Discrete")
limits <- c("mean", "weighted sum", "mean centred", "z score", "discrete")
colors <- c(hue_pal()(10)[6])
for (i in 7:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

pred_u_mae <- ggplot(pred_u, aes(x = k, y = mae, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

pred_u_r2 <- ggplot(pred_u, aes(x = k, y = r2, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

pred_u_online <- ggplot(pred_u, aes(x = k, y = online, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

pred_i_mae <- ggplot(pred_i, aes(x = k, y = mae, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

pred_i_r2 <- ggplot(pred_i, aes(x = k, y = r2, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

pred_i_online <- ggplot(pred_i, aes(x = k, y = online, color = predictor)) +
  geom_line(linewidth = 0.8) + labs(color = "Predictor:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

# dimensions 15 x 8
print(ggarrange(pred_u_mae, pred_u_r2, pred_u_online,
                pred_i_mae, pred_i_r2, pred_i_online,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
                font.label = c(size = 15)))