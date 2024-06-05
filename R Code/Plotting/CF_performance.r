# load packages
library("scales")
library("ggplot2")
library("ggpubr")
library("extrafont")

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
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("MAE of User-based Similarities") +
  theme(legend.text = element_text(size = 25))

sim_u_r2 <- ggplot(sim_u, aes(x = k, y = r2, color = metric)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle(expression(paste(R^2, " of User-based Similarities"))) +
  theme(legend.text = element_text(size = 25))

sim_u_online <- ggplot(sim_u, aes(x = k, y = online, color = metric)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  theme(legend.text = element_text(size = 25))

sim_i_mae <- ggplot(sim_i, aes(x = k, y = mae, color = metric)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("MAE of Item-based Similarities") +
  theme(legend.text = element_text(size = 25))

sim_i_r2 <- ggplot(sim_i, aes(x = k, y = r2, color = metric)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle(expression(paste(R^2, " of Item-based Similarities"))) +
  theme(legend.text = element_text(size = 25))

sim_i_online <- ggplot(sim_i, aes(x = k, y = online, color = metric)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  theme(legend.text = element_text(size = 25))

# dimensions 13.3 x 8
# print(ggarrange(sim_u_mae, sim_u_r2, sim_u_online,
#                 sim_i_mae, sim_i_r2, sim_i_online,
#                 common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3))

print(ggarrange(sim_u_mae, sim_u_r2, sim_i_mae, sim_i_r2,
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 2))

pred_u$k <- rep(krange, 5)
pred_i$k <- rep(krange, 5)

labels <- c("Mean", "Weighted Mean", "Mean-Centred", "Z-Score", "Discrete")
limits <- c("mean", "weighted sum", "mean centred", "z score", "discrete")
colors <- c(hue_pal()(10)[6])
for (i in 7:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

pred_u_mae <- ggplot(pred_u, aes(x = k, y = mae, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("MAE of User-based Predictors") +
  theme(legend.text = element_text(size = 25))

pred_u_r2 <- ggplot(pred_u, aes(x = k, y = r2, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle(expression(paste(R^2, " of User-based Predictors"))) +
  theme(legend.text = element_text(size = 25))

pred_u_online <- ggplot(pred_u, aes(x = k, y = online, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("Online phase of User-based Predictors") +
  theme(legend.text = element_text(size = 25))

pred_i_mae <- ggplot(pred_i, aes(x = k, y = mae, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("MAE of Item-based Predictors") +
  theme(legend.text = element_text(size = 25))

pred_i_r2 <- ggplot(pred_i, aes(x = k, y = r2, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle(expression(paste(R^2, " of Item-based Predictors"))) +
  theme(legend.text = element_text(size = 25))

pred_i_online <- ggplot(pred_i, aes(x = k, y = online, color = predictor)) +
  geom_line(linewidth = 1) + labs(color = "") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 20) +
  scale_color_manual(labels = labels, limits = limits, values = colors) +
  theme(text = element_text(family = "LM Roman 10")) +
  ggtitle("Online phase of Item-based Predictors") +
  theme(legend.text = element_text(size = 25))

# dimensions 20 x 10
print(ggarrange(pred_u_mae, pred_u_r2, pred_u_online,
                pred_i_mae, pred_i_r2, pred_i_online,
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3))