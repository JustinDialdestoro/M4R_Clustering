# load colour package
library("scales")
library("ggplot2")
library("ggpubr")
library("extrafont")
library("reshape2")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_c <- read.csv("M4R_Clustering/Data/ml100k_feat_c.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Mixed Clustering/Mixed_clustering_functions.r")

# initialise plotting variables
range <- 1:100

# generate ui matrix
ui <- gen_ui_matrix(ml100k, ml100k)
sim_u <- gen_acos_sim(ui)[range, range]
sim_i <- gen_acos_sim(ui, FALSE)[range, range]

# sim_u[sim_u < 0.5] <- 0.1
# sim_i[sim_i < 0.5] <- 0.1
# sim_u[sim_u >= 0.5] <- 1
# sim_i[sim_i >= 0.5] <- 1

ui <- ui[range, range]

# generate clustering
split_u <- kamila_clust(ml100k_dem, 5)$clusters[range]
split_i <- gow_pam(ml100k_feat_b, 3, FALSE)$clusters[range]

# re-organise user-item matrix
ui_split_u <- ui
sim_split_u <- sim_u
clust_count <- c(0, cumsum(table(split_u)))
for (i in 1:4) {
  ui_split_u[(clust_count[i] + 1):clust_count[i + 1], ] <- ui[split_u == i, ]
  sim_split_u[(clust_count[i] + 1):clust_count[i + 1], ] <-
    sim_u[split_u == i, ]
  sim_split_u[, (clust_count[i] + 1):clust_count[i + 1]] <-
    sim_u[, split_u == i]
}

ui_split_i <- ui
sim_split_i <- sim_i
clust_count <- c(0, cumsum(table(split_i)))
for (i in 1:3) {
  ui_split_i[, (clust_count[i] + 1):clust_count[i + 1]] <- ui[, split_i == i]
  sim_split_i[, (clust_count[i] + 1):clust_count[i + 1]] <-
    sim_i[, split_i == i]
  sim_split_i[(clust_count[i] + 1):clust_count[i + 1], ] <-
    sim_i[split_i == i, ]
}

ui0 <- ui
ui0[is.na(ui)] <- 0.5
ui_noclust <- melt(t(ui0))

ui0 <- ui_split_u
ui0[is.na(ui_split_u)] <- 0.5
ui_clust_u <- melt(t(ui0))

ui0 <- ui_split_i
ui0[is.na(ui_split_i)] <- 0.5
ui_clust_i <- melt(t(ui0))

sim_noclust_u <- melt(sim_u)
sim_noclust_i <- melt(sim_i)
sim_clust_u <- melt(sim_split_u)
sim_clust_i <- melt(sim_split_i)

lines_u <- data.frame(x = rep(0, 4), y = cumsum(table(split_u))[1:4] + 0.5,
                      xend = rep(max(range) + 0.5, 4),
                      yend = cumsum(table(split_u))[1:4] + 0.5)

lines_i <- data.frame(x = cumsum(table(split_i))[1:2] + 0.5, y = rep(0, 2),
                      xend = cumsum(table(split_i))[1:2] + 0.5,
                      yend = rep(max(range) + 0.5, 2))

map_noclust <- ggplot(ui_noclust, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient2(high = hue_pal()(10)[2]) +
  xlab("Users") + ylab("Movies") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("User-Item Rating Matrix")
map_uclust <- ggplot(ui_clust_u, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient2(high = hue_pal()(10)[2]) +
  xlab("Users") + ylab("Movies") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("User Clustered Rating Matrix") +
  geom_segment(data = lines_u, aes(x, y, xend = xend, yend = yend),
               linewidth = 0.2, inherit.aes = F)
map_iclust <- ggplot(ui_clust_i, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient2(high = hue_pal()(10)[2]) +
  xlab("Users") + ylab("Movies") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("Item Clustered Rating Matrix") +
  geom_segment(data = lines_i, aes(x, y, xend = xend, yend = yend),
               linewidth = 0.2, inherit.aes = F)

# dimensions 20 x 5
print(ggarrange(map_noclust, map_uclust, map_iclust,
                nrow = 1, ncol = 3))

lines_u <- data.frame(x = rep(0, 4), y = cumsum(table(split_u))[1:4] + 0.5,
                      xend = rep(max(range) + 0.5, 4),
                      yend = cumsum(table(split_u))[1:4] + 0.5)

lines_i <- data.frame(x = cumsum(table(split_i))[1:2] + 0.5, y = rep(0, 2),
                      xend = cumsum(table(split_i))[1:2] + 0.5,
                      yend = rep(max(range) + 0.5, 2))

s_noclust_u <- ggplot(sim_noclust_u, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(high = hue_pal()(10)[2]) +
  xlab("User") + ylab("User") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("User Similarity Matrix")
s_noclust_i <- ggplot(sim_noclust_i, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(high = hue_pal()(10)[2]) +
  xlab("Movie") + ylab("Movie") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("Movie Similarity Matrix")
s_uclust <- ggplot(sim_clust_u, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(high = hue_pal()(10)[2]) +
  xlab("Users") + ylab("Users") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("User Clustered Similarity Matrix")
#   geom_segment(data = lines_u, aes(x, y, xend = xend, yend = yend),
#                linewidth = 0.2, inherit.aes = F)
s_iclust <- ggplot(sim_clust_i, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(high = hue_pal()(10)[2]) +
  xlab("Movies") + ylab("Movies") + labs(color = "Rating") +
  theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.border = element_blank()) +
  ggtitle("Item Clustered Similarity Matrix")
#   geom_segment(data = lines_i, aes(x, y, xend = xend, yend = yend),
#                linewidth = 0.2, inherit.aes = F)