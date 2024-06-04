# load colour package
library("scales")
library("ggplot2")

# read in the data
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_u.csv"
mclust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_i.csv"
mclust_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_u.csv"
pred_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv"
clust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv"
clust_i <- read.csv(loc)

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
labels <- c("None", "k-Means", "k-Prototypes", "Gower", "Hennig-Liao",
            "Mixed k-Means", "Modha-Spangler k-Means", "FAMD",
            "Mixed Reduced k-Means", "KAMILA")
limits <- c("none", "ratings", "kproto", "gow", "hl", "mk", "msk", "famd",
            "mrk", "kam")
colors <- c(hue_pal()(10)[1])
for (i in 2:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

# prepare results
noclust_u <- pred_u[61:90, ]
names(noclust_u)[1] <- "clustering"
noclust_u[1] <- "none"
clust_u <- cbind(clustering = "ratings", clust_u)
names(mclust_u)[1] <- "clustering"
full_u <- rbind(noclust_u, clust_u, mclust_u)
full_u$k <- rep(krange, 10)

# prepare results
noclust_i <- pred_i[61:90, ]
names(noclust_i)[1] <- "clustering"
noclust_i[1] <- "none"
clust_i <- cbind(clustering = "ratings", clust_i)
names(mclust_i)[1] <- "clustering"
full_i <- rbind(noclust_i, clust_i, mclust_i)
full_i$k <- rep(krange, 10)

mclust_u_mae <- ggplot(full_u, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

mclust_u_r2 <- ggplot(full_u, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

mclust_u_online <- ggplot(full_u, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

mclust_i_mae <- ggplot(full_i, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

mclust_i_r2 <- ggplot(full_i, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

mclust_i_online <- ggplot(full_i, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

# dimensions 15 x 9
print(ggarrange(mclust_u_mae, mclust_u_r2, mclust_u_online,
                mclust_i_mae, mclust_i_r2, mclust_i_online,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
                font.label = c(size = 15)))