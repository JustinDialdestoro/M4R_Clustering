# load colour package
library("scales")
library("ggplot2")
library("ggpubr")

# read in the data
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fmclust_split_u.csv"
fmclust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fmclust_split_i.csv"
fmclust_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_u.csv"
pred_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_u.csv"
fclust_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_i.csv"
fclust_i <- read.csv(loc)

# initialise plotting variables
krange <- seq(from = 10, to = 300, by = 10)
labels <- c("None", "Fuzzy k-Means", "k-Prototypes", "Gower", "Hennig-Liao",
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
fclust_u <- cbind(clustering = "ratings", fclust_u)
names(fmclust_u)[1] <- "clustering"
full_u <- rbind(noclust_u, fclust_u, fmclust_u)
full_u$k <- rep(krange, 10)

# prepare results
noclust_i <- pred_i[61:90, ]
names(noclust_i)[1] <- "clustering"
noclust_i[1] <- "none"
fclust_i <- cbind(clustering = "ratings", fclust_i)
names(fmclust_i)[1] <- "clustering"
full_i <- rbind(noclust_i, fclust_i, fmclust_i)
full_i$k <- rep(krange, 10)

fmclust_u_mae <- ggplot(full_u, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

fmclust_u_r2 <- ggplot(full_u, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

fmclust_u_online <- ggplot(full_u, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

fmclust_i_mae <- ggplot(full_i, aes(x = k, y = mae, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("MAE") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

fmclust_i_r2 <- ggplot(full_i, aes(x = k, y = r2, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab(expression(R^2)) + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

fmclust_i_online <- ggplot(full_i, aes(x = k, y = online, color = clustering)) +
  geom_line(linewidth = 0.8) + labs(color = "Clustering Method:") +
  xlab("N Neighbours") + ylab("Time (seconds)") + theme_bw(base_size = 15) +
  scale_color_manual(labels = labels, limits = limits, values = colors)

# dimensions 15 x 9
print(ggarrange(fmclust_u_mae, fmclust_u_r2, fmclust_u_online,
                fmclust_i_mae, fmclust_i_r2, fmclust_i_online,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
                font.label = c(size = 15)))