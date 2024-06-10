# load colour package
library("scales")
library("ggplot2")
library("ggpubr")
library("extrafont")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")
ml100k_dem <- read.csv("M4R_Clustering/Data/ml100k_dem.csv")
ml100k_feat_b <- read.csv("M4R_Clustering/Data/ml100k_feat_b.csv")
ml100k_feat_d <- read.csv("M4R_Clustering/Data/ml100k_feat_d.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_functions.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")

# initialise plotting variables
ui <- gen_ui_matrix(ml100k, ml100k)

# compute mixed clustering memberships
fmemb_kmeans <- fuzzy_c_means(t(ui), 6, 1.2)$clusters
fmemb_hl <- fuzzy_hl(ml100k_feat_d, 5, 2, FALSE)$clusters
fmemb_kam <- fuzzy_kamila(ml100k_feat_b, 5, 1.2, FALSE)$clusters

fmemb_kproto_1 <- fuzzy_kproto(ml100k_dem, 5, 2)$clusters
fmemb_kproto_2 <- fuzzy_kproto(ml100k_dem, 5, 1.6)$clusters
fmemb_kproto_3 <- fuzzy_kproto(ml100k_dem, 5, 1.2)$clusters

# find users belonging to each cluster
avg_memb <- function(clusters) {
  c <- nrow(clusters)
  assigned <- c()
  for (j in 1:c) {
    assigned <- c(assigned, rep(j, c))
  }
  avgs <- data.frame(assigned = assigned, cluster = rep(1:c, c),
                     weights = rep(0, c**2))
  for (j in 0:(c - 1)) {
    avgs[(j * c + 1):((j + 1) * c), 3] <-
      rowMeans(clusters[, which(clusters[j + 1, ] > 1 / c)])
  }

  return(avgs)
}

kmeans_avg <- avg_memb(fmemb_kmeans)
hl_avg <- avg_memb(fmemb_hl)
kam_avg <- avg_memb(fmemb_kam)

kmeans_avg_plot <- ggplot(kmeans_avg, aes(fill = cluster, x = assigned,
                                          y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[2]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("Fuzzy k-Means Cluster Distribution")
hl_avg_plot <- ggplot(hl_avg, aes(fill = cluster, x = assigned, y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[5]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("Hennig-Liao Cluster Distribution")
kam_avg_plot <- ggplot(kam_avg, aes(fill = cluster, x = assigned,
                                    y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[10]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("KAMILA Cluster Distribution")

# dimensions 20 x 5
print(ggarrange(kmeans_avg_plot, hl_avg_plot, kam_avg_plot,
                nrow = 1, ncol = 3))

kproto_avg_1 <- avg_memb(fmemb_kproto_1)
kproto_avg_2 <- avg_memb(fmemb_kproto_2)
kproto_avg_3 <- avg_memb(fmemb_kproto_3)

kproto_avg_plot_1 <- ggplot(kproto_avg_1, aes(fill = cluster, x = assigned,
                                              y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[3]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("k-Prototypes, m = 2")
kproto_avg_plot_2 <- ggplot(kproto_avg_2, aes(fill = cluster, x = assigned,
                                              y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[3]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("k-Prototypes, m = 1.6")
kproto_avg_plot_3 <- ggplot(kproto_avg_3, aes(fill = cluster, x = assigned,
                                              y = weights)) +
  geom_bar(position = "stack", stat = "identity") + xlab("Assigned Cluster") +
  scale_fill_gradient2(high = hue_pal()(10)[3]) +
  ylab("Average Membership") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"), legend.position = "none") +
  ggtitle("k-Prototypes, m = 1.2")

# dimensions 20 x 5
print(ggarrange(kproto_avg_plot_1, kproto_avg_plot_2, kproto_avg_plot_3,
                nrow = 1, ncol = 3))
