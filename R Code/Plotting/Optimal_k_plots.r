# load packages
library("scales")
library("ggplot2")
library("ggpubr")
library("extrafont")

# read in the data
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_u.csv"
clust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_obj_i.csv"
clust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_u.csv"
mclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Crisp/mclust_obj_i.csv"
mclust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_obj_u.csv"
fclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_obj_i.csv"
fclust_obj_i <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fmclust_obj_u.csv"
fmclust_obj_u <- read.csv(loc)
loc <- "M4R_Clustering/Results/Mixed clustering/Fuzzy/fmclust_obj_i.csv"
fmclust_obj_i <- read.csv(loc)

# initialise plotting variables
nrange <- 2:15
colors <- c(hue_pal()(10)[1])
for (i in 2:10) {
  colors <- c(colors, hue_pal()(10)[i])
}

clust_obj_u <- data.frame(n = nrange, clust_obj_u)
clust_obj_i <- data.frame(n = nrange, clust_obj_i)

elbow_clust_u <- ggplot(clust_obj_u, aes(x = n, y = x)) +
  geom_line(color = colors[2], linewidth = 1) +
  geom_point(color = colors[2], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
  ggtitle("User Ratings Cluster Elbow") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

elbow_clust_i <- ggplot(clust_obj_i, aes(x = n, y = x)) +
  geom_line(color = colors[2], linewidth = 1) +
  geom_point(color = colors[2], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
  ggtitle("Item Ratings Cluster Elbow") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 5
print(ggarrange(elbow_clust_u, elbow_clust_i))

mclust_obj_u$n <- nrange
mclust_obj_i$n <- nrange

labels <- c("kproto", "gow", "hl", "mk", "msk", "famd", "mrk", "kam")
titles <- c("k-Prototypes Elbow", "Gower Elbow", "Hennig-Liao Elbow",
            "Mixed k-Means Elbow", "Modha-Spangler k-Means Elbow", "FAMD Elbow",
            "Mixed Reduced k-Means Elbow", "KAMILA Elbow")
obj <- c("Loss", "Loss", "Loss", "Loss", "Loss", "WCSS", "WCSS",
         "Log-Likelihood")

elbow_mclust_u_kproto <- ggplot(mclust_obj_u, aes(x = n, y = kproto)) +
  geom_line(color = colors[3], linewidth = 1) +
  geom_point(color = colors[3], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[1]) +
  ggtitle(titles[1]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_gow <- ggplot(mclust_obj_u, aes(x = n, y = gow)) +
  geom_line(color = colors[4], linewidth = 1) +
  geom_point(color = colors[4], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[2]) +
  ggtitle(titles[2]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_hl <- ggplot(mclust_obj_u, aes(x = n, y = hl)) +
  geom_line(color = colors[5], linewidth = 1) +
  geom_point(color = colors[5], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[3]) +
  ggtitle(titles[3]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_mk <- ggplot(mclust_obj_u, aes(x = n, y = mk)) +
  geom_line(color = colors[6], linewidth = 1) +
  geom_point(color = colors[6], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[4]) +
  ggtitle(titles[4]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_msk <- ggplot(mclust_obj_u, aes(x = n, y = msk)) +
  geom_line(color = colors[7], linewidth = 1) +
  geom_point(color = colors[7], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[5]) +
  ggtitle(titles[5]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_famd <- ggplot(mclust_obj_u, aes(x = n, y = famd)) +
  geom_line(color = colors[8], linewidth = 1) +
  geom_point(color = colors[8], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[6]) +
  ggtitle(titles[6]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_mrk <- ggplot(mclust_obj_u, aes(x = n, y = mrk)) +
  geom_line(color = colors[9], linewidth = 1) +
  geom_point(color = colors[9], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[7]) +
  ggtitle(titles[7]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_u_kam <- ggplot(mclust_obj_u, aes(x = n, y = kam)) +
  geom_line(color = colors[10], linewidth = 1) +
  geom_point(color = colors[10], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[8]) +
  ggtitle(titles[8]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 20 portrait
print(ggarrange(elbow_mclust_u_kproto, elbow_mclust_u_gow, elbow_mclust_u_hl,
                elbow_mclust_u_mk, elbow_mclust_u_msk, elbow_mclust_u_famd,
                elbow_mclust_u_mrk, elbow_mclust_u_kam,
                nrow = 4, ncol = 2))

# dimensions 13 x 5 portrait
print(ggarrange(elbow_mclust_u_kproto, elbow_mclust_u_kam))

elbow_mclust_i_kproto <- ggplot(mclust_obj_i, aes(x = n, y = kproto)) +
  geom_line(color = colors[3], linewidth = 1) +
  geom_point(color = colors[3], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[1]) +
  ggtitle(titles[1]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_gow <- ggplot(mclust_obj_i, aes(x = n, y = gow)) +
  geom_line(color = colors[4], linewidth = 1) +
  geom_point(color = colors[4], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[2]) +
  ggtitle(titles[2]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_hl <- ggplot(mclust_obj_i, aes(x = n, y = hl)) +
  geom_line(color = colors[5], linewidth = 1) +
  geom_point(color = colors[5], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[3]) +
  ggtitle(titles[3]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_mk <- ggplot(mclust_obj_i, aes(x = n, y = mk)) +
  geom_line(color = colors[6], linewidth = 1) +
  geom_point(color = colors[6], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[4]) +
  ggtitle(titles[4]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_msk <- ggplot(mclust_obj_i, aes(x = n, y = msk)) +
  geom_line(color = colors[7], linewidth = 1) +
  geom_point(color = colors[7], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[5]) +
  ggtitle(titles[5]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_famd <- ggplot(mclust_obj_i, aes(x = n, y = famd)) +
  geom_line(color = colors[8], linewidth = 1) +
  geom_point(color = colors[8], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[6]) +
  ggtitle(titles[6]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_mrk <- ggplot(mclust_obj_i, aes(x = n, y = mrk)) +
  geom_line(color = colors[9], linewidth = 1) +
  geom_point(color = colors[9], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[7]) +
  ggtitle(titles[7]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_mclust_i_kam <- ggplot(mclust_obj_i, aes(x = n, y = kam)) +
  geom_line(color = colors[10], linewidth = 1) +
  geom_point(color = colors[10], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[8]) +
  ggtitle(titles[8]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 20 portrait
print(ggarrange(elbow_mclust_i_kproto, elbow_mclust_i_gow, elbow_mclust_i_hl,
                elbow_mclust_i_mk, elbow_mclust_i_msk, elbow_mclust_i_famd,
                elbow_mclust_i_mrk, elbow_mclust_i_kam,
                nrow = 4, ncol = 2))

fclust_obj_u <- data.frame(n = nrange, fclust_obj_u)
fclust_obj_i <- data.frame(n = nrange, fclust_obj_i)

elbow_fclust_u <- ggplot(fclust_obj_u, aes(x = n, y = x)) +
  geom_line(color = colors[2], linewidth = 1) +
  geom_point(color = colors[2], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
  ggtitle("User Ratings Fuzzy Cluster Elbow") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

elbow_fclust_i <- ggplot(fclust_obj_i, aes(x = n, y = x)) +
  geom_line(color = colors[2], linewidth = 1) +
  geom_point(color = colors[2], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab("WCSS") +
  ggtitle("Item Ratings Fuzzy Cluster Elbow") + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 5
print(ggarrange(elbow_fclust_u, elbow_fclust_i))

fmclust_obj_u$n <- nrange
fmclust_obj_i$n <- nrange

elbow_fmclust_u_kproto <- ggplot(fmclust_obj_u, aes(x = n, y = kproto)) +
  geom_line(color = colors[3], linewidth = 1) +
  geom_point(color = colors[3], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[1]) +
  ggtitle(titles[1]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_gow <- ggplot(fmclust_obj_u, aes(x = n, y = gow)) +
  geom_line(color = colors[4], linewidth = 1) +
  geom_point(color = colors[4], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[2]) +
  ggtitle(titles[2]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_hl <- ggplot(fmclust_obj_u, aes(x = n, y = hl)) +
  geom_line(color = colors[5], linewidth = 1) +
  geom_point(color = colors[5], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[3]) +
  ggtitle(titles[3]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_mk <- ggplot(fmclust_obj_u, aes(x = n, y = mk)) +
  geom_line(color = colors[6], linewidth = 1) +
  geom_point(color = colors[6], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[4]) +
  ggtitle(titles[4]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_msk <- ggplot(fmclust_obj_u, aes(x = n, y = msk)) +
  geom_line(color = colors[7], linewidth = 1) +
  geom_point(color = colors[7], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[5]) +
  ggtitle(titles[5]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_famd <- ggplot(fmclust_obj_u, aes(x = n, y = famd)) +
  geom_line(color = colors[8], linewidth = 1) +
  geom_point(color = colors[8], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[6]) +
  ggtitle(titles[6]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_mrk <- ggplot(fmclust_obj_u, aes(x = n, y = mrk)) +
  geom_line(color = colors[9], linewidth = 1) +
  geom_point(color = colors[9], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[7]) +
  ggtitle(titles[7]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_u_kam <- ggplot(fmclust_obj_u, aes(x = n, y = kam)) +
  geom_line(color = colors[10], linewidth = 1) +
  geom_point(color = colors[10], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[8]) +
  ggtitle(titles[8]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 20 portrait
print(ggarrange(elbow_fmclust_u_kproto, elbow_fmclust_u_gow, elbow_fmclust_u_hl,
                elbow_fmclust_u_mk, elbow_fmclust_u_msk, elbow_fmclust_u_famd,
                elbow_fmclust_u_mrk, elbow_fmclust_u_kam,
                nrow = 4, ncol = 2))

elbow_fmclust_i_kproto <- ggplot(fmclust_obj_i, aes(x = n, y = kproto)) +
  geom_line(color = colors[3], linewidth = 1) +
  geom_point(color = colors[3], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[1]) +
  ggtitle(titles[1]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_gow <- ggplot(fmclust_obj_i, aes(x = n, y = gow)) +
  geom_line(color = colors[4], linewidth = 1) +
  geom_point(color = colors[4], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[2]) +
  ggtitle(titles[2]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_hl <- ggplot(fmclust_obj_i, aes(x = n, y = hl)) +
  geom_line(color = colors[5], linewidth = 1) +
  geom_point(color = colors[5], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[3]) +
  ggtitle(titles[3]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_mk <- ggplot(fmclust_obj_i, aes(x = n, y = mk)) +
  geom_line(color = colors[6], linewidth = 1) +
  geom_point(color = colors[6], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[4]) +
  ggtitle(titles[4]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_msk <- ggplot(fmclust_obj_i, aes(x = n, y = msk)) +
  geom_line(color = colors[7], linewidth = 1) +
  geom_point(color = colors[7], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[5]) +
  ggtitle(titles[5]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_famd <- ggplot(fmclust_obj_i, aes(x = n, y = famd)) +
  geom_line(color = colors[8], linewidth = 1) +
  geom_point(color = colors[8], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[6]) +
  ggtitle(titles[6]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_mrk <- ggplot(fmclust_obj_i, aes(x = n, y = mrk)) +
  geom_line(color = colors[9], linewidth = 1) +
  geom_point(color = colors[9], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[7]) +
  ggtitle(titles[7]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))
elbow_fmclust_i_kam <- ggplot(fmclust_obj_i, aes(x = n, y = kam)) +
  geom_line(color = colors[10], linewidth = 1) +
  geom_point(color = colors[10], size = 2.5) +
  theme(legend.position = "none") + xlab("k Clusters") + ylab(obj[8]) +
  ggtitle(titles[8]) + theme_bw(base_size = 20) +
  theme(text = element_text(family = "LM Roman 10"))

# dimensions 13 x 20 portrait
print(ggarrange(elbow_fmclust_i_kproto, elbow_fmclust_i_gow, elbow_fmclust_i_hl,
                elbow_fmclust_i_mk, elbow_fmclust_i_msk, elbow_fmclust_i_famd,
                elbow_fmclust_i_mrk, elbow_fmclust_i_kam,
                nrow = 4, ncol = 2))