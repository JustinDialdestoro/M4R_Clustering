# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_ups.csv"
pred_ups <- read.csv(loc)
noclust_u <- pred_ups[pred_ups$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/pref_clust_ups.csv"
pref_clust_ups <- read.csv(loc)
pref_clust_u <- pref_clust_ups[pref_clust_ups$predictor ==
                                 "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_u.csv"
clust_u <- read.csv(loc)

full <- rbind(noclust_u, pref_clust_u, clust_u,
              gow_u, hl_u, kproto_u, mk_u, msk_u, famd_u, mrk_u, kam_u)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("bottomright",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:10) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 1])
}
legend("topleft",
       c("no clustering", "preference clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)),
       lty = 1, lwd = 2, cex = 0.5)

# load unclustered performance results
loc <- "M4R_Clustering/Results/Collaborative Filtering/pred_i.csv"
pred_i <- read.csv(loc)
noclust_i <- pred_i[pred_i$predictor == "mean centred", ][2:6]
loc <- "M4R_Clustering/Results/Rating clustering/Crisp/clust_i.csv"
clust_i <- read.csv(loc)

full <- rbind(noclust_i, clust_i,
              gow_i, hl_i, kproto_i, mk_i, msk_i, famd_i, mrk_i, kam_i)

ymax <- max(full$rmse)
ymin <- min(full$rmse)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$rmse, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "RMSE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$rmse,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$mae)
ymin <- min(full$mae)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$mae, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "MAE",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$mae,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$r2)
ymin <- min(full$r2)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$r2, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours", ylab = "R2",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$r2,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topright",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)

ymax <- max(full$online)
ymin <- min(full$online)
ygap <- 0.2 * (ymax - ymin)

plot(krange, full[1:30, ]$online, lty = 1, type = "l", lwd = 2,
     col = hue_pal()(11)[1], xlab = "k neighbours",
     ylab = "Online phase time (seconds)",
     ylim = c(ymin - ygap, ymax + ygap))
for (i in 1:9) {
  lines(krange, full[(i * 30 + 1):((i + 1) * 30), ]$online,
        lty = 1, type = "l", lwd = 2, col = hue_pal()(11)[i + 2])
}
legend("topleft",
       c("no clustering", "rating clustering",
         "Gower/PAM", "HL/PAM", "K-prototypes", "Mixed K-Means", "MS K-Means",
         "FAMD", "Mixed RKM", "KAMILA"),
       col = c(hue_pal()(11)[-2]),
       lty = 1, lwd = 2, cex = 0.5)