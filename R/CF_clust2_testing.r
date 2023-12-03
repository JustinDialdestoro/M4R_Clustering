u100k <- read.table("M4R_Clustering/Data/u.data",
                    col.names = c("userID", "filmID", "rating", "timestamp"))

u1m <- read.csv("M4R_Clustering/Data/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")

udem <- read.table("M4R_Clustering/Data/u.user", sep = "|",
                   col.names = c("userID", "age", "gender",
                                 "occupation", "zip"))

source("M4R_Clustering/R/Mixed_Clust.r")
source("M4R_Clustering/R/CF_clust2.r")

krange <- seq(from = 10, to = 300, by = 10)
cos_scores <- cross_val(u100k, 10, gen_cos_sim, krange)
cos_scores_c2_2 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                gow_pam, udem, 2)
cos_scores_c2_3 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                gow_pam, udem, 3)
cos_scores_c2_4 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                gow_pam, udem, 4)
cos_scores_c2_5 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                gow_pam, udem, 5)
cos_scores_c2_6 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                gow_pam, udem, 6)

plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
     ylim = c(1.03, 1.25))
lines(krange, cos_scores_c2_2$rmse, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_c2_3$rmse, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_c2_4$rmse, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_c2_5$rmse, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_c2_6$rmse, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2)

plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
     ylim = c(0.83, 1))
lines(krange, cos_scores_c2_2$mae, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_c2_3$mae, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_c2_4$mae, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_c2_5$mae, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_c2_6$mae, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2)

plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
     ylim = c(0.04, 0.15))
lines(krange, cos_scores_c2_2$r2, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_c2_3$r2, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_c2_4$r2, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_c2_5$r2, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_c2_6$r2, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2)