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

cos_scores_gow_2 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                 gow_pam, udem, 2)
cos_scores_gow_3 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                 gow_pam, udem, 3)
cos_scores_gow_4 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                 gow_pam, udem, 4)
cos_scores_gow_5 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                 gow_pam, udem, 5)
cos_scores_gow_6 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
                                 gow_pam, udem, 6)

plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
     ylim = c(1.01, 1.065))
lines(krange, cos_scores_gow_2$rmse, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_gow_3$rmse, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_gow_4$rmse, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_gow_5$rmse, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_gow_6$rmse, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2, cex = 0.8)

plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
     ylim = c(0.8, 0.835))
lines(krange, cos_scores_gow_2$mae, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_gow_3$mae, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_gow_4$mae, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_gow_5$mae, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_gow_6$mae, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2, cex = 0.8)

plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
     ylim = c(0.15, 0.205))
lines(krange, cos_scores_gow_2$r2, type = "l", col = "#020294", lwd = 2)
lines(krange, cos_scores_gow_3$r2, type = "l", col = "blue", lwd = 2)
lines(krange, cos_scores_gow_4$r2, type = "l", col = "#006eff", lwd = 2)
lines(krange, cos_scores_gow_5$r2, type = "l", col = "#00aaff", lwd = 2)
lines(krange, cos_scores_gow_6$r2, type = "l", col = "#00fff7", lwd = 2)
legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
       col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
       lwd = 2, cex = 0.8)

# cos_scores_hl_2 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                 hl_pam, udem, 2)
# cos_scores_hl_3 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                 hl_pam, udem, 3)
# cos_scores_hl_4 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                 hl_pam, udem, 4)
# cos_scores_hl_5 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                 hl_pam, udem, 5)
# cos_scores_hl_6 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                 hl_pam, udem, 6)

# plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
#      ylim = c(1.03, 1.25))
# lines(krange, cos_scores_hl_2$rmse, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_hl_3$rmse, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_hl_4$rmse, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_hl_5$rmse, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_hl_6$rmse, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
#      ylim = c(0.83, 1))
# lines(krange, cos_scores_hl_2$mae, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_hl_3$mae, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_hl_4$mae, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_hl_5$mae, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_hl_6$mae, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
#      ylim = c(0.04, 0.15))
# lines(krange, cos_scores_hl_2$r2, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_hl_3$r2, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_hl_4$r2, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_hl_5$r2, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_hl_6$r2, type = "l", col = "#00fff7", lwd = 2)
# legend("bottomright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# cos_scores_kproto_2 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                     kprototypes, udem, 2)
# cos_scores_kproto_3 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                     kprototypes, udem, 3)
# cos_scores_kproto_4 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                     kprototypes, udem, 4)
# cos_scores_kproto_5 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                     kprototypes, udem, 5)
# cos_scores_kproto_6 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                     kprototypes, udem, 6)

# plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
#      ylim = c(1.03, 1.25))
# lines(krange, cos_scores_kproto_2$rmse, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_kproto_3$rmse, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_kproto_4$rmse, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_kproto_5$rmse, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_kproto_6$rmse, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
#      ylim = c(0.83, 1))
# lines(krange, cos_scores_kproto_2$mae, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_kproto_3$mae, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_kproto_4$mae, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_kproto_5$mae, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_kproto_6$mae, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
#      ylim = c(0.04, 0.15))
# lines(krange, cos_scores_kproto_2$r2, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_kproto_3$r2, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_kproto_4$r2, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_kproto_5$r2, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_kproto_6$r2, type = "l", col = "#00fff7", lwd = 2)
# legend("bottomright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# cos_scores_mixed_2 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                    mixed_k, udem, 2)
# cos_scores_mixed_3 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                    mixed_k, udem, 3)
# cos_scores_mixed_4 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                    mixed_k, udem, 4)
# cos_scores_mixed_5 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                    mixed_k, udem, 5)
# cos_scores_mixed_6 <- cross_val_c2(u100k, 10, gen_cos_sim, krange,
#                                    mixed_k, udem, 6)

# plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
#      ylim = c(1.03, 1.25))
# lines(krange, cos_scores_mixed_2$rmse, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_mixed_3$rmse, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_mixed_4$rmse, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_mixed_5$rmse, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_mixed_6$rmse, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
#      ylim = c(0.83, 1))
# lines(krange, cos_scores_mixed_2$mae, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_mixed_3$mae, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_mixed_4$mae, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_mixed_5$mae, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_mixed_6$mae, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
#      ylim = c(0.04, 0.15))
# lines(krange, cos_scores_mixed_2$r2, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_mixed_3$r2, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_mixed_4$r2, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_mixed_5$r2, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_mixed_6$r2, type = "l", col = "#00fff7", lwd = 2)
# legend("bottomright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# cos_scores_gow_2 <- cross_val_c2(u100k, 10, gen_cos_sim_2, krange,
#                                 gow_pam, udem, 2)
# cos_scores_gow_3 <- cross_val_c2(u100k, 10, gen_cos_sim_2, krange,
#                                 gow_pam, udem, 3)
# cos_scores_gow_4 <- cross_val_c2(u100k, 10, gen_cos_sim_2, krange,
#                                 gow_pam, udem, 4)
# cos_scores_gow_5 <- cross_val_c2(u100k, 10, gen_cos_sim_2, krange,
#                                 gow_pam, udem, 5)
# cos_scores_gow_6 <- cross_val_c2(u100k, 10, gen_cos_sim_2, krange,
#                                 gow_pam, udem, 6)

# plot(krange, cos_scores$rmse, type = "l", col = "red", lwd = 2,
#      ylim = c(1.005, 1.08))
# lines(krange, cos_scores_gow_2$rmse, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_gow_3$rmse, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_gow_4$rmse, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_gow_5$rmse, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_gow_6$rmse, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$mae, type = "l", col = "red", lwd = 2,
#      ylim = c(0.8, 0.85))
# lines(krange, cos_scores_gow_2$mae, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_gow_3$mae, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_gow_4$mae, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_gow_5$mae, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_gow_6$mae, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)

# plot(krange, cos_scores$r2, type = "l", col = "red", lwd = 2,
#      ylim = c(0.14, 0.21))
# lines(krange, cos_scores_gow_2$r2, type = "l", col = "#020294", lwd = 2)
# lines(krange, cos_scores_gow_3$r2, type = "l", col = "blue", lwd = 2)
# lines(krange, cos_scores_gow_4$r2, type = "l", col = "#006eff", lwd = 2)
# lines(krange, cos_scores_gow_5$r2, type = "l", col = "#00aaff", lwd = 2)
# lines(krange, cos_scores_gow_6$r2, type = "l", col = "#00fff7", lwd = 2)
# legend("topright", c("No clustering", "k=2", "k=3", "k=4", "k=5", "k=6"),
#        col = c("red", "#020294", "blue", "#006eff", "#00aaff", "#00fff7"),
#        lwd = 2)