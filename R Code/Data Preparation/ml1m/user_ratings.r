# read 1m rating data
u1m <- read.csv("Data/ml-1m/ml-1m/ratings.dat", sep = ":",
                colClasses = c(NA, "NULL"), header = FALSE)

# column names for 1m dataset
colnames(u1m) <- c("userID", "filmID", "rating", "timestamp")