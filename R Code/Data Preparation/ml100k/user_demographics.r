# read 100k user demographic data
u100k_dem <- read.table("Data/ml-100k/ml-100k/u.user", sep = "|",
                        col.names = c("userID", "age", "gender",
                                      "occupation", "zip"))

u100k_dem$userID <- NULL

# write cleaned 100k user demographic data into file
write.csv(u100k_dem, file = "M4R_Clustering/Data/u100k_dem.csv",
          row.names = FALSE)