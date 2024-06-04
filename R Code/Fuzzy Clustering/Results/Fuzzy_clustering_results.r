# load colour package
library("scales")

# read in the data
ml100k <- read.csv("M4R_Clustering/Data/ml100k.csv")

# call functions
source("M4R_Clustering/R Code/Fuzzy Clustering/Mixed_fuzzy_functions.r")
source("M4R_Clustering/R Code/Fuzzy Clustering/Fuzzy_clustering.r")
source("M4R_Clustering/R Code/Collaborative Filtering/CF.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Similarities.r")
source("M4R_Clustering/R Code/Collaborative Filtering/Predictors.r")

# initialise evaluation fixed variables
krange <- seq(from = 10, to = 300, by = 10)

# evaluate performance using optimum number of clusters
fclust_u <- cval_fclust_split(ml100k, 10, 7, 1.2, krange, gen_acos_sim,
                              mean_centered)
write.csv(fclust_u,
          "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_u.csv",
          row.names = FALSE)

# evaluate performance using optimum number of clusters
fclust_i <- cval_fclust_split(ml100k, 10, 6, 1.2, krange, gen_acos_sim,
                              mean_centered, FALSE)
write.csv(fclust_i,
          "M4R_Clustering/Results/Rating clustering/Fuzzy/fclust_i.csv",
          row.names = FALSE)
