### recommender for real-valued ratings
library("recommenderlab")
data(MovieLense)
## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(MovieLense, method = "split",
                      train = 0.9, k = 10, given = -1)

rmse <- c()

## create a user-based CF recommender using training data
for (k in seq(from = 10, to = 300, by = 10)) {
  r <- Recommender(getData(e, "train"), "UBCF",
                   parameter = c(nn = k, method = "cosine", weighted = TRUE))
  p <- predict(r, getData(e, "known"), type = "ratings")
  t <- calcPredictionAccuracy(p, getData(e, "unknown"))
  rmse <- c(rmse, t[1])
}
