cosine_similarity <- function(vector1, vector2) {
  # Filter out missing values (NAs)
  valid_indices <- which(!is.na(vector1) & !is.na(vector2))
  v1 <- vector1[valid_indices]
  v2 <- vector2[valid_indices]
  
  # Compute cosine similarity
  if (length(v1) == 0 || length(v2) == 0) {
    return(0)  # Return 0 if no overlap in ratings
  } else {
    return(sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2))))
  }
}

user_item_matrix <- matrix(
  c(1, NA, 3, 2, 3, NA, NA, 4, 1, 3, 1, NA, NA, 4, NA, 5),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(c("User1", "User2", "User3", "User4"), c("Item1", "Item2", "Item3", "Item4"))
)

similarity_matrix <- matrix(nrow = 4, ncol = 4)

for (i in 1:4) {
  for (j in 1:4) {
    similarity_matrix[i, j] <- cosine_similarity(user_item_matrix[i, ], user_item_matrix[j, ])
  }
}

# Set row and column names
rownames(similarity_matrix) <- colnames(similarity_matrix) <- c("User1", "User2", "User3", "User4")

sim <- similarity(as(user_item_matrix, "realRatingMatrix"), method = "cosine",
                    which = "users")
s <- as(sim, "matrix")
