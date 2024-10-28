# Function to check similarity between two strings
check_similarity <- function(x, y) {
  score <- 0
  min_len <- min(nchar(x), nchar(y))
  for (i in 1:min_len) {
    if (substr(x, i, i) == substr(y, i, i)) {
      score <- score + 1
    }
  }
  return(score)
}
