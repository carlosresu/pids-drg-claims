split_to_vector <- function(column) {
  ## Splits a column of strings into vectors using "||" as the delimiter
  # column: the column to split

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # Split the string into a vector using "||"
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })

  # Return the list of vectors
  return(result)
}
