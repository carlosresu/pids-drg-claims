remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_) # Return NA for NULL or empty lists
  } else {
    return(gsub("\\s+", "", x)) # Remove all whitespace characters
  }
}
