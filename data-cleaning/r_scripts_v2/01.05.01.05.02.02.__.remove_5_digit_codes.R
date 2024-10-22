remove_5_digit_codes <- function(col) {
  # Handle the column as a list of vectors
  return(lapply(col, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(x)
    }
    # stri_replace_all_regex(x, "\\b\\d{5}\\b", "") # Don't delete rvs codes from c1 and c2
  }))
}
