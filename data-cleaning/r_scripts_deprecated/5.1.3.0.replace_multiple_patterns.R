# Function to replace multiple patterns with corresponding replacements
replace_multiple_patterns <- function(text, patterns, replacements) {
  # Ensure patterns and replacements are the same length
  if (length(patterns) != length(replacements)) {
    stop("Patterns and replacements must have the same length.")
  }

  # Perform replacements
  modified_text <- stri_replace_all_regex(
    text,
    pattern = patterns,
    replacement = replacements,
    vectorize_all = FALSE # Apply all replacements simultaneously
  )

  # return the text after modification
  return(modified_text)
}
