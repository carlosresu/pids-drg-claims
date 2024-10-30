# Safely extracts fields from summaries with error handling
safe_extract <- function(summary, field) {
  tryCatch(summary[[field]], error = function(e) NULL)
}
