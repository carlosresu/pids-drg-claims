warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  ## Triggers warnings for invalid RVS codes

  # Create a new environment for valid RVS codes
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())

  # Populate the environment with valid RVS codes
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # Identify invalid RVS codes by checking if they exist in the valid_rvs_env environment
  invalid_matches <- lapply(matches, function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)])

  # Flatten the list of invalid matches into a single vector
  discarded_codes <- unlist(invalid_matches)

  # If invalid codes exist, create a summary table of their counts
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }

  # Return the table of invalid codes and their counts
  return(discarded_table)
}
