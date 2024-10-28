append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  ## Ensure both clin_rvs and col are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  # Find and append valid RVS codes to clin_rvs
  datatable[, matches := lapply(col, function(x) {
    # Identify valid RVS codes within each vector of 'col'
    valid_codes <- x[x %in% valid_rvs_codes]
    return(unique(valid_codes))
  })]

  # Append valid matches to the existing 'clin_rvs' vector
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique(c(rvs, matches))
  }, clin_rvs, matches, SIMPLIFY = FALSE)]

  # Recursively unlist
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA_character_)
    } else {
      return(unlist(x, recursive = TRUE, use.names = FALSE))
    }
  })]

  # Trigger warnings for invalid RVS codes
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv()) # Create environment for valid RVS codes

  # Populate the environment with valid RVS codes
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # Identify invalid RVS codes by checking against the valid RVS environment
  invalid_matches <- lapply(datatable$matches, function(x) {
    x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
  })

  # Flatten the list of invalid matches into a single vector
  discarded_codes <- unlist(invalid_matches)

  # Create a summary table of discarded codes if any invalid codes are found
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }

  # Return updated clin_rvs, cleaned col, and discarded RVS codes
  return(list(
    clin_rvs = datatable$clin_rvs,
    col = datatable$col,
    discarded_rvs = discarded_table
  ))
}
