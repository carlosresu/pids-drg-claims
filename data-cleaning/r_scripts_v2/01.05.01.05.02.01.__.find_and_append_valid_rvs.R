find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  datatable[, matches := lapply(col, function(x) {
    # Find valid RVS codes within each vector of 'col'
    valid_codes <- x[x %in% valid_rvs_codes]
    return(unique(valid_codes))
  })]

  # Append valid matches to the existing 'clin_rvs' vector
  datatable[, clin_rvs := mapply(function(rvs, matches) unique(c(rvs, matches)), clin_rvs, matches, SIMPLIFY = FALSE)]
}