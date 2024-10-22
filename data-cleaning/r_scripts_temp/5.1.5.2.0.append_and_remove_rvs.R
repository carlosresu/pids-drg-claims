append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  ## Ensure both clin_rvs and col are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col)

  valid_rvs_codes <- rvs_icd9$rvs

  # Append valid RVS codes to clin_rvs (handling each element of the vectors)
  find_and_append_valid_rvs(datatable, valid_rvs_codes)

  # Modify the column by removing 5-digit codes from each vector and recursively unlisting
  datatable[, col := lapply(col, function(x) {
    # Optimize by checking if x is already a character vector
    cleaned_col <- if (is.character(x)) {
      remove_5_digit_codes(x) # Directly modify the character vector
    } else {
      # Recursively unlist and clean the elements
      remove_5_digit_codes(as.character(x))
    }
    return(unlist(cleaned_col))
  })]

  # Trigger warnings for invalid RVS codes
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  # Return updated clin_rvs, cleaned col, and invalid codes
  return(list(clin_rvs = datatable$clin_rvs, col = datatable$col, discarded_rvs = discarded_rvs))
}
