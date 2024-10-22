get_unique_icd_codes <- function(c1, c2, clin_icd) {
  ## Obtains list of all unique ICD-10 codes across all cases and columns

  # Concatenate all elements from c1, c2, and clin_icd and remove duplicates using unique
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  # icds <- c(unlist(c1), unlist(c2), unlist(clin_icd))

  # Remove any NA values from the list of ICD codes
  icds <- icds[!is.na(icds)]

  # Return the unique list of ICD codes
  return(icds)
}
