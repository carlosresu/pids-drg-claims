apply_icd10_mapping_to_columns <- function(c1, c2, clin_icd, icd10_env) {
  ## Maps ICD-10 codes to the given columns using the provided environment

  # Helper function to map ICD-10 codes using the provided environment
  map_icd10_helper <- function(codes) {
    # Use mget to map each code to its equivalent in icd10_env or return the original if no match is found
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped))) # Return the mapped codes as an unnamed vector
  }

  # Apply the mapping function to each of the columns (c1, c2, and clin_icd)
  # Cel: change this to a for-loop
  # Carlos: lapply is faster because lapply is optimized for iteration in R’s internal C/C++ code,
  # Carlos: whereas for loops have more overhead due to their explicit nature in R.
  c1_mapped <- lapply(c1, map_icd10_helper)
  c2_mapped <- lapply(c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  # Return the mapped values for c1, c2, and clin_icd
  return(list(c1 = c1_mapped, c2 = c2_mapped, clin_icd = clin_icd_mapped))
}
