apply_add_c1_c2_to_clin_icd <- function(dt) {
  ## Deduplicates ICD codes in clin_icd if already present in c1 or c2
  # dt: input data.table

  # Add ICD codes from c1 and c2 to clin_icd, removing duplicates
  result <- add_c1_c2_to_clin_icd(dt$c1, dt$c2, dt$clin_icd)

  # Update clin_icd with the deduplicated result
  dt[, clin_icd := result$clin_icd]

  # Return the deduplicated data.table
  return(dt)
}
