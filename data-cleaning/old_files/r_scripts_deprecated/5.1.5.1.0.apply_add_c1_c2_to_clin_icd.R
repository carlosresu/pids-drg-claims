apply_add_c1_c2_to_clin_icd <- function(dt) {
  ## Deduplicates ICD codes in clin_icd if already present in c1 or c2
  # dt: input data.table

  add_c1_c2_to_clin_icd <- function(c1, c2, clin_icd) {
    ## Adds c1 and c2 ICD codes to clin_icd, allowing duplicates

    # Create a data.table to handle the merging of codes efficiently
    datatable <- data.table(c1 = c1, c2 = c2, clin_icd = clin_icd)

    # Map function to concatenate clin_icd with c1 and c2, allowing duplicates
    # TODO: Don't duplicate it if it's already there
    datatable[, clin_icd := Map(function(c1, c2, icd) {
      c(icd, c1, c2) # Concatenate clin_icd with c1 and c2
    }, c1, c2, clin_icd)]

    # Return the updated clin_icd column
    return(list(clin_icd = datatable$clin_icd))
  }

  # Add ICD codes from c1 and c2 to clin_icd, removing duplicates
  result <- add_c1_c2_to_clin_icd(dt$c1, dt$c2, dt$clin_icd)

  # Update clin_icd with the deduplicated result
  dt[, clin_icd := result$clin_icd]

  # Return the deduplicated data.table
  return(dt)
}
