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
