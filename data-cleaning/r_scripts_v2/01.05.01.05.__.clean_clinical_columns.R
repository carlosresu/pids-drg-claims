clean_clinical_columns <- function(dt) {
  ## Cleans and processes the clinical columns in a data.table
  # dt: input data.table with clinical columns

  # Deduplicate the ICD codes
  dt <- apply_add_c1_c2_to_clin_icd(dt)

  # Process case rate 1 RVS codes
  c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c1, rvs_icd9
  )
  dt[, clin_rvs := c1_rvs_results$clin_rvs]
  dt[, c1 := c1_rvs_results$col]
  c1_discarded_rvs <- c1_rvs_results$discarded_rvs

  # Process case rate 2 RVS codes
  c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c2, rvs_icd9
  )
  dt[, clin_rvs := c2_rvs_results$clin_rvs]
  dt[, c2 := c2_rvs_results$col]
  c2_discarded_rvs <- c2_rvs_results$discarded_rvs

  return(
    list(
      # Return the cleaned data.table
      dt = dt,
      # Return discarded RVS codes for checks
      discard_rvs_one = c1_discarded_rvs,
      discard_rvs_two = c2_discarded_rvs
    )
  )
}
