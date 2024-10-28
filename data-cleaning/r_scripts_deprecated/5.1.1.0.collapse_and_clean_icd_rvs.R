collapse_and_clean_icd_rvs <- function(dt) {
  ## Collapses and cleans ICD and RVS columns in a data.table
  # dt: input data.table containing ICD and RVS columns
  available_columns <- colnames(dt)

  # Dynamically detect which clin_icd columns exist
  icd_cols <- grep("^clin_icd\\d+$", available_columns, value = TRUE)
  # print(icd_cols)
  if (length(icd_cols) > 0) {
    # Collapse the ICD codes,
    # whether from multiple columns or a single column
    dt[, clin_icd := collapse_columns(mget(icd_cols))]
    # Remove the individual columns after collapsing
    dt[, (icd_cols) := NULL]
  }

  # Dynamically detect which clin_rvs columns exist
  rvs_cols <- grep("^clin_rvs\\d+$", available_columns, value = TRUE)
  if (length(rvs_cols) > 0) {
    # Collapse the RVS codes, whether
    # from multiple columns or a single column
    dt[, clin_rvs := collapse_columns(mget(rvs_cols))]
    # Remove the individual columns after collapsing
    dt[, (rvs_cols) := NULL]
  }

  # Handle any lumped ICD codes by splitting them if clin_icd exists
  if ("clin_icd" %in% available_columns) {
    # Apply cleaning for lumped codes
    dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]
    # Convert cleaned string to vector
    dt[, clin_icd := split_to_vector(clin_icd)]
  }

  # Handle any lumped RVS codes by splitting them if clin_rvs exists
  if ("clin_rvs" %in% available_columns) {
    # Convert cleaned string to vector
    dt[, clin_rvs := split_to_vector(clin_rvs)]
  }

  # Return the cleaned data.table
  return(dt)
}
