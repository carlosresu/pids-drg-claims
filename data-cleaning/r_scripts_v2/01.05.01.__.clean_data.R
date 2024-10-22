clean_data <- function(dt) {
  # Rename columns based on column_mappings
  available_columns <- colnames(dt)

  # Convert source year to integer
  if ("ADMISSION_YEAR" %in% available_columns) {
    # do nothing
  } else {
    dt[, SRC_YR := as.integer(year_to_load)]
  }

  dt[, is_covid := FALSE]

  # Identify the columns to rename based on the mapping
  old_names <- available_columns[
    available_columns %in% names(column_mappings)
  ]
  new_names <- sapply(old_names, function(col) column_mappings[[col]])

  # Rename the columns in the data.table
  setnames(dt, old = old_names, new = new_names)

  # Check if renaming was successful
  rename_success <- all(new_names %in% available_columns)

  dt[, id_series := trimws(id_series)]

  dt[, c1 := clin_c1]
  dt[, c2 := clin_c2]

  # Convert time_adm and time_dis for 2022-2023 format
  # Strip fractional seconds and handle AM/PM
  # conversion properly using as.POSIXct()
  dt[, time_adm := ifelse(
    grepl("AM|PM", time_adm),
    format(
      as.POSIXct(sub("\\.\\d+ ", " ", time_adm),
        format = "%m/%d/%Y %I:%M:%S %p"
      ), "%H:%M"
    ),
    time_adm
  )]

  dt[, time_dis := ifelse(
    grepl("AM|PM", time_dis),
    format(
      as.POSIXct(sub("\\.\\d+ ", " ", time_dis),
        format = "%m/%d/%Y %I:%M:%S %p"
      ), "%H:%M"
    ),
    time_dis
  )]

  # Collapse and clean ICD and RVS columns
  # See function(s) above
  dt <- collapse_and_clean_icd_rvs(dt)

  # Clean and compare c1 and c2 columns
  # Apply the cleaning function to dt$c1
  # Process c1 and update dt
  c1_result <- clean_clinical_column(dt$c1)
  dt[, c1_orig := c1_result$original_col] # Store original c1 column
  dt[, c1 := c1_result$cleaned_col] # Update c1 with cleaned column
  is_covid_c1 <- c1_result$is_covid # Extract is_covid flag from c1 processing

  # Process c2 and update dt
  c2_result <- clean_clinical_column(dt$c2)
  dt[, c2_orig := c2_result$original_col] # Store original c2 column
  dt[, c2 := c2_result$cleaned_col] # Update c2 with cleaned column
  is_covid_c2 <- c2_result$is_covid # Extract is_covid flag from c2 processing

  # Update the is_covid flag: Set to TRUE if either c1 or c2 indicates COVID
  dt[, is_covid := ifelse(
    is_covid == FALSE & (is_covid_c1 == TRUE | is_covid_c2 == TRUE),
    TRUE,
    is_covid
  )]

  c1_cleaning_comparison <- c1_result$comparison
  c2_cleaning_comparison <- c2_result$comparison

  # Apply multi-replacement function to implement manual replacements
  dt[, clin_icd := lapply(
    clin_icd,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, c1 := lapply(
    c1,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, c2 := lapply(
    c2,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]

  # See function(s) above
  # Handle any lumped ICD codes by splitting them
  dt[, c1 := remove_lumped_icd_codes(c1)]
  # Handle any lumped ICD codes by splitting them
  dt[, c2 := remove_lumped_icd_codes(c2)]

  # See function(s) above
  # Convert the cleaned columns into vectors
  dt[, c1 := split_to_vector(c1)]
  # Convert the cleaned columns into vectors
  dt[, c2 := split_to_vector(c2)]

  # See function(s) above
  # Clean clinical columns
  clean_clin_col_res <- clean_clinical_columns(dt)
  dt <- clean_clin_col_res$dt

  # Replace empty strings with NA and remap patient data
  # See cleaning-functions.R
  replace_result <- replace_empty_with_na(dt)
  dt <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  # See function(s) above
  remap_res <- remap_patient_data(dt)

  dt <- remap_res$data

  return(
    list(
      # data to return for further processing
      return_data = dt,
      # summary to return for checks and output
      return_summary = list(
        rename_success = rename_success,
        ICD_replacements_1 = c1_cleaning_comparison,
        ICD_replacements_2 = c2_cleaning_comparison,
        pat_type_mapped = remap_res$pat_type_mapped,
        pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
        pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
        clin_discharge_mapped = remap_res$clin_discharge_mapped,
        claim_status_mapped = remap_res$claim_status_mapped,
        pat_type_unmapped = remap_res$pat_type_unmapped,
        memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
        memcat_child_unmapped = remap_res$memcat_child_unmapped,
        discharge_unmapped = remap_res$discharge_unmapped,
        claim_status_unmapped = remap_res$claim_status_unmapped,
        discard_rvs_one = clean_clin_col_res$discard_rvs_one,
        discard_rvs_two = clean_clin_col_res$discard_rvs_two,
        empty_strings_replaced_1 = empty_strings_replaced_1
      )
    )
  )
}
