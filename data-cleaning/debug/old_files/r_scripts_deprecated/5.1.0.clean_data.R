clean_data <- function(dt) {
  # Rename columns based on column_mappings
  available_columns <- colnames(dt)

  # Convert source year to integer if needed
  if (!"ADMISSION_YEAR" %in% available_columns) {
    dt[, SRC_YR := as.integer(year_to_load)]
  }

  dt[, is_covid := FALSE]

  # Identify the columns to rename and apply renaming
  old_names <- available_columns[available_columns %in% names(column_mappings)]
  new_names <- sapply(old_names, function(col) column_mappings[[col]])
  setnames(dt, old = old_names, new = new_names)
  rename_success <- all(new_names %in% colnames(dt))

  dt[, id_series := trimws(id_series)]
  dt[, c1 := clin_c1]
  dt[, c2 := clin_c2]

  # Convert time columns
  dt[, time_adm := ifelse(
    grepl("AM|PM", time_adm),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_adm),
      format = "%m/%d/%Y %I:%M:%S %p"
    ), "%H:%M"),
    time_adm
  )]
  dt[, time_dis := ifelse(
    grepl("AM|PM", time_dis),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_dis),
      format = "%m/%d/%Y %I:%M:%S %p"
    ), "%H:%M"),
    time_dis
  )]

  # Identify ICD and RVS columns
  icd_col_names <- grep("^clin_icd\\d+$", names(dt), value = TRUE)
  rvs_col_names <- grep("^clin_rvs\\d+$", names(dt), value = TRUE)
  clin_icd_cols <- lapply(icd_col_names, function(col_name) dt[[col_name]])
  clin_rvs_cols <- lapply(rvs_col_names, function(col_name) dt[[col_name]])

  # Call the collapse and clean function
  col_list <- collapse_and_clean_icd_rvs_cols(clin_icd_cols, clin_rvs_cols)
  dt[, clin_icd := col_list$clin_icd]
  dt[, clin_rvs := col_list$clin_rvs]
  dt[, (icd_col_names) := NULL]
  dt[, (rvs_col_names) := NULL]

  # Clean and compare c1 and c2 columns
  c1_result <- clean_column(dt$c1)
  dt[, c1_orig := c1]
  dt[, c1 := c1_result$cleaned_col]
  is_covid_c1 <- c1_result$is_covid

  c1_cleaning_comparison <- data.table(
    old_code = sapply(dt$c1_orig, toString),
    new_code = sapply(dt$c1, toString)
  )[old_code != new_code,
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]

  c2_result <- clean_column(dt$c2)
  dt[, c2_orig := c2]
  dt[, c2 := c2_result$cleaned_col]
  is_covid_c2 <- c2_result$is_covid

  c2_cleaning_comparison <- data.table(
    old_code = sapply(dt$c2_orig, toString),
    new_code = sapply(dt$c2, toString)
  )[old_code != new_code,
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]

  dt[, is_covid := ifelse(
    is_covid == FALSE & (is_covid_c1 == TRUE | is_covid_c2 == TRUE), TRUE, is_covid
  )]

  # Apply manual replacements to columns
  dt[, clin_icd := lapply(clin_icd, function(text) {
    stri_replace_all_regex(
      text, manual_patterns_to_replace, manual_code_replacements,
      vectorize_all = FALSE
    )
  })]
  dt[, c1 := lapply(c1, function(text) {
    stri_replace_all_regex(
      text, manual_patterns_to_replace, manual_code_replacements,
      vectorize_all = FALSE
    )
  })]
  dt[, c2 := lapply(c2, function(text) {
    stri_replace_all_regex(
      text, manual_patterns_to_replace, manual_code_replacements,
      vectorize_all = FALSE
    )
  })]

  dt[, c1 := split_to_vector(remove_lumped_icd_codes(c1))]
  dt[, c2 := split_to_vector(remove_lumped_icd_codes(c2))]

  # Concatenate clin_icd with c1 and c2
  dt[, clin_icd := Map(function(c1, c2, icd) {
    c(icd, c1, c2)
  }, dt$c1, dt$c2, dt$clin_icd)]

  # Process RVS codes
  c1_results <- append_and_remove_rvs(dt$clin_rvs, dt$c1, rvs_icd9$rvs)
  dt[, clin_rvs := c1_results$clin_rvs]
  dt[, c1 := c1_results$col]
  c1_discarded_rvs <- c1_results$discarded_rvs

  c2_results <- append_and_remove_rvs(dt$clin_rvs, dt$c2, rvs_icd9$rvs)
  dt[, clin_rvs := c2_results$clin_rvs]
  dt[, c2 := c2_results$col]
  c2_discarded_rvs <- c2_results$discarded_rvs

  # Replace empty strings with NA and remap patient data
  replace_result <- replace_empty_with_na(dt)
  dt <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  # Call the updated remap_patient_data function
  remap_res <- remap_patient_data(
    pat_type = dt$pat_type,
    pat_memcat_parent = dt$pat_memcat_parent,
    pat_memcat_child = dt$pat_memcat_child,
    clin_discharge = dt$clin_discharge,
    claim_status = dt$claim_status,
    known_values = known_values
  )

  # Assign remapped columns back to dt
  dt[, pat_type := remap_res$remapped_columns$pat_type]
  dt[, pat_memcat_parent := remap_res$remapped_columns$pat_memcat_parent]
  dt[, pat_memcat_child := remap_res$remapped_columns$pat_memcat_child]
  dt[, clin_discharge := remap_res$remapped_columns$clin_discharge]
  dt[, claim_status := remap_res$remapped_columns$claim_status]

  return(
    list(
      return_data = dt,
      return_summary = list(
        rename_success = rename_success,
        ICD_replacements_1 = c1_cleaning_comparison,
        ICD_replacements_2 = c2_cleaning_comparison,
        pat_type_mapped = remap_res$mapped$pat_type,
        pat_memcat_parent_mapped = remap_res$mapped$pat_memcat_parent,
        pat_memcat_child_mapped = remap_res$mapped$pat_memcat_child,
        clin_discharge_mapped = remap_res$mapped$clin_discharge,
        claim_status_mapped = remap_res$mapped$claim_status,
        pat_type_unmapped = remap_res$unmapped$pat_type,
        memcat_parent_unmapped = remap_res$unmapped$pat_memcat_parent,
        memcat_child_unmapped = remap_res$unmapped$pat_memcat_child,
        discharge_unmapped = remap_res$unmapped$clin_discharge,
        claim_status_unmapped = remap_res$unmapped$claim_status,
        discard_rvs_one = c1_discarded_rvs,
        discard_rvs_two = c2_discarded_rvs,
        empty_strings_replaced_1 = empty_strings_replaced_1
      )
    )
  )
}
