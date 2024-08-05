clean_data <- function(dt) {
  #' @title Clean and preprocess data
  #' @description This function cleans and preprocesses
  #' the given data.table by performing tasks like renaming
  #' columns, collapsing and cleaning ICD and RVS columns,
  #' replacing empty strings, and remapping patient data.
  #' @param dt data.table. The data table to be cleaned.
  #' @return list. A list containing the cleaned data and
  #' various summaries.

  # Convert source year to integer
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if renaming was successful
  rename_success <- all(new_colnames %in% colnames(dt))

  # Collapse and clean ICD and RVS columns
  dt <- collapse_and_clean_icd_rvs(dt)

  # Clean clin_c1 column
  dt[, clin_c1_orig := dt$clin_c1]
  dt[, clin_c1 := clean_column(clin_c1, na_like_strings)]
  dt[, clin_c1_orig := sapply(clin_c1_orig, toString)]
  dt[, clin_c1 := sapply(clin_c1, toString)]

  # Compare cleaning results for clin_c1
  clin_c1_cleaning_comparison <- dt[
    !is.na(clin_c1_orig) & clin_c1 != clin_c1_orig,
    .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
    by = .(clin_c1_orig, clin_c1)
  ]

  # Clean clin_c2 column
  dt[, clin_c2_orig := dt$clin_c2]
  dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]
  dt[, clin_c2_orig := sapply(clin_c2_orig, toString)]
  dt[, clin_c2 := sapply(clin_c2, toString)]

  # Compare cleaning results for clin_c2
  clin_c2_cleaning_comparison <- dt[
    !is.na(clin_c2_orig) & clin_c2 != clin_c2_orig,
    .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
    by = .(clin_c2_orig, clin_c2)
  ]

  manual_multi_replace <- function(code, replacements) {
    # Iterate over each pattern and its corresponding replacement in the list
    for (pattern in names(replacements)) {
      replacement <- replacements[[pattern]]
      code <- gsub(paste0("\\b", pattern, "\\b"), replacement, code)
    }
    return(code)
  }

  # Apply the multi-replacement function using the named list
  dt[, clin_icd := lapply(clin_icd, manual_multi_replace, replacements = manual_code_replacements)]
  dt[, clin_c1 := lapply(clin_c1, manual_multi_replace, replacements = manual_code_replacements)]
  dt[, clin_c2 := lapply(clin_c2, manual_multi_replace, replacements = manual_code_replacements)]

  # Remove lumped ICD codes
  dt[, clin_c1 := remove_lumped_icd_codes(clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(clin_c2)]

  # Clean clinical columns
  clean_clin_col_res <- clean_clinical_columns(dt)
  dt <- clean_clin_col_res$dt
  discard_rvs_one <- clean_clin_col_res$discard_rvs_one
  discard_rvs_two <- clean_clin_col_res$discard_rvs_two

  # Replace empty strings with NA
  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  empty_strings_replaced_1 <- replace_result$replacement_summary

  # Remap patient data
  remapping_results <- remap_patient_data(dt, to_view_checks)
  dt <- remapping_results$data

  # Return the cleaned data and summaries
  return(list(
    data = dt,
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_unmapped = remapping_results$pat_type_unmapped,
    memcat_parent_unmapped = remapping_results$memcat_parent_unmapped,
    memcat_child_unmapped = remapping_results$memcat_child_unmapped,
    discharge_unmapped = remapping_results$discharge_unmapped,
    claim_status_unmapped = remapping_results$claim_status_unmapped,
    discard_rvs_one = discard_rvs_one,
    discard_rvs_two = discard_rvs_two,
    empty_strings_replaced_1 = empty_strings_replaced_1
  ))
}


collapse_and_clean_icd_rvs <- function(dt) {
  #' @title Collapse and clean ICD and RVS columns
  #' @description This function collapses and cleans the ICD
  #' and RVS columns in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dt[, clin_icd := collapse_columns(mget(paste0("clin_icd", 1:12)), na_like_strings)]
  dt[, paste0("clin_icd", 1:12) := NULL]
  dt[, clin_rvs := collapse_columns(mget(paste0("clin_rvs", 1:20)), na_like_strings)]
  dt[, paste0("clin_rvs", 1:20) := NULL]
  dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]
  return(dt)
}

clean_clinical_columns <- function(dt) {
  #' @title Clean clinical columns
  #' @description This function cleans the clinical columns
  #' in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.

  dt <- transfer_icd_codes(dt)
  dt <- deduplicate_icd_codes(dt)

  clin_c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c1, rvs_icd9
  )
  dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
  dt[, clin_c1 := clin_c1_rvs_results$col]
  clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

  # cat(clin_c1_discarded_rvs)

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]
  clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

  # cat(clin_c2_discarded_rvs)

  dt[, clin_rvs := lapply(clin_rvs, unique)]

  return_list <- list(
    dt = dt,
    discard_rvs_one = clin_c1_discarded_rvs,
    discard_rvs_two = clin_c2_discarded_rvs
  )

  # str(return_list)

  return(return_list)
}

transfer_icd_codes <- function(dt) {
  #' @title Transfer ICD codes
  #' @description This function transfers extra ICD-10 codes
  #' to clinical ICD in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dt[, clin_c1 := split_to_vector(clin_c1)]
  clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c1
  )
  dt[, clin_icd := clin_c1_result$clin_icd]
  dt[, clin_c1 := clin_c1_result$col_first]

  dt[, clin_c2 := split_to_vector(clin_c2)]
  clin_c2_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c2
  )
  dt[, clin_icd := clin_c2_result$clin_icd]
  dt[, clin_c2 := clin_c2_result$col_first]

  return(dt)
}

deduplicate_icd_codes <- function(dt) {
  #' @title Deduplicate ICD codes
  #' @description This function ensures unique ICD codes
  #' within and across clinical columns in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dedup_result <- ensure_unique_icd_codes(dt$clin_c1, dt$clin_c2, dt$clin_icd)
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]
  return(dt)
}

remap_patient_data <- function(dt, to_view_checks) {
  #' @title Remap patient data
  #' @description This function remaps patient data such as
  #' patient type, member category, and discharge disposition
  #' in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @param to_view_checks logical. Whether to view checks.
  #' @return list. A list containing the processed data and summaries.

  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL
  claim_status_unmap <- NULL


  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    pat_unmap <- result$unmapped
  }

  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    parent_unmap <- result$unmapped
  }

  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    child_unmap <- result$unmapped
  }

  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    discharge_unmap <- result$unmapped
  }

  result <- remap_claim_status(dt$claim_status)
  dt$claim_status <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    claim_status_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    claim_status_unmapped = claim_status_unmap
  ))
}
