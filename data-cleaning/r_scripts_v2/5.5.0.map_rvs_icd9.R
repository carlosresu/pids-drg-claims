map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
  # Identify RVS codes that are NOT used for DRG (is_drg == FALSE)
  # These may require special handling or exclusion downstream
  without_drg_codes <- unique(rvs[is_drg == FALSE]$rvs)

  # Sort RVS data by RVS code for grouping consistency
  setorder(rvs, rvs)

  # Group by RVS code and create a list of ICD-9-CM codes per RVS
  unique_rvs <- rvs[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Separate single-mapped RVS codes (only one ICD-9-CM)...
  solo <- unique_rvs[lengths(icd9cm_list) == 1]

  # ...from multi-mapped RVS codes (more than one ICD-9-CM)
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  # Create named vectors for lookup: solo mappings (named by RVS code)
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)

  # And for list mappings (also named by RVS code)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Map each RVS vector in clin_rvs to corresponding ICD-9-CM codes
  result <- lapply(clin_rvs, function(x) {
    # For each RVS code in the vector:
    # - Try to find a match in rvs_map_solo
    # - If not found, try rvs_map_list
    # - If still not found, fallback to "_"
    mapped_icd9 <- unique(unlist(lapply(unlist(x), function(code) {
      rvs_map_solo[[code]] %||% rvs_map_list[[code]] %||% "_"
    })))

    # Return unmapped fallback "_" if no codes found
    if (length(mapped_icd9)) unname(mapped_icd9) else "_"
  })

  # Return the list of mapped ICD-9-CM codes per RVS vector
  return(result)
}
