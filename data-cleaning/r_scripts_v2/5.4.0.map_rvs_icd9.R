map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
  # Identify RVS codes with is_drg == FALSE for later tracking
  without_drg_codes <- unique(rvs[is_drg == FALSE]$rvs)

  # Order by RVS code, group by RVS code, and list ICD-9-CM codes
  setorder(rvs, rvs)
  unique_rvs <- rvs[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Create mappings for RVS codes: solo and multi-mapped lists
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Map each RVS code in clin_rvs to its specific ICD-9-CM code(s)
  result <- lapply(clin_rvs, function(x) {
    mapped_icd9 <- unique(unlist(lapply(unlist(x), function(code) {
      rvs_map_solo[[code]] %||% rvs_map_list[[code]] %||% NULL
    })))
    if (length(mapped_icd9)) mapped_icd9 else NA_character_
  })

  return(result)
}
