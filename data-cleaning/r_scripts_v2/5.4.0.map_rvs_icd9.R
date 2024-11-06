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
  icd9_list <- lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    # Retrieve ICD-9-CM codes specifically for each code
    mapped_icd9 <- unique(unlist(lapply(codes, function(code) {
      if (code %chin% names(rvs_map_solo)) {
        rvs_map_solo[[code]]
      } else if (code %chin% names(rvs_map_list)) {
        rvs_map_list[[code]]
      } else {
        NULL
      }
    })))
    if (length(mapped_icd9) > 0) mapped_icd9 else NA_character_
  })

  # Collect summary statistics and diagnostics for RVS mapping
  rvss <- unique(unlist(clin_rvs))
  mappable_rvs <- intersect(rvss, rvs$rvs)
  unmappable_rvs <- setdiff(rvss, rvs$rvs)
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))

  # Identify mapped codes with is_drg == FALSE
  mapped_without_drg <- intersect(mappable_rvs, without_drg_codes)

  return_list <- list(
    icd9_list = icd9_list,
    rvs_map_list = rvs_map_list,
    rvss = rvss,
    mappable_rvs = mappable_rvs,
    unmappable_rvs = unmappable_rvs,
    multi_mapped_rvs = multi_mapped_rvs,
    without_drg = mapped_without_drg
  )

  # Return the result as a list
  return(return_list)
}
