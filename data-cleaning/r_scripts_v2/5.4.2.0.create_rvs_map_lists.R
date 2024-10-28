create_rvs_map_lists <- function(with_drg) {
  ## Create two lists for mapping RVS codes to ICD-9-CM codes

  # Order the table by RVS code and whether it's associated with a DRG
  setorder(with_drg, rvs, -is_drg)

  # Group by RVS code and create a list of associated ICD-9-CM codes for each RVS
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Separate RVS codes that map to a single ICD-9-CM code from those with multiple mappings
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  # Create named lists for solo and multi-mapped RVS codes
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Return the solo and multi-mapped lists
  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}
