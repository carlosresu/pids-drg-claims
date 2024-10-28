split_rvs_codes <- function(rvs_icd9) {
  ## Split the RVS codes into those with DRG and those without

  # Filter rows where the RVS code has an associated DRG
  with_drg <- rvs_icd9[is_drg == TRUE]

  # Filter rows where the RVS code does not have a DRG
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]

  # Return the lists of RVS codes with and without DRG
  return(list(with_drg = with_drg, without_drg = without_drg))
}
