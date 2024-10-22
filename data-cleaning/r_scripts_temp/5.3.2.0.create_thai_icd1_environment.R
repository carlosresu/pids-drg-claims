create_thai_icd10_environment <- function(thai_icd10_codes) {
  ## Create environment for Thai ICD-10 codes

  # Create a new environment where the Thai ICD-10 codes are set to TRUE
  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )

  # Return the created environment
  return(thai_icd10_env)
}
