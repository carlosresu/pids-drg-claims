find_direct_icd_matches <- function(icds, thai_icd10_env) {
  ## Identify ICD-10 codes with exact matches in the Thai ICD-10 library
  # icds: list of ICD-10 codes to be cross-checked
  # thai_icd10_env: environment of Thai ICD-10 codes

  # Retrieve the values of each ICD code from the thai_icd10_env environment.
  # If a code is not found, it returns FALSE (using ifnotfound argument).
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))

  # Extract the names of ICD codes that matched (i.e., returned TRUE from the environment)
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

  # Return the matched ICD codes
  return(direct_match_codes)
}
