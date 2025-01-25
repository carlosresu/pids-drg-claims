prep_pdx_inputs <- function(
    # inputs to process
    c1_orig, c2_orig, clin_icd_orig,
    # dependencies
    accpdx = acc_pdx, neoplasmsdtactual = neoplasms_dt_actual,
    acrrvs = acr_rvs, covidrvs = covid_rvs) {
  # Assumes existing definition of remove_whitespace and filter_icds
  # Remove whitespace in preparation for pdx finding
  c1_temp <- lapply(c1_orig, remove_whitespace)
  c2_temp <- lapply(c2_orig, remove_whitespace)
  clin_icd_temp <- lapply(clin_icd_orig, remove_whitespace)

  # Check for NA values in c1_temp
  if (any(sapply(c1_temp, is.na))) stop("NA values detected in c1_temp")

  # split, unlist, and then filter icd codes
  c1_final <- filter_icds(unlist(strsplit(c1_temp, "\\|")))
  c2_final <- filter_icds(unlist(strsplit(c2_temp, "\\|")))
  clin_icd_final <- filter_icds(unlist(strsplit(clin_icd_temp, "\\|")))

  # prepare dependencies (ensure uniqueness, extract relevant column)
  acc_pdx_set_final <- unique(accpdx)
  neoplasm_codes_final <- unique(neoplasmsdtactual$icd10)
  rvs_codes_final <- unique(acrrvs$rvs)
  covidrvsfinal <- unique(covidrvs)

  # prepare return list
  pdx_inputs <- list(
    # prepared outputs (input to find_pdx)
    c1 = c1_final, c2 = c2_final, clin_icd = clin_icd_final,
    # prepared outputs (dependencies of find_pdx)
    acc_pdx = acc_pdx_set_final, neoplasm_codes = neoplasm_codes_final,
    rvs_codes = rvs_codes_final, covid_rvs = covidrvsfinal
  )

  # return prepared list
  return(pdx_inputs)
}
