prep_pdx_inputs <- function(
    # inputs to process
    c1_orig, c2_orig, clin_icd_orig,
    # dependencies
    accpdx = acc_pdx, neoplasmsdtactual = neoplasms_dt_actual,
    acrrvs = acr_rvs, covidrvs = covid_rvs) {
  # prepare dependencies (ensure uniqueness, extract relevant column)
  acc_pdx_set_final <- unique(accpdx)
  neoplasm_codes_final <- unique(neoplasmsdtactual$icd10)
  rvs_codes_final <- unique(acrrvs$rvs)
  covidrvsfinal <- unique(covidrvs)

  # Assumes existing definition of remove_whitespace and filter_icds
  # Remove whitespace in preparation for pdx finding
  c1_temp <- lapply(c1_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })
  c2_temp <- lapply(c2_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })
  clin_icd_temp <- lapply(clin_icd_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })

  # Filter ICD codes to remove invalid candidates
  c1_final <- lapply(
    c1_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )
  c2_final <- lapply(
    c2_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )
  clin_icd_final <- lapply(
    clin_icd_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )

  # prepared outputs (input to find_pdx)
  ret_list <- list(c1 = c1_final, c2 = c2_final, clin_icd = clin_icd_final)
  return(ret_list)
}
