prep_pdx_inputs <- function(
    # inputs to process
    c1_arg, c2_arg, clin_sdx_arg,
    # dependencies
    accpdx = acc_pdx
    # , neoplasmsdtactual = neoplasms_dt_actual,
    # acrrvs = acr_rvs, covidrvs = covid_rvs
    ) {
  # prepare dependencies (ensure uniqueness, extract relevant column)
  acc_pdx_set_final <- unique(accpdx)
  # neoplasm_codes_final <- unique(neoplasmsdtactual$icd10)
  # rvs_codes_final <- unique(acrrvs$rvs)
  # covidrvsfinal <- unique(covidrvs)

  # Assumes existing definition of remove_whitespace and filter_icds
  # Remove whitespace in preparation for clin_pdx finding
  # c1_temp <- lapply(c1_arg, function(x) {
  #   split <- safe_split(remove_whitespace(x))
  #   return(split)
  # })
  # c2_temp <- lapply(c2_arg, function(x) {
  #   split <- safe_split(remove_whitespace(x))
  #   return(split)
  # })
  # clin_sdx_temp <- lapply(clin_sdx_arg, function(x) {
  #   split <- safe_split(remove_whitespace(x))
  #   return(split)
  # })

  # Filter ICD codes to remove invalid candidates
  c1_final <- lapply(
    c1_arg, filter_icds,
    # neoplasm_codes_final, covidrvsfinal,
    acc_pdx_set_final
  )
  c2_final <- lapply(
    c2_arg, filter_icds,
    # neoplasm_codes_final, covidrvsfinal,
    acc_pdx_set_final
  )
  clin_sdx_final <- lapply(
    clin_sdx_arg, filter_icds,
    # neoplasm_codes_final, covidrvsfinal,
    acc_pdx_set_final
  )

  # prepared outputs (input to find_pdx)
  ret_list <- list(c1 = c1_final, c2 = c2_final, clin_sdx = clin_sdx_final)
  return(ret_list)
}
