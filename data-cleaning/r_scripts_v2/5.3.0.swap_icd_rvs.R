swap_icd_rvs <- function(clin_icd, clin_rvs) {
  # Ensure all input lists are lists of vectors
  datatable <- data.table(clin_icd = clin_icd, clin_rvs = clin_rvs)

  # Step 1: Identify valid RVS codes in clin_icd to move to clin_rvs
  datatable[, rvs_matches := mapply(function(icd_vec, rvs_vec) {
    rvs_codes_in_icd <- Filter(function(code) {
      flag <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = covid_env, ifnotfound = list(NULL))[[1]])
      return(flag)
    }, icd_vec)

    updated_rvs <- c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec])
    deduplicated_rvs <- updated_rvs[!duplicated(updated_rvs)]
    return(deduplicated_rvs)
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]

  # Step 2: Update clin_rvs
  datatable[, clin_rvs := rvs_matches]

  # Step 3: Identify valid ICD codes in clin_rvs to move to clin_icd
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      flag <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code, envir = icd_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = phil_icds_env, ifnotfound = list(NULL))[[1]])
      return(flag)
    }, rvs_vec)

    updated_icd <- c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec])
    deduplicated_icd <- updated_icd[!duplicated(updated_icd)]
    return(deduplicated_icd)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 4: Update clin_icd
  datatable[, clin_icd := icd_matches]

  # Step 5: Remove non-ICD codes from clin_icd
  datatable[, clin_icd := lapply(clin_icd, function(icd_vec) {
    filtered_icd <- Filter(function(code) {
      valid_icd <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code, envir = icd_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = phil_icds_env, ifnotfound = list(NULL))[[1]])
      return(valid_icd)
    }, icd_vec)
    return(filtered_icd)
  })]

  # Step 6: Remove ICD codes from clin_rvs
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    filtered_rvs <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = covid_env, ifnotfound = list(NULL))[[1]])
      return(valid_rvs)
    }, rvs_vec)
    return(filtered_rvs)
  })]

  # Step 7: Assign final results before returning
  ret_list <- list(
    clin_icd = datatable$clin_icd,
    clin_rvs = datatable$clin_rvs
  )

  return(ret_list)
}
