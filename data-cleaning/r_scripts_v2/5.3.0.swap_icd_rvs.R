swap_icd_rvs <- function(clin_icd, clin_rvs) {
  # Wrap input vectors into a data.table for row-wise operations
  datatable <- data.table(clin_icd = clin_icd, clin_rvs = clin_rvs)

  # Step 1: Identify RVS-like codes mistakenly placed in clin_icd and
  # move to clin_rvs
  datatable[, rvs_matches := mapply(function(icd_vec, rvs_vec) {
    rvs_codes_in_icd <- Filter(function(code) {
      # A code is considered an RVS if:
      # - it's 5 digits starting with a number
      # - it has 2 capital letters (like "XR", "DR")
      # - it exists in RVS or COVID environments
      flag <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(flag)
    }, icd_vec)

    # Add valid RVS codes to existing RVS vector, remove duplicates
    updated_rvs <- c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec])
    deduplicated_rvs <- updated_rvs[!duplicated(updated_rvs)]
    return(deduplicated_rvs)
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]

  # Step 2: Update clin_rvs with the corrected values
  datatable[, clin_rvs := rvs_matches]

  # Step 3: Identify ICD-like codes mistakenly placed in clin_rvs and
  # move to clin_icd
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      # A code is considered an ICD if:
      # - it's not a 5-digit purely numeric RVS
      # - it's not a 2-letter abbreviation
      # - it's not a neoplasm-style slash code
      # - or it's found in ICD dictionaries
      flag <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}$", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code,
          envir = icd_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = phil_icds_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(flag)
    }, rvs_vec)

    # Add ICD-like codes to existing ICD vector, remove duplicates
    updated_icd <- c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec])
    deduplicated_icd <- updated_icd[!duplicated(updated_icd)]
    return(deduplicated_icd)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 4: Update clin_icd with the corrected values
  datatable[, clin_icd := icd_matches]

  # Step 5: Clean clin_icd by removing anything that doesn't resemble
  # a valid ICD
  datatable[, clin_icd := lapply(clin_icd, function(icd_vec) {
    filtered_icd <- Filter(function(code) {
      valid_icd <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}$", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code,
          envir = icd_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = phil_icds_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_icd)
    }, icd_vec)
    return(filtered_icd)
  })]

  # Step 6: Clean clin_rvs by removing anything that doesn't resemble
  # a valid RVS
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    filtered_rvs <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}$", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_rvs)
    }, rvs_vec)
    return(filtered_rvs)
  })]

  # Step 7: Return updated vectors as a named list
  ret_list <- list(
    clin_icd = datatable$clin_icd,
    clin_rvs = datatable$clin_rvs
  )

  return(ret_list)
}
