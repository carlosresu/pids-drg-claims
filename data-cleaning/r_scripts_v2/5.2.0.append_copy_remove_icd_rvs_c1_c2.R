append_copy_remove_icd_rvs_c1_c2 <- function(col, clin_rvs, clin_icd) {
  # Ensure all input lists are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)

  # Step 1: Identify valid RVS codes in col (c1, c2) to move to clin_rvs
  datatable[, matches := lapply(col, function(x) {
    valid_rvs_codes <- Filter(function(code) {
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
    }, x)
    return(valid_rvs_codes)
  })]

  # Step 2: Prepend valid RVS matches from col to
  # clin_rvs (remove duplicates before prepending)
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique_matches <- Filter(function(code) {
      valid_rvs <- !is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
      return(valid_rvs)
    }, matches)

    updated_rvs <- c(unique_matches[!unique_matches %in% rvs], rvs)
    deduplicated_rvs <- updated_rvs[!duplicated(updated_rvs)]
    return(deduplicated_rvs)
  }, clin_rvs, matches, SIMPLIFY = FALSE)]

  # Step 3: Identify valid ICD codes in clin_rvs to move to clin_icd
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      flag <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
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

    updated_icd <- c(icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec], icd_vec)
    deduplicated_icd <- updated_icd[!duplicated(updated_icd)]
    return(deduplicated_icd)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 4: Update clin_icd
  datatable[, clin_icd := icd_matches]

  # Step 5: Remove RVS codes from col only if they don’t belong there
  datatable[, col := lapply(col, function(x) {
    filtered_col <- Filter(function(code) {
      invalid_rvs <- is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
      return(invalid_rvs)
    }, x)
    return(filtered_col)
  })]

  # Step 6: Remove ICD codes from clin_rvs only if they don’t belong there
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    filtered_rvs <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
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

  # Step 7: Append RVS codes from clin_icd to clin_rvs
  datatable[, clin_rvs := mapply(function(rvs_vec, icd_vec) {
    rvs_codes_in_icd <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_rvs)
    }, icd_vec)

    updated_rvs <- c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec])
    return(updated_rvs)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 8: Append ICD codes from clin_rvs to clin_icd
  datatable[, clin_icd := mapply(function(icd_vec, rvs_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      valid_icd <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
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
    }, rvs_vec)

    updated_icd <- c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec])
    return(updated_icd)
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]

  # Step 9: Recursively unlist elements in col
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      processed_col <- character(0)
    } else {
      processed_col <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    return(processed_col)
  })]

  # Step 10: Assign final results before returning
  ret_list <- list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col
  )

  return(ret_list)
}
