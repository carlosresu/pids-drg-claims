append_copy_remove_icd_rvs_c1_c2 <- function(col, clin_rvs, clin_icd) {
  # Combine input vectors into a working data.table for row-wise operations
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)

  # Step 1: From `col`, extract codes that resemble RVS
  # (or known COVID/RVS codes)
  datatable[, matches := lapply(col, function(x) {
    valid_rvs_codes <- Filter(function(code) {
      # 5-digit code starting with a digit
      flag <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) || # code with 2 uppercase letters (likely RVS)
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) || # in RVS env
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]]) # in COVID env
      return(flag)
    }, x)
    return(valid_rvs_codes)
  })]

  # Step 2: Append matched RVS codes to `clin_rvs` if not already there
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique_matches <- Filter(function(code) {
      !is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]]) # keep only confirmed RVS codes
    }, matches)

    # Prepend new codes, ensure no duplicates
    updated_rvs <- c(unique_matches[!unique_matches %in% rvs], rvs)
    deduplicated_rvs <- updated_rvs[!duplicated(updated_rvs)]
    return(deduplicated_rvs)
  }, clin_rvs, matches, SIMPLIFY = FALSE)]

  # Step 3: Extract ICD-like codes mistakenly in `clin_rvs` and
  # prepare to move to `clin_icd`
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      flag <- (!grepl("^[0-9]{5}$", code) && # not a 5-digit RVS
        !grepl("^[A-Z]{2}", code) && # not a 2-letter RVS
        !grepl("/", code)) || # not a neoplasm-style slash
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

    # Merge into clin_icd, de-duplicated
    updated_icd <- c(icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec], icd_vec)
    deduplicated_icd <- updated_icd[!duplicated(updated_icd)]
    return(deduplicated_icd)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 4: Apply new `clin_icd` with corrected ICD inclusions
  datatable[, clin_icd := icd_matches]

  # Step 5: Remove RVS codes from `col` that should be in `clin_rvs` instead
  datatable[, col := lapply(col, function(x) {
    filtered_col <- Filter(function(code) {
      is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]]) # retain only if not known RVS
    }, x)
    return(filtered_col)
  })]

  # Step 6: Ensure `clin_rvs` contains only valid RVS or COVID codes
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    filtered_rvs <- Filter(function(code) {
      # 5-digit numeric
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) || # 2-letter
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

  # Step 7: Move any remaining RVS-looking codes from `clin_icd`
  # back to `clin_rvs`
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

  # Step 8: Move any remaining ICD-looking codes from `clin_rvs`
  # back to `clin_icd`
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

  # Step 9: Flatten and clean `col`, ensuring it's a plain character vector
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      processed_col <- character(0)
    } else {
      processed_col <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    return(processed_col)
  })]

  # Step 10: Return updated values as list
  ret_list <- list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col
  )

  return(ret_list)
}
