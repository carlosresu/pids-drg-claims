append_copy_remove_icd_rvs <- function(col, clin_rvs, clin_icd) {
  # Ensure all input lists are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)

  # Step 1: Identify valid RVS codes in col (c1, c2) to move to clin_rvs
  datatable[, matches := lapply(col, function(x) {
    # RVS criteria: 5 numeric digits, start with two letters,
    # or in valid RVS codes/covid RVS
    Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
    }, x)
  })]

  # Prepend valid RVS matches from col to the
  # START of clin_rvs (remove duplicates before prepending)
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique_matches <- Filter(function(code) {
      !is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
    }, matches)
    # Remove duplicates and prepend to the start
    updated_rvs <- c(unique_matches[!unique_matches %in% rvs], rvs)
    updated_rvs[!duplicated(updated_rvs)]
  }, clin_rvs, matches, SIMPLIFY = FALSE)]

  # Step 2: Identify valid ICD codes in clin_rvs to move to clin_icd
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    # ICD criteria: not exactly 5 digits,
    # does not start with two letters, in valid ICD or phil_icds
    icd_codes_in_rvs <- Filter(function(code) {
      (!grepl("^[0-9]{5}$", code) &&
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
    }, rvs_vec)
    # Remove duplicates and prepend to the start of clin_icd
    updated_icd <- c(
      icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec], icd_vec
    )
    updated_icd[!duplicated(updated_icd)]
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Update clin_icd with identified valid ICD codes, adding to START of clin_icd
  datatable[, clin_icd := icd_matches]

  # Step 3: Remove RVS codes from col only if they don’t belong there
  datatable[, col := lapply(col, function(x) {
    # Keep in col only those codes that do not meet RVS criteria
    Filter(function(code) {
      is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
    }, x)
  })]

  # Step 4: Remove ICD codes from clin_rvs only if they don’t belong there
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
    }, rvs_vec)
  })]

  # Step 5: Append RVS codes from clin_icd to the
  # END of clin_rvs without removing existing duplicates
  datatable[, clin_rvs := mapply(function(rvs_vec, icd_vec) {
    # Extract RVS codes from clin_icd based on criteria
    rvs_codes_in_icd <- Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
    }, icd_vec)
    # Append unique RVS codes to the END of clin_rvs
    c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec])
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]

  # Step 6: Append ICD codes from clin_rvs to the
  # END of clin_icd without removing existing duplicates
  datatable[, clin_icd := mapply(function(icd_vec, rvs_vec) {
    # Extract ICD codes from clin_rvs based on criteria
    icd_codes_in_rvs <- Filter(function(code) {
      (!grepl("^[0-9]{5}$", code) &&
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
    }, rvs_vec)
    # Append unique ICD codes to the END of clin_icd
    c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec])
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]

  # Step 7: Recursively unlist elements in col
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA_character_)
    } else {
      return(unlist(x, recursive = TRUE, use.names = FALSE))
    }
  })]

  # Return updated clin_rvs, clin_icd, and cleaned col
  return(list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col
  ))
}
