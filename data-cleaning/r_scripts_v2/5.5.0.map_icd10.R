map_icd10 <- function(col) {
  # Collect and pre-filter unique ICD codes,
  # excluding those in covid_rvs_neoplasm_env
  icds <- unique(unlist(col))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !vapply(icds, function(code) {
      exists(x = code, envir = covid_rvs_neoplasm_env, inherits = FALSE)
    }, logical(1))]

  # Initialize the mapping list
  icd_mapping <- list()

  # Process each unique filtered ICD code
  for (code in filtered_icds) {
    code <- trimws(code) # Trim whitespace

    # 1. **Exact match check**
    if (exists(x = code, envir = icd_codes_env, inherits = FALSE)) {
      icd_mapping[[code]] <- code
      next
    }

    # 2. **Attempt adding '9' for 3-character codes**
    if (nchar(code) == 3) {
      modified_code <- paste0(code, "9")
      if (exists(x = modified_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- modified_code
        next
      }
    }

    # 3. **Trimming codes longer than 4 characters**
    trimmed_code <- if (nchar(code) > 4) {
      sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
    } else if (nchar(code) == 4) {
      code
    } else {
      NULL
    }

    if (!is.null(trimmed_code)) {
      if (exists(x = trimmed_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_code
        next
      }
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (exists(x = trimmed_to_3, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_to_3
        next
      }
    }

    # 4. **Mark as unmatched if all else fails**
    icd_mapping[[code]] <- NA_character_
  }

  # Return mapped ICD codes while ensuring output remains a list of vectors
  ret <- lapply(col, function(codes) {
    unname(lapply(codes, function(code) {
      if (!is.null(icd_mapping[[code]]) && !is.na(icd_mapping[[code]])) {
        return(icd_mapping[[code]])
      } else {
        return(code) # Keep original code if no mapping found
      }
    }))
  })

  return(ret) # Always return a list of vectors
}
