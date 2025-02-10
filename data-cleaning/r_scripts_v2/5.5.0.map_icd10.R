# Updated map_icd10 function using covid_rvs_neoplasm_env
map_icd10 <- function(col) {
  # This function processes a single column of ICD-10 codes, taken in as a
  # list column of character vectors, and maps them to equivalent Thai ICD-10
  # codes. It checks for exact matches, or modifies them (appending a 9 or
  # trimming) and seeing if they then match. It otherwise returns NA.

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

  # Initialize things
  icd_mapping <- list() # Mapping to store results

  # Loop through every element of ICDs that are filtered
  for (code in filtered_icds) {
    # 0. **Trim whitespace for ALL codes**
    code <- trimws(code)

    # 1. **Exact match check**
    # If code exists directly/exactly,
    if (exists(x = code, envir = icd_codes_env, inherits = FALSE)) {
      # Add that code to the mapping
      icd_mapping[[code]] <- code

      # Skip further processing for this code
      next
    }

    # 2. **Attempt adding '9' for 3-character codes, then check for existence**
    # If code is 3 characters long, e.g., J18
    if (nchar(code) == 3) {
      # First, add '9' to it
      modified_code <- paste0(code, "9")

      # If the modified code exists in the environment
      if (exists(x = modified_code, envir = icd_codes_env, inherits = FALSE)) {
        # Add the modified code to the generated mapping
        icd_mapping[[code]] <- modified_code

        # Skip further processing for this code
        next
      }
    }

    # 3. **Trimming codes longer than or equal to 4 characters**
    trimmed_code <-
      if (nchar(code) > 4) {
        # For codes longer than 4 characters, trim them
        sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
      } else if (nchar(code) == 4) {
        # For codes exactly 4 characters, keep them as is
        code
      } else {
        # For codes shorter than 4 characters, set to NULL to skip trimming
        NULL
      }

    # For non-null trimmed codes
    if (!is.null(trimmed_code)) {
      # Check if the trimmed 4-character code exists
      if (exists(x = trimmed_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_code

        # Skip further processing for this code
        next
      }

      # If the 4-character code doesn't exist, trim further to 3 characters
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (exists(x = trimmed_to_3, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_to_3

        # Skip further processing for this code
        next
      }
    }

    # 4. **Mark as unmatched once all else fails**
    icd_mapping[[code]] <- NA_character_
  }

  # Return the results
  return(lapply(col, function(codes) {
    unname(sapply(codes, function(code) {
      # str(icd_mapping)
      if (!is.null(icd_mapping[[code]]) && !is.na(icd_mapping[[code]])) {
        icd_mapping[[code]]
      } else {
        code
      }
    }))
  }))
}
