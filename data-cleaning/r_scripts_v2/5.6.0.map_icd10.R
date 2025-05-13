map_icd10 <- function(col) {
  # Collect all unique ICD codes from the column
  icds <- unique(unlist(col))

  # Pre-filter ICDs:
  # - must be non-NA, at least 3 characters
  # - exclude purely numeric, 2-letter codes, or neoplasm-style slashes
  # - also exclude anything already known in covid_rvs_neoplasm_zben_env
  filtered_icds <- icds[!is.na(icds) &
    nchar(icds) >= 3 &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !vapply(icds, function(code) {
      # add zben codes to exclusion criteria
      exists(x = code, envir = covid_rvs_neoplasm_zben_env, inherits = FALSE)
    }, logical(1))]

  # Initialize the output ICD mapping list
  icd_mapping <- list()

  # Loop through each filtered code
  for (code in filtered_icds) {
    code <- trimws(code) # Remove leading/trailing whitespace

    # 1. Exact match against ICD dictionary
    if (exists(x = code, envir = icd_codes_env, inherits = FALSE)) {
      icd_mapping[[code]] <- code
      next
    }

    # 2. If it's a 3-character code, try appending '9' (e.g., J18 → J189)
    if (nchar(code) == 3) {
      modified_code <- paste0(code, "9")
      if (exists(x = modified_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- modified_code
        next
      }
    }

    # 3. Attempt to trim excess characters if longer than 4
    trimmed_code <- if (nchar(code) > 4) {
      # TODO: the below doesn't account for things like J1892C,
      # since it demands the 6th char to be a number
      # also, \\D+ at the start doesn't match the start of strings
      # so, codes are eventually still wrong if they start incorrectly
      # \\D+ just means any non-digit character, but we should be more specific
      # i.e., specify [A-Z] only
      sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
    } else if (nchar(code) == 4) {
      code # Already 4 characters, no need to trim
    } else {
      NULL # Invalid length
    }

    # Check if trimmed code exists in dictionary
    if (!is.null(trimmed_code)) {
      if (exists(x = trimmed_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_code
        next
      }

      # If 4-character trimmed fails, try matching first 3 characters
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (exists(x = trimmed_to_3, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_to_3
        next
      }
    }

    # 4. Fallback: mark as unmappable
    icd_mapping[[code]] <- "_"
  }

  # Apply mapping back to the original structure
  ret <- lapply(col, function(vec) {
    if (length(vec) == 0) {
      return(character(0)) # Preserve empty entries
    }

    # Replace each code with its mapped counterpart or "_" if not found
    mapped_vec <- vapply(vec, function(code) {
      if (!is.null(icd_mapping[[code]]) && icd_mapping[[code]] != "") {
        return(unname(icd_mapping[[code]])) # Unnamed string value
      } else {
        return("_") # Default for unmappable
      }
    }, FUN.VALUE = character(1))

    # Strip names just in case
    mapped_vec <- unname(mapped_vec)

    return(mapped_vec)
  })

  return(ret) # Always return list of character vectors (per row)
}
