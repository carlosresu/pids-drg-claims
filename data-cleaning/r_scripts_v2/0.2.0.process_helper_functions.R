manual_replacement <- function(text) {
  replaced <- stri_replace_all_regex(
    text,
    manual_patterns_to_replace,
    manual_code_replacements,
    vectorize_all = FALSE
  )
  return(replaced)
}

remove_periods_and_whitespaces <- function(x) {
  # Ensure UTF-8 encoding
  x <- sapply(x, function(elem) {
    element <- iconv(elem, from = "latin1", to = "UTF-8")
    return(element)
  }, USE.NAMES = FALSE)

  # Remove periods and whitespaces
  cleaned <- gsub("[.\\s]", "", x)
  return(cleaned)
}

split_to_vector <- function(column) {
  lapply(column, function(long_string) {
    # Initialize result vector
    result <- character(0)

    # Ensure long_string is not NA before proceeding
    if (is.na(long_string)) {
      return(result)
    }

    # Step 1: Extract COVID codes
    covid_matches <- gregexpr(covid_pattern, long_string, perl = TRUE)[[1]]
    if (!is.na(covid_matches[1]) && covid_matches[1] != -1) {
      covid_codes <- regmatches(long_string, list(covid_matches))[[1]]
      result <- c(result, covid_codes)
      # Remove COVID codes from long_string
      long_string <- gsub(covid_pattern, "", long_string, perl = TRUE)
    }

    # Step 2: Extract Neoplasm codes
    neoplasm_matches <- gregexpr(neoplasm_pattern,
      long_string,
      perl = TRUE
    )[[1]]
    if (!is.na(neoplasm_matches[1]) && neoplasm_matches[1] != -1) {
      neoplasm_codes <- regmatches(long_string, list(neoplasm_matches))[[1]]
      result <- c(result, neoplasm_codes)
      # Remove Neoplasm codes from long_string
      long_string <- gsub(neoplasm_pattern, "", long_string, perl = TRUE)
    }

    # Step 3: Extract RVS codes using individual patterns
    rvs_patterns <- c(
      "[A-Za-z]{3}[0-9]{2}", # Three letters followed by one or two digits
      "[A-Za-z]{2}[0-9]{3}", # Two letters followed by two or three digits
      "[A-Za-z][0-9]{4}", # A letter followed by four or five digits
      "[0-9]{5}" # Five consecutive numbers
    )

    for (pattern in rvs_patterns) {
      rvs_matches <- gregexpr(pattern, long_string, perl = TRUE)[[1]]
      if (!is.na(rvs_matches[1]) && rvs_matches[1] != -1) {
        rvs_codes <- regmatches(long_string, list(rvs_matches))[[1]]
        result <- c(result, rvs_codes)
        # Remove RVS codes from long_string
        long_string <- gsub(pattern, "", long_string, perl = TRUE)
      }
    }

    # Step 4: Remaining content in long_string should be lumped ICD codes
    if (!is.na(long_string) && nchar(long_string) > 0) {
      result <- c(result, long_string)
    }

    # Final Step: Split each element of result by "||" and flatten the output
    final_result <- unlist(lapply(result, function(element) {
      strsplit(element, "\\|\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings
    clean_result <- final_result[final_result != ""]
    return(clean_result)
  })
}

# USING ENVIRONMENTS
# Function to process ICD codes
remove_lumped_icd_codes <- function(column) {
  ## Processes a list column of character vectors, splitting lumped ICD-10 codes
  unlumped <- lapply(column, function(vec) {
    # Iterate through each element of the vector
    processed <- unlist(lapply(vec, function(element) {
      if ((is.na(element) || element == "")
      ) {
        # Keep intact if it's a valid neoplasm or COVID code
        return(character(0))
      } else {
        # Perform regex-based splitting for ICD-10
        # codes using the combined regex
        return(unlist(strsplit(element, "(?<=\\d)(?=[A-Z][0-9]{2,})",
          perl = TRUE
        )))
      }
    }))

    # Filter out empty strings and return the cleaned vector
    processed <- processed[processed != ""]
    return(processed)
  })
  return(unlumped)
}

flatten_then_check_null_na <- function(input) {
  # Fully flatten all nested lists into a character vector
  input <- unlist(input, recursive = TRUE)

  # Check if the flattened result is empty or only contains NULL/NA
  if (length(input) == 0 || all(is.null(input)) || all(is.na(input))) {
    return(character(0)) # Return empty character vector if all NULL/NA
  } else {
    return(input) # Already a flat character vector
  }
}

remove_lumped_rvs_codes <- function(column) {
  ## Separates out lumped RVS codes by splitting into chunks of 5 chars each
  modified_column <- sapply(
    as.character(column),
    function(code) {
      # Check if the code is NA, empty, or NULL, and return NA if so
      if (is.na(code) || code == "" || is.null(code)) {
        return(NA_character_)
      }

      # Remove all non-alphanumeric characters and clean the code
      # Remove all "|" characters
      code_clean <- gsub("\\|", "", code)
      # Remove non-alphanumeric characters
      code_clean <- gsub("[^A-Z0-9]", "", code_clean)

      # If the cleaned code length is 0, return NA
      if (nchar(code_clean) == 0) {
        return(NA_character_)
      }

      # If the cleaned code length is not a multiple of 5,
      # log a message and return NA
      if (nchar(code_clean) %% 5 != 0) {
        return(NA_character_)
      }

      # Insert "||" every 5 characters to split the code
      modified_code <- gsub("(.{5})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    },
    USE.NAMES = FALSE
  )
  return(modified_column) # Return the modified column with split RVS codes
}

# Function to collapse the replaced text with "||" as separator
collapse_to_string <- function(vec) {
  # Collapse non-empty elements with "||" as the separator
  vec <- vec[vec != "" & !is.na(vec)]
  if (length(vec) > 0) {
    vector <- paste(vec, collapse = "||")
    return(vector)
  } else {
    empty_vec <- NA_character_
    return(empty_vec)
  }
}

replace_na_or_empty <- function(
    dt, replace_with,
    to_view_checks = TRUE, additional_columns = NULL) {
  # Identify columns based on `replace_with` type
  cols <- if (replace_with == "NA_character_") {
    # Apply to character, factor, or list columns
    names(dt)[sapply(dt, function(col) {
      return(is.character(col) || is.factor(col) || is.list(col))
    })]
  } else {
    # Apply only to list columns if `replace_with` is character(0)
    names(dt)[sapply(dt, is.list)]
  }

  # Include any additional columns specified, avoiding duplicates
  cols <- unique(c(cols, additional_columns))

  # Define replacement values based on `replace_with` argument
  replacement_value <- if (replace_with == "NA_character_") {
    NA_character_
  } else {
    character(0)
  }

  # Process each relevant column
  for (colname in cols) {
    col <- dt[[colname]]
    # Separate handling for list and non-list columns
    if (is.list(col)) {
      # Replace values with `character(0)` in list columns
      dt[, (colname) := lapply(get(colname), function(x) {
        if (all(is.na(x)) || identical(x, "") || identical(x, "NA")) {
          character(0)
        } else {
          x
        }
      })]
    } else {
      # Count and replace for non-list columns
      # if `replace_with` is `NA_character_`
      # Replace values with `NA_character_`
      dt[
        get(colname) == "" | get(colname) == "NA" |
          get(colname) == "character(0)",
        (colname) := NA_character_
      ]
      # Ensure NA is a level if the column is a factor
      if (is.factor(col)) {
        set(dt, j = colname, value = factor(dt[[colname]],
          levels = c(levels(col), NA)
        ))
      }
    }
  }

  return(dt)
}


clean_column <- function(col) {
  # Convert column to character and normalize to ASCII
  column_to_clean <- as.character(col)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # First pass: Keep alphanumeric characters, /, and \
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d/\\\\]+", "")

  # Second pass: Remove / or \ if a letter is on either side
  cleaned_col <- stri_replace_all_regex(
    cleaned_col,
    "(?<=[A-Z])/|/(?=[A-Z])|(?<=[A-Z])\\\\|\\\\(?=[A-Z])",
    "",
    opts_regex = stri_opts_regex(case_insensitive = TRUE)
  )

  # Replace any NA-like strings with actual NA values
  cleaned_col[cleaned_col %chin% na_like_strings] <- NA_character_

  # Restore slashes for neoplasm ICD-10 codes
  neopl <- setNames(
    neoplasms_dt_actual$icd10,
    gsub("/", "", neoplasms_dt_actual$icd10)
  )
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[
    matched_indices[!is.na(matched_indices)]
  ]

  return(cleaned_col)
}

remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    # Return NA for NULL or empty lists
    return(NA_character_)
  } else {
    # Remove all whitespace characters
    return(gsub("\\s+", "", x))
  }
}

prep_icd_for_mapping <- function(text) {
  text %>%
    manual_replacement() %>%
    collapse_to_string() %>%
    split_to_vector() %>%
    remove_lumped_icd_codes() %>%
    flatten_then_check_null_na()
}

# Filter ICD codes based on exclusion criteria,
# for use in prepare_pdx_inputs function
filter_icds <- function(codes, neoplasm_codes, covidrvs, acc_pdx_set) {
  codes <- codes[!is.null(codes) & !is.na(codes) & !grepl("^[0-9]", codes) &
    !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
    !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
    !(codes %chin% covidrvs)]
  filtered <- codes[codes %chin% acc_pdx_set]
  return(filtered)
}

# Helper function to handle NULL or NA safely
safe_split <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    # Return an empty character vector for consistency
    return(NA_character_)
  }
  # Split valid strings by '|'
  unlisted_and_split <- unlist(strsplit(x, "\\|"))
  return(unlisted_and_split)
}

print_status_update <- function(
    status_part, split_parts,
    processing_times, phase) {
  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Inline conversion of seconds to period and formatting to string
  convert_and_format_time <- function(seconds) {
    # Convert seconds to period using lubridate
    period <- lubridate::seconds_to_period(round(seconds))

    # Extract hours, minutes, and seconds
    h <- lubridate::hour(period)
    m <- lubridate::minute(period)
    s <- lubridate::second(period)

    # Build time string
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))
    trimmed_time <- trimws(paste(time_components, collapse = " "))
    return(trimmed_time)
  }

  # Calculate and format elapsed and remaining time
  elapsed_str <- convert_and_format_time(elapsed_time)
  remaining_str <- convert_and_format_time(estimated_remaining_time)

  # Helper to print the status update
  print_update <- function(message) {
    cat(sprintf(message, status_part, split_parts, elapsed_str, remaining_str))
    utils::flush.console()
  }

  # Determine when to print the status update based on phase
  if (phase == "split") {
    if (avg_time_per_part >= 4 || status_part %% 5 == 0) {
      print_update("\rFinished splitting %d of %d parts in %s (ETA %s)       ")
    }
  } else if (phase == "clean") {
    if (avg_time_per_part >= 4 || status_part %% 5 == 0) {
      print_update("\rFinished cleaning %d of %d parts in %s (ETA %s)       ")
    }
  }
}
