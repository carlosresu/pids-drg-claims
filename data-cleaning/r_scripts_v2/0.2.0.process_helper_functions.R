manual_replacement <- function(text) {
  stri_replace_all_regex(text, manual_patterns_to_replace, manual_code_replacements, vectorize_all = FALSE)
}

remove_periods_and_whitespaces <- function(x) {
  # Ensure UTF-8 encoding
  x <- sapply(x, function(elem) iconv(elem, from = "latin1", to = "UTF-8"), USE.NAMES = FALSE)

  # Remove periods and whitespaces
  x <- gsub("[.\\s]", "", x)

  return(x)
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
    neoplasm_matches <- gregexpr(neoplasm_pattern, long_string, perl = TRUE)[[1]]
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

    return(final_result[final_result != ""]) # Remove any empty strings
  })
}

# USING ENVIRONMENTS
# Function to process ICD codes
remove_lumped_icd_codes <- function(column) {
  ## Processes a list column of character vectors, splitting lumped ICD-10 codes

  result <- lapply(column, function(vec) {
    # Iterate through each element of the vector
    processed <- unlist(lapply(vec, function(element) {
      if ((is.na(element) || element == "") # && (!is.null(mget(element, envir = neoplasm_env, ifnotfound = NA)[[1]]) || !is.null(mget(element, envir = covid_env, ifnotfound = NA)[[1]]))
      ) {
        return(character(0)) # Keep intact if it's a valid neoplasm or COVID code
      } else {
        # Perform regex-based splitting for ICD-10 codes using the combined regex
        return(unlist(strsplit(element, "(?<=\\d)(?=[A-Z][0-9]{2,})", perl = TRUE)))
      }
    }))

    # Filter out empty strings and return the cleaned vector
    return(processed[processed != ""])
  })

  return(result)
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

  # Define a helper function to split each code into 5-character chunks
  split_rvs_codes_helper <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # If the input code is NA, empty, or NULL, return NA
    }

    # Remove all non-alphanumeric characters and clean the code
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters

    # If the cleaned code length is 0, return NA
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 5 != 0) {
      # If the length is not a multiple of 5, log a message and return NA
      message(paste0("Total length of concatenated RVS codes is not a multiple of 5 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert "||" every 5 characters to split the code
      modified_code <- gsub("(.{5})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element of the input column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper, USE.NAMES = FALSE)

  return(modified_column) # Return the modified column with split RVS codes
}

# Function to collapse the replaced text with "||" as separator
collapse_to_string <- function(vec) {
  # Collapse non-empty elements with "||" as the separator
  vec <- vec[vec != "" & !is.na(vec)]
  if (length(vec) > 0) {
    paste(vec, collapse = "||")
  } else {
    NA_character_
  }
}

replace_na_or_empty <- function(dt, replace_with, to_view_checks = TRUE, additional_columns = NULL) {
  # Validate `replace_with` argument
  if (!replace_with %chin% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }

  # Identify columns based on `replace_with` type
  cols <- if (replace_with == "NA_character_") {
    # Apply to character, factor, or list columns
    names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col) || is.list(col))]
  } else {
    # Apply only to list columns if `replace_with` is character(0)
    names(dt)[sapply(dt, is.list)]
  }

  # Include any additional columns specified, avoiding duplicates
  cols <- unique(c(cols, additional_columns))

  # Initialize summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    String_NA_Replaced = integer(),
    Actual_NA_Replaced = integer()
  )

  # Define replacement values based on `replace_with` argument
  replacement_value <- if (replace_with == "NA_character_") NA_character_ else character(0)
  label_na_replaced <- if (replace_with == "NA_character_") "String_NA_Replaced" else "Actual_NA_Replaced"
  label_char0_replaced <- if (replace_with == "NA_character_") "Actual_NA_Replaced" else "String_NA_Replaced"

  # Process each relevant column
  for (col_name in cols) {
    col <- dt[[col_name]]
    empty_count <- 0
    string_na_count <- 0
    actual_na_count <- 0

    # Separate handling for list and non-list columns
    if (is.list(col)) {
      if (to_view_checks) {
        # Count occurrences in list columns
        empty_count <- sum(sapply(col, function(x) identical(x, "")))
        string_na_count <- sum(sapply(col, function(x) identical(x, "NA")))
        actual_na_count <- sum(sapply(col, function(x) all(is.na(x)) || (is.list(x) && length(x) == 0)))
      }
      # Replace values with `character(0)` in list columns
      dt[, (col_name) := lapply(get(col_name), function(x) {
        if (all(is.na(x)) || identical(x, "") || identical(x, "NA")) character(0) else x
      })]
    } else {
      # Count and replace for non-list columns if `replace_with` is `NA_character_`
      if (to_view_checks) {
        empty_count <- sum(col == "", na.rm = TRUE)
        string_na_count <- sum(col == "NA", na.rm = TRUE)
        actual_na_count <- sum(col == "character(0)", na.rm = TRUE)
      }
      # Replace values with `NA_character_`
      dt[
        get(col_name) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
        (col_name) := NA_character_
      ]
      # Ensure NA is a level if the column is a factor
      if (is.factor(col)) {
        set(dt, j = col_name, value = factor(dt[[col_name]], levels = c(levels(col), NA)))
      }
    }

    # Update the replacement summary
    summary_row <- data.table(
      Column = col_name,
      Empty_Replaced = empty_count,
      String_NA_Replaced = ifelse(replace_with == "NA_character_", string_na_count, NA_integer_),
      Actual_NA_Replaced = ifelse(replace_with == "character(0)", actual_na_count, NA_integer_)
    )
    replacement_summary <- rbind(replacement_summary, summary_row, fill = TRUE)
  }

  # Filter out columns with no replacements made
  replacement_summary <- replacement_summary[
    Empty_Replaced > 0 | get(label_na_replaced) > 0 | get(label_char0_replaced) > 0
  ]

  return(list(
    return_data = dt,
    return_replacement_summary = replacement_summary
  ))
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

  # Detect if any COVID-related RVS is found
  covid_rvs_pattern <- paste0(covid_rvs, collapse = "|")
  is_covid <- stri_detect_regex(cleaned_col, covid_rvs_pattern)
  is_covid[is.na(is_covid)] <- FALSE # Handle NAs

  # Restore slashes for neoplasm ICD-10 codes
  neopl <- setNames(neoplasms_dt_actual$icd10, gsub("/", "", neoplasms_dt_actual$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column and is_covid flag
  return(list(
    cleaned_col = cleaned_col,
    is_covid = is_covid
  ))
}

remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_) # Return NA for NULL or empty lists
  } else {
    return(gsub("\\s+", "", x)) # Remove all whitespace characters
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
filter_icds <- function(codes) {
  codes <- codes[!is.na(codes) & !grepl("^[0-9]", codes) &
    !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
    !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
    !(codes %chin% covidrvs)]
  codes[codes %chin% acc_pdx_set]
}
