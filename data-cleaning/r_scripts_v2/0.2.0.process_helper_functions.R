manual_replacement <- function(text) {
  stri_replace_all_regex(text, manual_patterns_to_replace, manual_code_replacements, vectorize_all = FALSE)
}

remove_periods_and_whitespaces <- function(x) {
  gsub("[.\\s]", "", x)
}

# OLD CODE BEFORE REFACTORING:
# remove_lumped_icd_codes <- function(column) {
#   ## Takes a column and separates out ICD-10 codes using "||"
#   ## been lumped into a single string

#   # Use regex to add "||" between letters and digits
#   # in the ICD codes (e.g., A123B456 -> A123||B456)
#   stri_replace_all_regex(
#     column, "(?<=\\d)(?=[A-Z]\\d{2,4})", "||",
#     # Specify regex options for the replacement
#     opts_regex = stri_opts_regex()
#   )
# }

# split_to_vector <- function(column, covidrvspattern = covid_rvs_pattern) {
#   ## Splits a column of strings in two passes:
#   # 1. Split on COVID RVS codes
#   # 2. Split on "||" delimiter

#   result <- lapply(column, function(x) {
#     # If the entry is NA, leave it as is
#     if (is.na(x)) {
#       return(NA_character_)
#     } else {
#       # First pass: Split on COVID RVS codes to handle lumped codes
#       first_split <- unlist(strsplit(x, covidrvspattern, perl = TRUE))

#       # Second pass: Split on "||" within each split chunk
#       final_split <- unlist(strsplit(first_split, "||", fixed = TRUE))

#       # Remove empty strings or NA-like values
#       final_split <- final_split[final_split != "" & !is.na(final_split)]

#       return(final_split)
#     }
#   })

#   # Return the list of vectors
#   return(result)
# }

# NEW REFACTORED CODE:

### Split to Vector Function ###
split_to_vector <- function(column) {
  ## Splits strings into vectors by first handling COVID, RVS, and neoplasm codes
  result <- lapply(column, function(x) {
    # Handle NA values
    if (is.na(x)) {
      return(NA_character_)
    }

    # First pass: Split using combined pattern of COVID, RVS, and neoplasm codes
    first_split <- unlist(strsplit(x, paste0("(", covid_rvs_neoplasm_pattern, ")"), perl = TRUE))

    # Filter out empty strings
    first_split <- first_split[first_split != ""]

    second_split <- unlist(strsplit(first_split, "\\|\\|"))
    # Return the character vector of the split result
    return(second_split)
  })

  return(result)
}

# ### Remove Lumped ICD Codes Function ###
# remove_lumped_icd_codes <- function(column) {
#   ## Processes a list column of character vectors, splitting lumped ICD-10 codes

#   result <- lapply(column, function(vec) {
#     # Iterate through each element of the vector
#     processed <- unlist(lapply(vec, function(element) {
#       # Check if the element matches COVID, RVS, or neoplasm codes
#       if (element %in% c(covid_codes, rvs_codes, neoplasm_codes)) {
#         return(element) # Keep intact if it's a valid code
#       } else {
#         # Perform regex-based splitting for ICD-10 codes
#         return(unlist(strsplit(element, "(?=[A-Z][0-9]{2,})", perl = TRUE)))
#       }
#     }))

#     # Filter out empty strings and return the cleaned vector
#     return(processed[processed != ""])
#   })

#   return(result)
# }

# New code:
remove_lumped_icd_codes <- function(column) {
  ## Processes a list column of character vectors, splitting lumped ICD-10 codes

  result <- lapply(column, function(vec) {
    # Iterate through each element of the vector
    processed <- unlist(lapply(vec, function(element) {
      # Check if the element matches COVID, RVS, or neoplasm codes
      if (element %in% c(covid_codes, rvs_codes, neoplasm_codes)) {
        return(element) # Keep intact if it's a valid code
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

# replace_empty_with_na <- function(dt, to_view_checks = TRUE) {
#   ## Replaces empty strings with NA across an entire data.table.
#   # dt: input data.table
#   # to_view_checks: flag to track the replacement count for checks.
#   # Identify columns that are character, factor, or list
#   char_factor_cols <- names(dt)[sapply(
#     dt,
#     function(col) is.character(col) || is.factor(col) || is.list(col)
#   )]

#   # Create a summary table for tracking replacements
#   replacement_summary <- data.table(
#     Column = character(),
#     Empty_Replaced = integer(),
#     NA_Replaced = integer(),
#     Character0_Replaced = integer()
#   )

#   # Loop through each identified column
#   for (col_name in char_factor_cols) {
#     col <- dt[[col_name]]
#     if (to_view_checks) {
#       # Count how many empty, "NA", or "character(0)" entries exist
#       empty_count <- sum(col == "", na.rm = TRUE)
#       na_count <- sum(col == "NA", na.rm = TRUE)
#       char0_count <- sum(col == "character(0)", na.rm = TRUE)
#     }

#     # Replace all empty, "NA", and "character(0)" values with actual NA
#     dt[
#       get(
#         col_name
#       ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
#       (col_name) := NA_character_
#     ]

#     # If the column is a factor, ensure that NA is a valid level
#     if (is.factor(col)) {
#       set(dt,
#         j = col_name,
#         value = factor(dt[[col_name]],
#           levels = c(levels(col), NA)
#         )
#       )
#     }
#     # Update the replacement summary
#     replacement_summary <- rbind(replacement_summary, data.table(
#       Column = col_name,
#       Empty_Replaced = empty_count,
#       NA_Replaced = na_count,
#       Character0_Replaced = char0_count
#     ))
#   }
#   # Filter out columns where no replacements were made
#   replacement_summary <- replacement_summary[
#     Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
#   ]

#   return(
#     list(
#       # Return the modified data.table
#       return_data = dt,
#       # Return the summary of replacements
#       return_replacement_summary = replacement_summary
#     )
#   )
# }

replace_na_or_empty <- function(dt, replace_with, to_view_checks = TRUE, additional_columns = NULL) {
  # Validate `replace_with` argument
  if (!replace_with %in% c("NA_character_", "character(0)")) {
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
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

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

flatten_and_clean <- function(input) {
  # Fully flatten all nested lists into a character vector
  input <- unlist(input, recursive = TRUE)

  # Check if the flattened result is empty or only contains NULL/NA
  if (length(input) == 0 || all(is.null(input)) || all(is.na(input))) {
    return(character(0)) # Return empty character vector if all NULL/NA
  } else {
    return(input) # Already a flat character vector
  }
}
