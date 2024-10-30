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

    # Return the character vector of the split result
    return(first_split)
  })

  return(result)
}

### Remove Lumped ICD Codes Function ###
remove_lumped_icd_codes <- function(column) {
  ## Processes a list column of character vectors, splitting lumped ICD-10 codes

  result <- lapply(column, function(vec) {
    # Iterate through each element of the vector
    processed <- unlist(lapply(vec, function(element) {
      # Check if the element matches COVID, RVS, or neoplasm codes
      if (element %in% c(covid_codes, rvs_codes, neoplasm_codes)) {
        return(element) # Keep intact if it's a valid code
      } else {
        # Perform regex-based splitting for ICD-10 codes
        return(unlist(strsplit(element, "(?=[A-Z][0-9]{2,})", perl = TRUE)))
      }
    }))

    # Filter out empty strings and return the cleaned vector
    return(processed[processed != ""])
  })

  return(result)
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


replace_empty_with_na <- function(dt, to_view_checks = TRUE) {
  ## Replaces empty strings with NA across an entire data.table.
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.
  # Identify columns that are character, factor, or list
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  # Create a summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  # Loop through each identified column
  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      # Count how many empty, "NA", or "character(0)" entries exist
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Replace all empty, "NA", and "character(0)" values with actual NA
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA_character_
    ]

    # If the column is a factor, ensure that NA is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }
    # Update the replacement summary
    replacement_summary <- rbind(replacement_summary, data.table(
      Column = col_name,
      Empty_Replaced = empty_count,
      NA_Replaced = na_count,
      Character0_Replaced = char0_count
    ))
  }
  # Filter out columns where no replacements were made
  replacement_summary <- replacement_summary[
    Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
  ]

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
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
