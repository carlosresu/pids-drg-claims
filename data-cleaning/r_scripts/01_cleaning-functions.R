### Helper functions for general data cleaning and processing


## NOTE: Consider renaming this to clean_string_column
clean_column <- function(column_to_clean, na_like_strings, neoplasms_dt = neoplasms_dt_actual) {
  ## Cleans a string column by performing basic string operations
  # column_to_clean: the column to clean.
  # na_like_strings: strings to treat as NA.
  # neoplasms_dt: data.table for neoplasm codes where slashes should be preserved.

  # Convert the column to uppercase and ASCII format
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Remove non-letter and non-digit characters from the string (except delimiters like commas and pipes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d,|]+", "")

  # Replace any NA-like strings (as defined) with actual NA values
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # Replace any COVID-related codes within the string with "COVID" (even if they are part of other codes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, covid_rvs_pattern, "COVID")

  # Restore slashes for certain neoplasm ICD-10 codes, where slashes are important
  neopl <- setNames(neoplasms_dt$icd10, gsub("/", "", neoplasms_dt$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column
  return(cleaned_col)
}

# collapse_columns <- function(cols_to_process, na_like_strings) {
#   ## Combines multiple string columns into one and cleans the result
#   # cols_to_process: a list of columns to concatenate.
#   # na_like_strings: strings considered as NA.

#   # Clean each column in cols_to_process by applying clean_column
#   cleaned_columns <- lapply(cols_to_process, function(col) {
#     clean_column(col, na_like_strings, neoplasms_dt)
#   })

#   # Collapse the cleaned columns into a single column, separated by "||"
#   collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))

#   # Remove any occurrences of "||NA" or "NA||" or empty "||" from the collapsed string
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
#   collapsed_column <- stri_replace_all_regex(collapsed_column, "^\\|\\|", "")

#   # If the collapsed string is still NA-like, replace it with NA
#   collapsed_column <- ifelse(collapsed_column %in% na_like_strings,
#     NA_character_, collapsed_column
#   )

#   # Return the collapsed and cleaned column
#   return(collapsed_column)
# }

# Clean and collapse columns
collapse_columns <- function(
    cols_to_process,
    na_like_strings,
    neoplasms_dt = neoplasms_dt_actual) {
  ## Combines multiple string columns into one and cleans the result using clean_column.
  # cols_to_process: a list of columns to concatenate.
  # na_like_strings: strings considered as NA.
  # neoplasms_dt: data.table for neoplasm codes where slashes should be preserved.

  # Function to clean and split the column by different delimiters
  clean_and_split <- function(col, na_like_strings, neoplasms_dt) {
    # Clean the column using the clean_column function
    cleaned_col <- clean_column(col, na_like_strings, neoplasms_dt)

    # Split by multiple delimiters (comma, single pipe, or double pipe) while handling spaces
    split_col <- strsplit(cleaned_col, "\\s*,\\s*|\\|\\||\\|")

    # Return the split column
    return(split_col)
  }

  # Clean and split each column in cols_to_process
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_and_split(col, na_like_strings, neoplasms_dt)
  })

  # Collapse the cleaned columns into a single column, combining them with "||"
  collapsed_column <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
    # Combine corresponding rows from all columns and remove empty strings or NA-like values
    combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
    combined <- combined[!combined %in% na_like_strings & combined != ""]

    # Collapse the cleaned and combined values using "||" as the final separator
    if (length(combined) > 0) {
      return(paste(combined, collapse = "||"))
    } else {
      return(NA_character_)
    }
  })

  # Return the collapsed and cleaned column
  return(collapsed_column)
}

replace_empty_with_na_python <- function(dt, to_view_checks) {
  ## Replaces empty strings in a data.table with NA.
  ## Handles strings, factors, and lists.
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.

  # Identify columns that are characters, factors, or lists
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

  # Loop through each identified column and replace empty strings
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
      (col_name) := NA
    ]

    # If the column is a factor, make sure NA is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
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

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}


replace_empty_with_none <- function(dt, to_view_checks = FALSE) {
  ## Replaces empty values and NA with "None" across a data.table
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

    # Replace all empty, "NA", and "character(0)" values with "None"
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)" | is.na(get(col_name)),
      (col_name) := "None"
    ]

    # If the column is a factor, ensure "None" is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), "None")
        )
      )
    }

    if (to_view_checks) {
      # Update the replacement summary
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out columns where no replacements were made
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
  }

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}

split_to_vector_single <- function(column) {
  ## Splits a column of strings into vectors using "|" as the delimiter
  # column: the column to split

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # Split the string into a vector using "|"
      return(unlist(strsplit(x, "|", fixed = TRUE)))
    }
  })

  # Return the list of vectors
  return(result)
}

split_to_vector <- function(column) {
  ## Splits a column of strings into vectors using "||" as the delimiter
  # column: the column to split

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # Split the string into a vector using "||"
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })

  # Return the list of vectors
  return(result)
}

# collapse_and_clean_icd_rvs <- function(dt) {
#   ## Collapses and cleans ICD and RVS columns in a data.table
#   # dt: input data.table containing ICD and RVS columns

#   # Collapse the ICD codes from multiple columns into a single "clin_icd" column
#   dt[, clin_icd := collapse_columns(mget(paste0("clin_icd", 1:12)), na_like_strings)]
#   dt[, paste0("clin_icd", 1:12) := NULL] # Remove the individual columns

#   # Collapse the RVS codes from multiple columns into a single "clin_rvs" column
#   dt[, clin_rvs := collapse_columns(mget(paste0("clin_rvs", 1:20)), na_like_strings)]
#   dt[, paste0("clin_rvs", 1:20) := NULL] # Remove the individual columns

#   # Handle any lumped ICD codes by splitting them
#   dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]

#   # Convert the cleaned columns into vectors
#   dt[, clin_icd := split_to_vector(clin_icd)]
#   # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)]
#   dt[, clin_rvs := split_to_vector(clin_rvs)]

#   # Return the cleaned data.table
#   return(dt)
# }


collapse_and_clean_icd_rvs <- function(dt) {
  ## Collapses and cleans ICD and RVS columns in a data.table
  # dt: input data.table containing ICD and RVS columns
  available_columns <- colnames(dt)

  # Dynamically detect which clin_icd columns exist
  icd_cols <- grep("^clin_icd\\d+$", available_columns, value = TRUE)
  if (length(icd_cols) > 0) {
    # Collapse the ICD codes, whether from multiple columns or a single column
    dt[, clin_icd := collapse_columns(mget(icd_cols), na_like_strings)]
    dt[, (icd_cols) := NULL] # Remove the individual columns after collapsing
  }

  # Dynamically detect which clin_rvs columns exist
  rvs_cols <- grep("^clin_rvs\\d+$", available_columns, value = TRUE)
  if (length(rvs_cols) > 0) {
    # Collapse the RVS codes, whether from multiple columns or a single column
    dt[, clin_rvs := collapse_columns(mget(rvs_cols), na_like_strings)]
    dt[, (rvs_cols) := NULL] # Remove the individual columns after collapsing
  }

  # Handle any lumped ICD codes by splitting them if clin_icd exists
  if ("clin_icd" %in% available_columns) {
    dt[, clin_icd := remove_lumped_icd_codes(clin_icd)] # Apply cleaning for lumped codes
    dt[, clin_icd := split_to_vector(clin_icd)] # Convert cleaned string to vector
  }

  # Handle any lumped RVS codes by splitting them if clin_rvs exists
  if ("clin_rvs" %in% available_columns) {
    # Assuming there is a `remove_lumped_rvs_codes` function, apply it here.
    # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)] #TODO: why is this commented out?
    dt[, clin_rvs := split_to_vector(clin_rvs)] # Convert cleaned string to vector
  }

  # Return the cleaned data.table
  return(dt)
}

clean_clinical_columns <- function(dt) {
  ## Cleans and processes the clinical columns in a data.table
  # dt: input data.table with clinical columns

  # Transfer ICD-10 codes from case rates to clinical ICD column
  # dt <- transfer_cr_icd(dt)
  # Deduplicate the ICD codes
  dt <- apply_add_c1_c2_to_clin_icd(dt)

  # TODO: append rvs to clin_proc, dont delete from c1 and c2
  # Process case rate 1 RVS codes
  c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c1, rvs_icd9
  )
  dt[, clin_rvs := c1_rvs_results$clin_rvs]
  dt[, c1 := c1_rvs_results$col]
  c1_discarded_rvs <- c1_rvs_results$discarded_rvs

  # Process case rate 2 RVS codes
  c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$c2, rvs_icd9
  )
  dt[, clin_rvs := c2_rvs_results$clin_rvs]
  dt[, c2 := c2_rvs_results$col]
  c2_discarded_rvs <- c2_rvs_results$discarded_rvs

  # Ensure uniqueness of RVS codes in the final result
  # dt[, clin_rvs := lapply(clin_rvs, unique)]
  return(
    list(
      # Return the cleaned data.table
      dt = dt,
      # Return discarded RVS codes for checks
      discard_rvs_one = c1_discarded_rvs,
      discard_rvs_two = c2_discarded_rvs
    )
  )
}


transfer_cr_icd <- function(dt) {
  ## Transfers ICD-10 codes in case rates 1 and 2 to the clinical ICD list
  # dt: input data.table with case rates and clinical ICD codes

  # Split case rate 1 into vectors and transfer extra ICD-10 codes to clin_icd
  # dt[, c1 := split_to_vector(c1)]
  # c1_result <- transfer_extra_icd10s_to_clin_icd(
  #   dt$clin_icd, dt$c1
  # )
  # dt[, clin_icd := c1_result$clin_icd]
  # dt[, c1 := c1_result$col_first]

  # Split case rate 2 into vectors and transfer extra ICD-10 codes to clin_icd
  # dt[, c2 := split_to_vector(c2)]
  # c2_result <- transfer_extra_icd10s_to_clin_icd(
  #   dt$clin_icd, dt$c2
  # )
  # dt[, clin_icd := c2_result$clin_icd]
  # dt[, c2 := c2_result$col_first]

  # Return the updated data.table
  return(dt)
}

apply_add_c1_c2_to_clin_icd <- function(dt) {
  ## Deduplicates ICD codes in clin_icd if already present in c1 or c2
  # dt: input data.table

  # Add ICD codes from c1 and c2 to clin_icd, removing duplicates
  result <- add_c1_c2_to_clin_icd(dt$c1, dt$c2, dt$clin_icd)

  # Update clin_icd with the deduplicated result
  dt[, clin_icd := result$clin_icd]

  # Return the deduplicated data.table
  return(dt)
}
