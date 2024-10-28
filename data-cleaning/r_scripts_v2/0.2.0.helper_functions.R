remove_lumped_icd_codes <- function(column) {
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  # Use regex to add "||" between letters and digits
  # in the ICD codes (e.g., A123B456 -> A123||B456)
  modified_column <- stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Z]\\d{2,4})", "||",
    # Specify regex options for the replacement
    opts_regex = stri_opts_regex()
  )

  # Return the modified column with ICD codes split
  return(modified_column)
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
  # Clean the column
  column_to_clean <- as.character(col)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Remove non-letter and non-digit characters (except delimiters like commas and pipes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d,|]+", "")

  # Replace any NA-like strings with actual NA values
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # Save whether any COVID-related RVS is found
  covid_rvs_pattern <- paste0(covid_rvs, collapse = "|")
  is_covid <- stri_detect_regex(cleaned_col, covid_rvs_pattern)
  is_covid[is.na(is_covid)] <- FALSE # Ensure is_covid is logical and handle NAs

  # Restore slashes for certain neoplasm ICD-10 codes
  neopl <- setNames(neoplasms_dt_actual$icd10, gsub("/", "", neoplasms_dt_actual$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column and is_covid flag
  return(list(
    cleaned_col = cleaned_col,
    is_covid = is_covid
  ))
}

safe_unlist <- function(x) unique(na.omit(unlist(x)))

combine_comparison_tables <- function(summaries, field) {
  comparison_list <- lapply(summaries, function(summary) summary[[field]])
  combined <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined) == 0) {
    return(data.table(old_code = character(), new_code = character(), diff_chars = integer()))
  }

  combined[, `:=`(
    old_code = gsub("\\s", "", iconv(old_code, to = "UTF-8")),
    new_code = gsub("\\s", "", iconv(new_code, to = "UTF-8"))
  )]
  combined[, diff_chars := abs(nchar(old_code) - nchar(new_code))]
  return(unique(combined[order(-diff_chars)]))
}

combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}

combine_replace_empty_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(Column = character(), Empty_Replaced = integer(), NA_Replaced = integer(), Character0_Replaced = integer()))
  }
  return(combined[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)])
}

final_combine_replace_empty_tables <- function(summaries, field) {
  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Total_Empty_Replaced = integer(),
      Total_NA_Replaced = integer(),
      Total_Character0_Replaced = integer(),
      Total_Elements = integer(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE),
    Total_Elements = if (to_sample) sample_size * split_parts else total_rows
  ), by = Column]

  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", (Total_Empty_Replaced / Total_Elements) * 100),
    NA_Replaced_Percentage = sprintf("%.2f%%", (Total_NA_Replaced / Total_Elements) * 100),
    Character0_Replaced_Percentage = sprintf("%.2f%%", (Total_Character0_Replaced / Total_Elements) * 100)
  )]

  return(combined_replace_empty[order(
    -as.numeric(gsub("%", "", Empty_Replaced_Percentage)),
    -as.numeric(gsub("%", "", NA_Replaced_Percentage)),
    -as.numeric(gsub("%", "", Character0_Replaced_Percentage))
  ), .(
    Column,
    Empty_Replaced_Percentage,
    NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}

combine_unmatched_icd10_codes <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
  return(combined[, .(count = sum(count)), by = .(code, source)][order(-count)])
}
