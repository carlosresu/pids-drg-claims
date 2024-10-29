remove_lumped_icd_codes <- function(column) {
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  # Use regex to add "||" between letters and digits
  # in the ICD codes (e.g., A123B456 -> A123||B456)
  stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Z]\\d{2,4})", "||",
    # Specify regex options for the replacement
    opts_regex = stri_opts_regex()
  )
}

split_to_vector <- function(column, covidrvspattern = covid_rvs_pattern) {
  ## Splits a column of strings in two passes:
  # 1. Split on COVID RVS codes
  # 2. Split on "||" delimiter

  result <- lapply(column, function(x) {
    # If the entry is NA, leave it as is
    if (is.na(x)) {
      return(NA_character_)
    } else {
      # First pass: Split on COVID RVS codes to handle lumped codes
      first_split <- unlist(strsplit(x, covidrvspattern, perl = TRUE))

      # Second pass: Split on "||" within each split chunk
      final_split <- unlist(strsplit(first_split, "||", fixed = TRUE))

      # Remove empty strings or NA-like values
      final_split <- final_split[final_split != "" & !is.na(final_split)]

      return(final_split)
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

print_status_update <- function(status_part, split_parts, processing_times, phase) {
  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Inline conversion of seconds to period and formatting to string
  convert_and_format_time <- function(seconds) {
    # Convert seconds to period using lubridate
    period <- seconds_to_period(round(seconds))

    # Extract hours, minutes, and seconds
    h <- hour(period)
    m <- minute(period)
    s <- second(period)

    # Build time string
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))

    return(trimws(paste(time_components, collapse = " ")))
  }

  # Calculate and format elapsed and remaining time
  elapsed_str <- convert_and_format_time(elapsed_time)
  remaining_str <- convert_and_format_time(estimated_remaining_time)

  # Helper to print the status update
  print_update <- function(message) {
    cat(sprintf(message, status_part, split_parts, elapsed_str, remaining_str))
    flush.console()
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
