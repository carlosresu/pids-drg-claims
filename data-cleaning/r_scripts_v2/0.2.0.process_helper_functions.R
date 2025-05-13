clean_column <- function(col) {
  # Convert the input column to its ASCII representation.
  # (Note: "column_to_clean" should be "col" if that's the intended variable.)
  cleaned_col <- stri_trans_general(col, "Latin-ASCII")

  # Convert all characters in the column to uppercase.
  cleaned_col <- toupper(cleaned_col)

  # First pass: Remove any characters that are not:
  # - Word characters (\w) Digits (\d)
  # - Forward slashes (/) Backslashes (\)
  # This effectively keeps alphanumeric characters and the slash symbols.
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d/\\\\]+", "")

  # Second pass: Remove any forward slash or backslash that is adjacent to a letter.
  # The regex uses lookbehind (?<=[A-Z]) and lookahead (?=[A-Z])
  # assertions to detect if a slash is preceded or followed by
  # an uppercase letter. It removes the slash if a letter is on
  # either side, ensuring that such punctuation is dropped.
  cleaned_col <- stri_replace_all_regex(
    cleaned_col,
    "(?<=[A-Z])/|/(?=[A-Z])|(?<=[A-Z])\\\\|\\\\(?=[A-Z])",
    "",
    opts_regex = stri_opts_regex(case_insensitive = TRUE)
  )

  # Replace any strings that match common NA-like values
  # (stored in na_values) with an actual NA_character_.
  # This standardizes missing values.
  cleaned_col[cleaned_col %chin% na_values] <- NA_character_

  # Restore original slashes for neoplasm ICD-10 codes:
  # 1. Create a named vector 'neopl' where the names are
  # the ICD-10 codes with the slash removed, and the values
  # are the original ICD-10 codes (with the slash).
  neopl <- setNames(
    neoplasms_dt_actual$icd10,
    gsub("/", "", neoplasms_dt_actual$icd10)
  )
  # 2. Find the positions in cleaned_col that match
  # any of the names in 'neopl'.
  matched_indices <- match(cleaned_col, names(neopl))
  # 3. Replace the entries in cleaned_col that have a
  # match with the corresponding original ICD-10 code.
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column vector.
  return(cleaned_col)
}

# Function to collapse selected columns into a single character vector
collapse_cols <- function(cols) {
  apply(do.call(cbind, cols), 1, function(row) {
    row_vals <- na.omit(row) # Remove NAs
    if (length(row_vals) > 0) {
      paste0(row_vals, collapse = "||")
    } else {
      NA_character_
    }
  })
}

replace_na_or_empty <- function(dt, replace_with) {
  na_vals <- na_values
  # Identify relevant columns
  cols <- if (replace_with == "NA_character_") {
    names(dt)[sapply(
      dt,
      function(col) is.character(col) || is.factor(col) || is.list(col)
    )]
  } else {
    names(dt)[sapply(dt, is.list)]
  }

  # Define replacement value
  replacement_value <- if (replace_with == "NA_character_") {
    NA_character_
  } else {
    character()
  }

  # Process each column
  for (col_name in cols) {
    col <- dt[[col_name]]

    if (is.list(col)) {
      dt[, (col_name) := lapply(get(col_name), function(x) {
        if (all(is.na(x)) || x %in% na_vals) character() else x
      })]
    } else {
      dt[
        get(col_name) %in% na_vals,
        (col_name) := replacement_value
      ]

      # Ensure NA is a level if the column is a factor
      if (is.factor(col)) {
        set(dt, j = col_name, value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        ))
      }
    }
  }

  return(dt)
}

manual_replacement <- function(text) {
  replaced <- stri_replace_all_regex(
    text,
    manual_patterns_to_replace,
    manual_code_replacements,
    vectorize_all = FALSE
  )
  return(replaced)
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

# Function to expand mappings row-wise
expand_mappings <- function(raw_list, map_list) {
  rbindlist(mapply(function(raw, map) {
    if (length(map) == 0) {
      return(data.table(raw_code = raw, mapped_code = "_")) # Mark unmappable cases
    }
    data.table(raw_code = rep(raw, length(map)), mapped_code = map)
  }, raw_list, map_list, SIMPLIFY = FALSE), fill = TRUE)
}
