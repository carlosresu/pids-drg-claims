print_status_update <- function(status_part, split_parts, processing_times, phase) {
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

    return(trimws(paste(time_components, collapse = " ")))
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

safe_unlist <- function(x) if (length(x) > 0) unlist(x, recursive = TRUE) else character(0)

extract_modified_matches <- function(summaries) {
  all_modified_matches <- list()
  all_modified_match <- list()

  for (summary in summaries) {
    result <- safe_access(summary, "modified_matches")
    if (is.list(result) && all(c("modified_matches", "modified_match") %in% names(result))) {
      all_modified_matches <- append(all_modified_matches, result$modified_matches)
      all_modified_match <- append(all_modified_match, result$modified_match)
    }
  }

  data.table::data.table(
    modified_matches = safe_unlist(all_modified_matches),
    modified_match = safe_unlist(all_modified_match)
  )[, .(count = .N), by = .(modified_matches, modified_match)]
}

safe_extract <- function(summary, field) {
  tryCatch(summary[[field]], error = function(e) NULL)
}

combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- data.table::rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table::data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}

combine_replacement_tables <- function(rboundlist, replace_with) {
  # Validate replace_with argument
  if (!replace_with %in% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }

  # Define column names based on replacement type
  if (replace_with == "NA_character_") {
    summary_cols <- c("Empty_Replaced", "NA_Replaced", "Character0_Replaced")
  } else {
    summary_cols <- c("Empty_Replaced", "String_NA_Replaced", "Actual_NA_Replaced")
  }

  # Initialize empty table if the input has no rows
  if (nrow(rboundlist) == 0) {
    return(data.table::data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_or_String_Replaced = integer(),
      Character0_or_Actual_Replaced = integer()
    ))
  }

  # Ensure missing columns are filled with 0 for aggregation
  for (col in summary_cols) {
    if (!col %in% names(rboundlist)) rboundlist[, (col) := 0]
  }

  # Aggregate replacement counts by column
  combined <- rboundlist[, .(
    Empty_Replaced = sum(get(summary_cols[1]), na.rm = TRUE),
    Replaced_1 = sum(get(summary_cols[2]), na.rm = TRUE),
    Replaced_2 = sum(get(summary_cols[3]), na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -Replaced_1, -Replaced_2)]

  # Return with distinct names based on replacement type
  if (replace_with == "NA_character_") {
    data.table::setnames(combined, old = c("Replaced_1", "Replaced_2"), new = c("NA_Replaced", "Character0_Replaced"))
    return(combined[, .(Column, Empty_Replaced, NA_Replaced, Character0_Replaced)])
  } else {
    data.table::setnames(combined, old = c("Replaced_1", "Replaced_2"), new = c("String_NA_Replaced", "Actual_NA_Replaced"))
    return(combined[, .(Column, Empty_Replaced, String_NA_Replaced, Actual_NA_Replaced)])
  }
}

final_combine_replace_tables <- function(rboundlist, replace_with, samplesizedivisor = sample_size_divisor, splitparts = split_parts, totalrows = total_rows) {
  # Validate the replace_with argument
  if (!replace_with %in% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }

  # Define column names based on replacement type
  if (replace_with == "NA_character_") {
    summary_cols <- c("Empty_Replaced", "NA_Replaced", "Character0_Replaced")
  } else {
    summary_cols <- c("Empty_Replaced", "String_NA_Replaced", "Actual_NA_Replaced")
  }

  # Calculate the sample size
  samplesize <- ceiling(totalrows / samplesizedivisor)
  total_elements <- samplesize * splitparts

  if (nrow(rboundlist) == 0) {
    return(data.table::data.table(
      Column = character(),
      Empty_Replaced_Percentage = character(),
      NA_or_String_Replaced_Percentage = character(),
      Character0_or_Actual_Replaced_Percentage = character()
    ))
  }

  # Ensure missing columns are filled with 0 for aggregation
  for (col in summary_cols) {
    if (!col %in% names(rboundlist)) rboundlist[, (col) := 0]
  }

  # Aggregate replacement counts by column
  combined_replace <- rboundlist[, .(
    Total_Empty_Replaced = sum(get(summary_cols[1]), na.rm = TRUE),
    Total_Replaced_1 = sum(get(summary_cols[2]), na.rm = TRUE),
    Total_Replaced_2 = sum(get(summary_cols[3]), na.rm = TRUE)
  ), by = Column]

  # Calculate and cap percentages at 100%
  combined_replace[, `:=`(
    Empty_Replaced_Percentage = pmin((Total_Empty_Replaced / total_elements) * 100, 100),
    Replaced_1_Percentage = pmin((Total_Replaced_1 / total_elements) * 100, 100),
    Replaced_2_Percentage = pmin((Total_Replaced_2 / total_elements) * 100, 100)
  )]

  # Format percentages with two decimal places
  combined_replace[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    Replaced_1_Percentage = sprintf("%.2f%%", Replaced_1_Percentage),
    Replaced_2_Percentage = sprintf("%.2f%%", Replaced_2_Percentage)
  )]

  # Return with distinct names based on replacement type
  if (replace_with == "NA_character_") {
    data.table::setnames(combined_replace, old = c("Replaced_1_Percentage", "Replaced_2_Percentage"), new = c("NA_Replaced_Percentage", "Character0_Replaced_Percentage"))
    return(combined_replace[, .(Column, Empty_Replaced_Percentage, NA_Replaced_Percentage, Character0_Replaced_Percentage)])
  } else {
    data.table::setnames(combined_replace, old = c("Replaced_1_Percentage", "Replaced_2_Percentage"), new = c("String_NA_Replaced_Percentage", "Actual_NA_Replaced_Percentage"))
    return(combined_replace[, .(Column, Empty_Replaced_Percentage, String_NA_Replaced_Percentage, Actual_NA_Replaced_Percentage)])
  }
}
