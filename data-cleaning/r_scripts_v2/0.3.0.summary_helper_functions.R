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

safe_access <- function(s, field) {
  if (is.list(s) && field %in% names(s)) s[[field]] else NULL
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

  data.table(
    modified_matches = safe_unlist(all_modified_matches),
    modified_match = safe_unlist(all_modified_match)
  )[, .(count = .N), by = .(modified_matches, modified_match)]
}

safe_extract <- function(summary, field) {
  tryCatch(summary[[field]], error = function(e) NULL)
}

combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}

combine_replace_empty_tables <- function(rboundlist) {
  combined <- rboundlist
  if (nrow(combined) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }
  return(combined[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)])
}

final_combine_replace_empty_tables <- function(rboundlist, samplesizedivisor = sample_size_divisor, splitparts = split_parts, totalrows = total_rows) {
  # Calculate the sample size based on the divisor
  samplesize <- ceiling(totalrows / samplesizedivisor)

  combined_replace_empty <- rboundlist

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  # Ensure numeric conversion for all relevant columns
  combined_replace_empty[, `:=`(
    Empty_Replaced = as.numeric(Empty_Replaced),
    NA_Replaced = as.numeric(NA_Replaced),
    Character0_Replaced = as.numeric(Character0_Replaced)
  )]

  # Calculate total elements: use sample size * splitparts or totalrows as fallback
  total_elements <- samplesize * splitparts

  # Aggregate replacement counts by column
  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]

  # Calculate and cap percentages at 100%
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = pmin((Total_Empty_Replaced / total_elements) * 100, 100),
    NA_Replaced_Percentage = pmin((Total_NA_Replaced / total_elements) * 100, 100),
    Character0_Replaced_Percentage = pmin((Total_Character0_Replaced / total_elements) * 100, 100)
  )]

  # Format percentages with two decimal places
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    NA_Replaced_Percentage = sprintf("%.2f%%", NA_Replaced_Percentage),
    Character0_Replaced_Percentage = sprintf("%.2f%%", Character0_Replaced_Percentage)
  )]

  # Return the formatted table
  return(combined_replace_empty[, .(
    Column, Empty_Replaced_Percentage, NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}
