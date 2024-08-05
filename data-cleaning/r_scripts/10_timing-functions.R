format_large_numbers <- function(x) {
  #' @title Format Large Numbers
  #'
  #' @description This function formats large numbers into a
  #' more readable string with units (k, m, b).
  #'
  #' @param x numeric. The number to be formatted.
  #'
  #' @return character. The formatted number as a string.

  if (x >= 1e9) {
    return(sprintf("%.1fb", x / 1e9))
  } else if (x >= 1e6) {
    return(sprintf("%.1fm", x / 1e6))
  } else if (x >= 1e3) {
    return(sprintf("%.1fk", x / 1e3))
  } else {
    return(as.character(x))
  }
}

# Function to print time estimates
print_time_estimates <- function() {
  #' @title Print Time Estimates
  #'
  #' @description This function prints time estimates for
  #' processing rows in a data table.
  #'
  #' @return NULL.
  toc_data <- toc(log = TRUE)
  total_time <- toc_data$toc - toc_data$tic
  total_rows <- if (to_sample) {
    dim_dt[1] * split_parts * sample_size_divisor
  } else {
    dim_dt[1] * split_parts
  }

  total_rows_dt <- dim_dt[1] * split_parts
  total_cells <- dim_dt[1] * dim_dt[2]
  time_per_cell <- total_time / total_cells
  time_per_row <- total_time / total_rows_dt
  time_estimate_total_rows <- time_per_row * total_rows

  # Format the row numbers
  formatted_total_rows_dt <- format_large_numbers(total_rows_dt)
  formatted_total_rows <- format_large_numbers(total_rows)

  # Print the results for processing the whole file
  # cat(sprintf(
  #   "Time spent (total) for %s rows: %2.2f sec  (actual)\n",
  #   formatted_total_rows_dt, total_time
  # ))
  cat(sprintf(
    "Time spent (t/row) for %s rows: %2.2f msec\n",
    formatted_total_rows_dt, time_per_row * 1000
  ))
  cat(sprintf(
    "Time (est) (total) for %s rows: %2.2f min\n",
    formatted_total_rows, time_estimate_total_rows / 60
  ))
}

print_status_update <- function(part, split_parts, processing_times) {
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param part integer. The current part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each part.

  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:part])
  avg_time_per_part <- elapsed_time / part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Convert time to hours, minutes, and seconds
  convert_to_hr_min_sec <- function(seconds) {
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds %% 3600) / 60)
    remaining_seconds <- round(seconds %% 60)
    return(list(hours = hours, minutes = minutes, seconds = remaining_seconds))
  }

  # Calculate elapsed and remaining time in hr:min:sec format
  elapsed <- convert_to_hr_min_sec(elapsed_time)
  remaining <- convert_to_hr_min_sec(estimated_remaining_time)

  # Construct time strings based on non-zero values
  format_time <- function(time) {
    time_str <- ""
    if (time$hours > 0) {
      time_str <- paste0(time_str, time$hours, " hr ")
    }
    if (time$minutes > 0 || time$hours > 0) {
      # Include minutes if hours are present
      time_str <- paste0(time_str, time$minutes, " min ")
    }
    time_str <- paste0(time_str, time$seconds, " sec")
    return(time_str)
  }

  elapsed_str <- format_time(elapsed)
  remaining_str <- format_time(remaining)

  # Determine when to print the status update
  if (avg_time_per_part >= 4) {
    # Print status updates for every part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  } else if (avg_time_per_part < 4 && part %% 5 == 0) {
    # Print status updates for every 5th, 10th, 15th part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  }
}
