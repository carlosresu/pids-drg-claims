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
print_time_estimates <- function(dt, total_time, total_rows) {
  #' @title Print Time Estimates
  #'
  #' @description This function prints time estimates for
  #' processing rows in a data table.
  #'
  #' @param dt data.table. The input data table.
  #' @param total_time numeric. The total time spent processing.
  #' @param total_rows numeric. The total number of rows to estimate for.
  #'
  #' @return NULL.

  total_rows_dt <- nrow(dt) * split_parts
  total_cells <- nrow(dt) * ncol(dt)
  time_per_cell <- total_time / total_cells
  time_per_row <- total_time / total_rows_dt
  time_estimate_total_rows <- time_per_row * total_rows

  # Format the row numbers
  formatted_total_rows_dt <- format_large_numbers(total_rows_dt)
  formatted_total_rows <- format_large_numbers(total_rows)

  # Print the results for processing the whole file
  cat(sprintf(
    "Time spent (total) for %s rows:  %1.2f sec  (actual)\n",
    formatted_total_rows_dt, total_time
  ))
  cat(sprintf(
    "Time spent (t/row) for %s rows:  %1.2f msec (actual)\n",
    formatted_total_rows_dt, time_per_row * 1000
  ))
  cat(sprintf(
    "Time spent (total) for %s rows: %2.2f min  (estimate)\n",
    formatted_total_rows, time_estimate_total_rows / 60
  ))
}

print_status_update <- function(part, split_parts, processing_times) {
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param part integer. The current part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each part.
  elapsed_time <- sum(processing_times[1:part])
  avg_time_per_part <- elapsed_time / part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time
  cat(sprintf(
    "Status Update: Finished processing part %d of %d\n",
    part, split_parts
  ))
  cat(sprintf("ETA: %d seconds\n", round(estimated_remaining_time)))
}
