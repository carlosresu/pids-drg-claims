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
