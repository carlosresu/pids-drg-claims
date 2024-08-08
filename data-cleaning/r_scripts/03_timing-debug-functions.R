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

# Function to print status updates using lubridate
print_status_update <- function(status_part, split_parts, processing_times) {
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param status_part integer. The current status_part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each
  #' status_part.

  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Convert time to period (using lubridate)
  convert_to_hr_min_sec <- function(seconds) {
    # Round seconds to the nearest whole number
    period <- seconds_to_period(round(seconds))
    return(period)
  }

  # Calculate elapsed and remaining time
  elapsed <- convert_to_hr_min_sec(elapsed_time)
  remaining <- convert_to_hr_min_sec(estimated_remaining_time)

  # Format period to string
  format_time <- function(period) {
    # Extract components
    h <- hour(period)
    m <- minute(period)
    s <- second(period)

    # Construct time string with labels
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))

    # Join components and return
    time_str <- paste(time_components, collapse = " ")
    return(trimws(time_str))
  }

  elapsed_str <- format_time(elapsed)
  remaining_str <- format_time(remaining)

  # Determine when to print the status update
  if (avg_time_per_part >= 4) {
    # Print status updates for every status_part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      status_part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
    # Print status updates for every 5th, 10th, 15th status_part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      status_part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  }
}

concatenate_r_files <- function(input_path, output_file) {
  #' @title Concatenate R Files
  #'
  #' @description This function concatenates all .R files in
  #' a specified directory into a single output file.
  #'
  #' @param input_path character. The directory containing the
  #' .R files to concatenate.
  #' @param output_file character. The path to the output file
  #' where the concatenated content will be written.
  #'
  #' @return NULL.

  # List all .R files in the directory
  r_files <- list.files(
    path = input_path,
    pattern = "\\.R$", full.names = TRUE
  )

  # Delete the existing output file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Read and concatenate contents
  file_contents <- lapply(r_files, readLines)
  concatenated_content <- unlist(file_contents)

  # Write concatenated content to the output file
  cat(concatenated_content, file = output_file, sep = "\n")
}
