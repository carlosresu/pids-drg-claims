# 01.02.01 Function to print status updates using lubridate
print_status_update <- function(status_part,
                                split_parts,
                                processing_times,
                                phase) {
  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # See 2.A.A below for function definition of convert_to_hms

  # Calculate elapsed and remaining time
  elapsed <- convert_to_hms(elapsed_time)
  remaining <- convert_to_hms(estimated_remaining_time)

  # See 2.A.B for function definition of format_time

  elapsed_str <- format_time(elapsed)
  remaining_str <- format_time(remaining)

  # Determine when to print the status update
  if (phase == "split") {
    if (avg_time_per_part >= 4) {
      # Print status updates for every status_part
      cat(sprintf(
        "\rFinished splitting %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
      # Print status updates for every 5th, 10th, 15th status_part
      cat(sprintf(
        "\rFinished splitting %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    }
  } else if (phase == "clean") {
    if (avg_time_per_part >= 4) {
      # Print status updates for every status_part
      cat(sprintf(
        "\rFinished cleaning %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
      # Print status updates for every 5th, 10th, 15th status_part
      cat(sprintf(
        "\rFinished cleaning %d of %d parts in %s (ETA %s)       ",
        status_part, split_parts, elapsed_str, remaining_str
      ))
      flush.console()
    }
  }
}
