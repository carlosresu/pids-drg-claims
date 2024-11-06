split_and_save_part <- function(split_loop_part) {
  # Calculate how many rows per part
  rows_per_part <- ceiling(total_rows / split_parts)

  chunk_file <- here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_loop_part),
    "_of_", split_parts, ".rds"
  ))

  # Only process if the part does not already exist
  if (!file.exists(chunk_file)) {
    # Determine start and end rows for this chunk
    start_row <- (split_loop_part - 1) * rows_per_part + 1
    end_row <- min(split_loop_part * rows_per_part, total_rows)

    # Extract chunk of data for processing
    chunk_dt <- full_file[start_row:end_row]

    # Save the chunk as an RDS file
    saveRDS(chunk_dt, chunk_file, compress = TRUE)
    rm(chunk_dt)
    invisible(gc())

    # Save processing time for this part
    split_processing_times[[split_loop_part]] <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )

    # Print status update and estimate remaining time
    print_status_update(
      split_loop_part, split_parts, split_processing_times, "split"
    )
  }
}
