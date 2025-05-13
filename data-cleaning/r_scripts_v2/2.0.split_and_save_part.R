split_and_save_part <- function(split_loop_part) {
  # Calculate how many rows should go into each part
  rows_per_part <- ceiling(total_rows / split_parts)

  # Construct the output file path for this part
  chunk_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_loop_part),
    "_of_", split_parts, ".rds"
  ))

  # Only proceed if the chunk file doesn't already exist
  if (!file.exists(chunk_file)) {
    # Determine the range of rows to extract for this chunk
    start_row <- (split_loop_part - 1) * rows_per_part + 1
    end_row <- min(split_loop_part * rows_per_part, total_rows)

    # Slice the full dataset to get the current chunk
    chunk_dt <- full_file[start_row:end_row]

    # Save the chunk to an .rds file with compression
    saveRDS(chunk_dt, chunk_file, compress = TRUE)

    # Clean up memory
    rm(chunk_dt)
    invisible(gc()) # Trigger garbage collection quietly
  }
}
