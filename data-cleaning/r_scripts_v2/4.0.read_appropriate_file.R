read_appropriate_file <- function(read_part, to_sample_argument = to_sample) {
  # Construct the file path based on whether
  # we are reading from the sampled or full dataset
  chunk_file <- if (to_sample_argument) {
    here(raw_claims_samples_path, paste0(
      "sampled_claims_", year_to_load, "_", sample_size_divisor,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  }

  # Load the data chunk from file
  dt <- readRDS(chunk_file)

  # Store the column names for external use
  available_columns <<- colnames(dt)

  # Identify and remove columns that should be dropped
  cols_to_drop <- intersect(available_columns, c(drop_cols, drop_cols_manual))
  if (length(cols_to_drop) > 0) {
    dt <- dt[, (cols_to_drop) := NULL]
  }

  # Track the number of rows loaded for this part
  nrow_start[[read_part]] <<- nrow(dt)

  # Ensure data is a data.table and return it
  ret_val <- setDT(dt)
  return(ret_val)
}
