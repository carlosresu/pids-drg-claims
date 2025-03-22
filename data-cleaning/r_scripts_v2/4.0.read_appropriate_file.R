read_appropriate_file <- function(read_part, to_sample_argument = to_sample) {
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

  dt <- readRDS(chunk_file)

  available_columns <<- colnames(dt)

  # Drop columns
  cols_to_drop <- intersect(available_columns, c(drop_cols, drop_cols_manual))
  if (length(cols_to_drop) > 0) {
    dt <- dt[, (cols_to_drop) := NULL]
  }

  nrow_start[[read_part]] <<- nrow(dt)

  ret_val <- setDT(dt)
  return(ret_val)
}
