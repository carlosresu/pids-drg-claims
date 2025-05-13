create_sample_files <- function(
    sample_part,
    sampled_claims_file, seed = global_seed) {
  # Construct the path to the .rds file for the given part of the claims data
  partial_file_for_sampling <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
  ))

  # Load the claims data for this part
  dt <- readRDS(partial_file_for_sampling)

  # Set seed for reproducible sampling
  set.seed(seed)

  # Randomly sample rows (up to sample_size or total rows, whichever is smaller)
  dt <- dt[sample(.N, min(sample_size, .N))]

  # Save the sampled data to the specified file as a compressed .rds file
  saveRDS(dt, sampled_claims_file, compress = TRUE)
}
