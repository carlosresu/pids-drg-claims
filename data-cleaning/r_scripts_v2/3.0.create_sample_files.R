create_sample_files <- function(sample_part, sampled_claims_file, seed = global_seed) {
  partial_file_for_sampling <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
  ))
  dt <- readRDS(partial_file_for_sampling)
  set.seed(seed)
  dt <- dt[sample(.N, min(sample_size, .N))]
  saveRDS(dt, sampled_claims_file, compress = TRUE)
}
