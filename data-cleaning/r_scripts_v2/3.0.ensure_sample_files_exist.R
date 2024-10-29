ensure_sample_files_exist <- function(sample_part) {
  if (!file.exists(sampled_claims_file)) {
    partial_file_for_sampling <- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
    ))
    dt <- readRDS(partial_file_for_sampling)
    dt <- dt[sample(.N, min(sample_size, .N))]
    saveRDS(dt, sampled_claims_file, compress = TRUE)
  }
}
