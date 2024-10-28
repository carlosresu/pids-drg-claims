ensure_sample_files_exist <- function(sample_part) {
  if (!file.exists(sampled_claims_file)) {
    partial_file_for_sampling <- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".fst"
    ))
    dt <- read_fst(partial_file_for_sampling, as.data.table = TRUE)
    dt <- dt[sample(.N, min(sample_size, .N))]
    write_fst(dt, sampled_claims_file, compress = 100)
  }
}
