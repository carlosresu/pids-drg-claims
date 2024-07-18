# Function to read and save partial data with header row
read_and_save_partial <- function(start_row, end_row, part_num) {
  header <- fread(full_claims_file(), nrows = 1, header = TRUE)
  skip_rows <- if (part_num == 1) start_row else start_row - 1
  dt <- fread(full_claims_file(),
    na.strings = na_values,
    colClasses = "character",
    nrows = end_row - start_row + 1,
    skip = skip_rows,
    header = FALSE
  )
  setnames(dt, colnames(header))
  partial_file_path <- full_claims_file(part = part_num, fileext = TRUE)
  print(paste("Saving partial file:", partial_file_path))
  fwrite(dt, partial_file_path, quote = TRUE)
  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part_num)
    print(paste("Creating sampled file:", sampled_file_path))
    sampled_dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(sampled_dt, colnames(header))
    fwrite(sampled_dt, sampled_file_path, quote = TRUE)
  }
}
