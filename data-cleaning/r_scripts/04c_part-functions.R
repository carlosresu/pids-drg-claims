process_part <- function(
    part, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, to_write, to_group, to_sample) {
  #' @title Process Part
  #' @description Process a single part of the data, including reading, processing, and summarizing.
  #' @param part integer. The part number to process.
  #' @param ncores integer. Number of cores to use for parallel processing.
  #' @param to_view_checks logical. Whether to view checks.
  #' @param global_seed integer. Global seed for random operations.
  #' @param tmp_nrow integer. Number of intermediate rows to show.
  #' @param rvs_icd9 character. RVS ICD9 codes.
  #' @param tdrg_icd10 character. TDRG ICD10 codes.
  #' @param acc_pdx character. Accepted PDX codes.
  #' @param para logical. Whether to parallelize the process.
  #' @param to_write logical. Whether to write intermediate files.
  #' @param to_group logical. Whether to group data for batch processing.
  #' @param to_sample logical. Whether to read sample files instead of full partial files.
  #' @return list. A list containing the processed data and summary.

  start_time <- Sys.time()

  # Ensure partial and sample files exist
  ensure_partial_files_exist(part)
  if (to_sample) ensure_sample_files_exist(part)

  # Read the appropriate file
  read_result <- read_appropriate_file(part, to_sample)
  dt <- read_result$dt
  replacement_sumamry <- read_result$replacement_summary

  result <- parallelize_and_summarize_data(
    dt, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel
  )
  dt <- result$dt
  combined_summary <- result$combined_summary

  combined_summary$replacement_summary <- replacement_sumamry

  if (to_write) write_intermediate_file(to_write, part, dt)

  if (to_group) export_for_batch_grouper(dt, year_to_load, output_txt_file(part))

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  return(list(dt = dt, combined_summary = combined_summary, processing_time = processing_time))
}
