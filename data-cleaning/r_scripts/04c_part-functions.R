split_and_save_parts <- function() {
  #' @title Split and save parts of the data
  #'
  #' @description This function splits the data into parts and
  #' saves them as separate files.
  #'
  #' @return NULL. The function is used for its side effect of
  #' splitting and saving the data.

  if (to_split) {
    rows_per_part <- ceiling(total_rows / split_parts)
    for (part in 1:split_parts) {
      chunk_file <- if (to_sample) {
        sampled_claims_file(part)
      } else {
        full_claims_file(part)
      }

      if (!file.exists(chunk_file)) {
        start_row <- (part - 1) * rows_per_part + 1
        end_row <- min(part * rows_per_part, total_rows)
        read_and_save_partial(start_row, end_row, part)
      }
    }
  }
}

read_part <- function(part) {
  #' @title Read and process a part of the data
  #'
  #' @description This function reads and processes a part
  #' of the data from a file.
  #'
  #' @param part integer. The part number of the file to read.
  #'
  #' @return data.table. The processed part of the data.

  chunk_file <- if (to_sample) {
    sampled_claims_file(part)
  } else {
    full_claims_file(part)
  }

  if (!file.exists(chunk_file)) {
    stop(paste("File does not exist:", chunk_file))
  }

  dt <- fread(chunk_file, na.strings = na_values, colClasses = col_classes)

  if (to_sample) {
    dt <- handle_sampling(dt, part)
  }

  return(dt)
}

process_part <- function(
    part, num_cores, to_view_checks, global_seed, intermediate_rows_to_show,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize, to_write, to_group) {
  #' @title Process Part
  #' @description Process a single part of the data,
  #' including reading, processing, and summarizing.
  #' @param part integer. The part number to process.
  #' @param num_cores integer. Number of cores to use for parallel processing.
  #' @param to_view_checks logical. Whether to view checks.
  #' @param global_seed integer. Global seed for random operations.
  #' @param intermediate_rows_to_show integer. Number of
  #' intermediate rows to show.
  #' @param rvs_icd9 character. RVS ICD9 codes.
  #' @param tdrg_icd10 character. TDRG ICD10 codes.
  #' @param acc_pdx character. Accepted PDX codes.
  #' @param to_parallelize logical. Whether to parallelize the process.
  #' @param to_write logical. Whether to write intermediate files.
  #' @param to_group logical. Whether to group data for batch processing.
  #' @return list. A list containing the processed data and summary.
  start_time <- Sys.time()

  # Read in the part and do initial processing
  dt <- read_part(part)

  # Main script parallelization call, mostly calls process_chunk on each chunk
  result <- parallelize_and_summarize_data(
    dt, num_cores, to_view_checks, global_seed,
    intermediate_rows_to_show, rvs_icd9, tdrg_icd10,
    acc_pdx, to_parallelize
  )

  dt <- result$dt
  combined_summary <- result$combined_summary

  # Writes out intermediate file if to_write is TRUE
  write_intermediate_file(to_write, part, dt)

  # Exports for batch grouper if to_group is TRUE
  group_data(to_group, part, dt)

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  return(
    list(
      dt = dt,
      combined_summary = combined_summary,
      processing_time = processing_time
    )
  )
}
