process_chunk <- function(
    chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  #' @title Process and map clinical data chunk
  #'
  #' @description This function processes a data chunk by cleaning
  #' the data, mapping ICD codes, replacing empty strings with NA,
  #' and applying primary diagnosis logic. It returns the processed
  #' chunk along with a summary of the processing steps.
  #'
  #' @param chunk data.table The input data chunk containing clinical
  #' data to be processed.
  #' @param to_view_checks logical If TRUE, enables viewing checks for
  #' debugging. If FALSE, suppresses output.
  #' @param rvs_icd9 data.frame Mapping data for RVS to ICD-9 codes.
  #' @param tdrg_icd10 data.frame Mapping data for ICD-10 codes.
  #' @param acc_pdx data.frame Data for primary diagnosis (PDX) application.
  #'
  #' @return list A list containing the processed data chunk and a
  #' summary of the processing steps.

  if (to_view_checks) {
    # cat("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  clean_result <- clean_data(chunk)
  chunk <- clean_result$data

  summary <- list(
    rename_success = clean_result$rename_success,
    ICD_replacements_1 = clean_result$ICD_replacements_1,
    ICD_replacements_2 = clean_result$ICD_replacements_2,
    pat_type_mapped = clean_result$pat_type_mapped,
    pat_memcat_parent_mapped = clean_result$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = clean_result$pat_memcat_child_mapped,
    clin_discharge_mapped = clean_result$clin_discharge_mapped,
    claim_status_mapped = clean_result$claim_status_mapped,
    pat_type_unmapped = clean_result$pat_type_unmapped,
    memcat_parent_unmapped = clean_result$memcat_parent_unmapped,
    memcat_child_unmapped = clean_result$memcat_child_unmapped,
    discharge_unmapped = clean_result$discharge_unmapped,
    claim_status_unmapped = clean_result$claim_status_unmapped,
    discard_rvs_one = clean_result$discard_rvs_one,
    discard_rvs_two = clean_result$discard_rvs_two,
    empty_strings_replaced_1 = clean_result$empty_strings_replaced_1
  )

  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- implement_icd10_mapping(
    clin_c1, clin_c2, clin_icd, tdrg_icd10
  )
  chunk[, clin_c1 := icd10_mapping_result$clin_c1]
  chunk[, clin_c2 := icd10_mapping_result$clin_c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
  chunk <- chunk_replace_result$data
  summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

  pdx_result <- apply_find_pdx(
    chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Ensure consistent lengths of clin_rvs and icd9_list
  clin_rvs_len <- lengths(chunk$clin_rvs)
  icd9_list_len <- lengths(chunk$icd9_list)

  max_len <- max(c(clin_rvs_len, icd9_list_len))
  chunk$clin_rvs <- lapply(chunk$clin_rvs, function(x) {
    length(x) <- max_len
    x
  })
  chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
    length(x) <- max_len
    x
  })

  summary$unique_icds <- icd10_mapping_result$unique_icds
  summary$direct_matches <- icd10_mapping_result$direct_matches
  summary$unmatched <- icd10_mapping_result$unmatched
  summary$unmatched_sources <- icd10_mapping_result$unmatched_sources
  summary$icd10_map_dt <- icd10_mapping_result$icd10_map_dt

  summary$rvss <- rvs_mapping_result$rvss
  summary$mappable_rvs <- rvs_mapping_result$mappable_rvs
  summary$unmappable_rvs <- rvs_mapping_result$unmappable_rvs
  summary$multi_mapped_rvs <- rvs_mapping_result$multi_mapped_rvs
  summary$without_drg <- rvs_mapping_result$without_drg

  if (to_dec_mem_usage) gc() # debug
  return(list(chunk = chunk, summary = summary))
}

parallelize_and_summarize_data <- function(
    dt, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, diff_chars) {
  #' @title Parallelize and summarize data processing
  #'
  #' @description This function parallelizes the data processing
  #' across multiple cores and summarizes the results.
  #'
  #' Main processing step; calls process_chunk
  #' with or without parallelization
  #' Process chunk does (per chunk):
  #' 1. Clean data
  #' 2. Maps RVS
  #' 3. Maps ICD
  #' 4. Replaces empty strings
  #' 5. Finds PDXs
  #' 6. Returns chunk and chunk summaries

  chunk_size <- ceiling(nrow(dt) / ncores)
  chunks <- split(dt, rep(1:ncores, each = chunk_size, length.out = nrow(dt)))

  if (to_parallel) {
    parallel_results <- future_lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx,
      future.seed = global_seed
    )
  } else {
    parallel_results <- lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx
    )
  }

  processed_chunks <- lapply(parallel_results, function(res) res$chunk)

  dt <- rbindlist(processed_chunks)

  if (to_dec_mem_usage) rm(processed_chunks) # debug

  combined_summary <- combine_chunk_summaries(parallel_results, tmp_nrow, diff_chars)

  if (to_dec_mem_usage) rm(parallel_results) # debug
  if (to_dec_mem_usage) gc() # debug

  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  invalid_pdx_indices <- which(
    !is.na(dt$pdx) & dt$pdx != "" & !sapply(dt$pdx, function(x) exists(x, acc_pdx_env))
  )

  if (length(invalid_pdx_indices) > 0) {
    cat(paste("Invalid PDx found:", dt$pdx[invalid_pdx_indices]))
    combined_summary$pdx_success <- FALSE
  } else {
    combined_summary$pdx_success <- TRUE
  }

  if (to_dec_mem_usage) gc() # debug

  return(list(
    dt = dt,
    combined_summary = combined_summary
  ))
}
