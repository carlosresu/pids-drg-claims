clean_data <- function(dt) {
  # Add year column
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if all columns were successfully renamed
  if (!all(new_colnames %in% colnames(dt))) {
    # missing_cols <- setdiff(new_colnames, colnames(dt))
    # warning(
    #   "Failed to rename the following columns: ",
    #   paste(missing_cols, collapse = ", ")
    # )
    rename_success <- FALSE
    # stop("Column renaming failed.")
  } else {
    rename_success <- TRUE
  }

  # Collapse columns clin_icd1 to clin_icd12 into clin_icd
  dt[, clin_icd := collapse_columns(
    mget(paste0("clin_icd", 1:12), envir = as.environment(dt)),
    na_like_strings
  )]
  dt[, paste0("clin_icd", 1:12) := NULL]

  # Collapse columns clin_rvs1 to clin_rvs20 into clin_rvs
  dt[, clin_rvs := collapse_columns(
    mget(paste0("clin_rvs", 1:20), envir = as.environment(dt)),
    na_like_strings
  )]
  dt[, paste0("clin_rvs", 1:20) := NULL]

  # Remove lumped ICD codes from clin_icd
  dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]

  # Turn clin_icd and clin_rvs into lists
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]

  # Ensure clean_column function and na_like_
  # strings are correctly defined and applied
  dt[, clin_c1_orig := dt$clin_c1]
  # Ensure clin_c1_orig captures original values
  dt[, clin_c1 := clean_column(clin_c1, na_like_strings)] # Clean clin_c1

  # Generate cleaning comparison table
  clin_c1_cleaning_comparison <- dt[
    clin_c1 != clin_c1_orig,
    .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
    by = .(clin_c1_orig, clin_c1)
  ]

  # Optionally remove clin_c1_orig from dt if no longer needed
  dt[, clin_c1_orig := NULL]

  dt[, clin_c2_orig := dt$clin_c2] # Capture original clin_c2

  # Clean clin_c2 within the data.table context
  dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]

  # Create cleaning comparison table
  clin_c2_cleaning_comparison <- dt[
    clin_c2 != clin_c2_orig, # Compare cleaned clin_c2 with original
    .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
    by = .(clin_c2_orig, clin_c2)
  ]

  # Optionally remove clin_c2_orig from dt if no longer needed
  dt[, clin_c2_orig := NULL]

  dt[, clin_c1 := remove_lumped_icd_codes(dt$clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(dt$clin_c2)]

  dt[, clin_c1 := split_to_vector(clin_c1)]
  clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c1
  )
  dt[, clin_icd := clin_c1_result$clin_icd]
  dt[, clin_c1 := clin_c1_result$col_first]

  dt[, clin_c2 := split_to_vector(clin_c2)]
  clin_c2_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c2)
  dt[, clin_icd := clin_c2_result$clin_icd]
  dt[, clin_c2 := clin_c2_result$col_first]

  clin_c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c1, rvs_icd9
  )
  dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
  dt[, clin_c1 := clin_c1_rvs_results$col]
  clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]
  clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

  dt[, clin_rvs := lapply(clin_rvs, unique)]
  dedup_result <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]

  # Replace empty strings in character and factor columns with NA
  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  empty_strings_replaced_1 <- replace_result$replacement_summary

  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL

  warning_thrown <- FALSE

  # Remap and check for patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    pat_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    parent_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    child_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for clinical discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    discharge_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    discard_rvs_one = clin_c1_discarded_rvs,
    discard_rvs_two = clin_c2_discarded_rvs,
    empty_strings_replaced_1 = empty_strings_replaced_1
  ))
}

process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  if (to_view_checks) {
    # print("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  clean_result <- clean_data(chunk)
  chunk <- clean_result$data

  summary <- list()
  summary$rename_success <- clean_result$rename_success
  summary$ICD_replacements_1 <- clean_result$ICD_replacements_1
  summary$ICD_replacements_2 <- clean_result$ICD_replacements_2
  summary$pat_type_unmapped <- clean_result$pat_type_unmapped
  summary$memcat_parent_unmapped <- clean_result$memcat_parent_unmapped
  summary$memcat_child_unmapped <- clean_result$memcat_child_unmapped
  summary$discharge_unmapped <- clean_result$discharge_unmapped
  summary$discard_rvs_one <- clean_result$discard_rvs_one
  summary$discard_rvs_two <- clean_result$discard_rvs_two
  summary$empty_strings_replaced_1 <- clean_result$empty_strings_replaced_1

  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- implement_icd10_mapping(clin_c1, clin_c2, clin_icd, tdrg_icd10)
  chunk[, clin_c1 := icd10_mapping_result$clin_c1]
  chunk[, clin_c2 := icd10_mapping_result$clin_c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
  chunk <- chunk_replace_result$data
  summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

  pdx_result <- apply_find_pdx(chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx)
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

  return(list(chunk = chunk, summary = summary))
}


# Function to split and save chunks
split_and_save_chunks <- function() {
  if (to_split) {
    rows_per_part <- ceiling(total_rows / split_chunks)
    for (part in 1:split_chunks) {
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

# Function to read and process each chunk
read_and_process_chunk <- function(part) {
  chunk_file <- if (to_sample) {
    sampled_claims_file(part)
  } else {
    full_claims_file(part)
  }

  if (!file.exists(chunk_file)) {
    stop(paste("File does not exist:", chunk_file))
  }

  dt <- read_entire_file(chunk_file, initial_read = is_partial_file(chunk_file))

  if (to_sample) {
    dt <- handle_sampling(dt, part)
  }

  return(dt)
}

# Function to combine and summarize data in parallel
parallelize_and_summarize_data <- function(
    dt, num_cores, to_view_checks, global_seed, rows_to_show,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize) {
  chunk_size <- ceiling(nrow(dt) / num_cores)
  chunks <- split(dt, rep(1:num_cores,
    each = chunk_size, length.out = nrow(dt)
  ))

  if (to_parallelize) {
    # Plan for parallel processing
    plan(multisession, workers = num_cores)

    # Process each chunk in parallel
    parallel_results <- future_lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx,
      future.seed = global_seed
    )
  } else {
    # Process each chunk sequentially
    parallel_results <- lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx
    )
  }

  # Combine processed chunks
  processed_chunks <- lapply(parallel_results, function(res) res$chunk)
  dt <- rbindlist(processed_chunks)

  # Combine summaries
  summaries <- lapply(parallel_results, function(res) res$summary)
  combined_summary <- combine_all_parts_summaries(summaries, rows_to_show)

  return(list(
    dt = dt,
    combined_summary = combined_summary
  ))
}

group_data <- function(part, dt) {
  if (to_group) {
    export_for_batch_grouper(
      dt, year_to_load,
      output_txt_file(part)
    )
    for_batch_grouping <- fread(
      output_txt_file(part),
      sep = "|", na.strings = "--"
    )
    if (file.exists(grouper_result_file(part))) {
      batch_grouping_result <- fread(
        grouper_result_file(part),
        sep = "|", na.strings = "--"
      )
    }
  }
}

write_intermediate_file <- function(part, dt) {
  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE))
  }
}
