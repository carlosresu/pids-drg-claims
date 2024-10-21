# Define main clean data function, which does majority of the data cleaning on the claims file
clean_data <- function(dt) {
  # Rename columns based on column_mappings
  available_columns <- colnames(dt)

  # Convert source year to integer
  if ("ADMISSION_YEAR" %in% available_columns) {
    # do nothing
  } else {
    dt[, SRC_YR := as.integer(year_to_load)]
  }

  dt[, is_covid := FALSE]

  # Identify the columns to rename based on the mapping
  old_names <- available_columns[available_columns %in% names(column_mappings)]
  new_names <- sapply(old_names, function(col) column_mappings[[col]])

  # Rename the columns in the data.table
  setnames(dt, old = old_names, new = new_names)

  # Check if renaming was successful
  rename_success <- all(new_names %in% available_columns)

  dt[, id_series := trimws(id_series)]

  dt[, c1 := clin_c1]
  dt[, c2 := clin_c2]

  # Convert time_adm and time_dis for 2022-2023 format

  # Strip fractional seconds and handle AM/PM conversion properly using as.POSIXct()
  dt[, time_adm := ifelse(grepl("AM|PM", time_adm),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_adm), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
    time_adm
  )]

  dt[, time_dis := ifelse(grepl("AM|PM", time_dis),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_dis), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
    time_dis
  )]

  # Collapse and clean ICD and RVS columns
  dt <- collapse_and_clean_icd_rvs(dt)

  # Helper function to clean and compare clinical columns
  clean_clinical_column <- function(col_name) {
    # Store the original column
    dt[, (paste0(col_name, "_orig")) := dt[[col_name]]]

    # Apply the clean_column function, which now returns a list of cleaned_col and is_covid
    cleaned_data <- clean_column(dt[[col_name]], na_like_strings, neoplasms_dt_actual)

    # Extract cleaned column and is_covid flag
    cleaned_col <- cleaned_data$cleaned_col
    is_covid_flag <- cleaned_data$is_covid

    # Update the cleaned column
    dt[, (col_name) := cleaned_col]

    # Store the original column as a string (for comparison purposes)
    dt[, (paste0(col_name, "_orig")) := sapply(get(paste0(col_name, "_orig")), toString)]

    # Convert the cleaned column to a string for comparison
    dt[, (col_name) := sapply(get(col_name), toString)]

    # Convert is_covid to logical (if it's currently a factor)
    dt[, is_covid := as.logical(as.character(is_covid))]

    # If is_covid is FALSE and is_covid_flag is TRUE, set is_covid to TRUE
    dt[, is_covid := ifelse(is_covid == FALSE & is_covid_flag == TRUE, TRUE, is_covid)]

    # Ensure is_covid is logical and replace any NA with FALSE
    dt[is.na(is_covid), is_covid := FALSE]

    # Compare cleaning results (only rows where the cleaned column differs from the original)
    comparison <- dt[
      !is.na(get(paste0(col_name, "_orig"))) & get(col_name) != get(paste0(col_name, "_orig")),
      .(
        old_code = get(paste0(col_name, "_orig")),
        new_code = get(col_name), count = .N
      ),
      by = .(get(paste0(col_name, "_orig")), get(col_name))
    ]

    return(comparison)
  }

  # Clean and compare c1 and c2 columns
  c1_cleaning_comparison <- clean_clinical_column("c1")
  c2_cleaning_comparison <- clean_clinical_column("c2")

  # Function to replace multiple patterns with corresponding replacements
  replace_multiple_patterns <- function(text, patterns, replacements) {
    # Ensure patterns and replacements are the same length
    if (length(patterns) != length(replacements)) {
      stop("Patterns and replacements must have the same length.")
    }

    # Perform replacements
    modified_text <- stri_replace_all_regex(
      text,
      pattern = patterns,
      replacement = replacements,
      vectorize_all = FALSE # Apply all replacements simultaneously
    )

    # return the text after modification
    return(modified_text)
  }

  # Apply multi-replacement function to implement manual replacements
  dt[, clin_icd := lapply(clin_icd,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, c1 := lapply(c1,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, c2 := lapply(c2,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]

  # Handle any lumped ICD codes by splitting them
  dt[, c1 := remove_lumped_icd_codes(c1)]
  # Handle any lumped ICD codes by splitting them
  dt[, c2 := remove_lumped_icd_codes(c2)]
  # Convert the cleaned columns into vectors
  dt[, c1 := split_to_vector(c1)]
  # Convert the cleaned columns into vectors
  dt[, c2 := split_to_vector(c2)]

  # # Remove lumped ICD codes
  # dt[, c1 := remove_lumped_icd_codes(c1)]
  # dt[, c2 := remove_lumped_icd_codes(c2)]
  # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)]

  # Clean clinical columns
  clean_clin_col_res <- clean_clinical_columns(dt)
  dt <- clean_clin_col_res$dt

  # Replace empty strings with NA and remap patient data
  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  remapping_results <- remap_patient_data(dt, to_view_checks)
  dt <- remapping_results$data
  return_summary_list <- list(
    rename_success = rename_success,
    ICD_replacements_1 = c1_cleaning_comparison,
    ICD_replacements_2 = c2_cleaning_comparison,
    pat_type_mapped = remapping_results$pat_type_mapped,
    pat_memcat_parent_mapped = remapping_results$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remapping_results$pat_memcat_child_mapped,
    clin_discharge_mapped = remapping_results$clin_discharge_mapped,
    claim_status_mapped = remapping_results$claim_status_mapped,
    pat_type_unmapped = remapping_results$pat_type_unmapped,
    memcat_parent_unmapped = remapping_results$memcat_parent_unmapped,
    memcat_child_unmapped = remapping_results$memcat_child_unmapped,
    discharge_unmapped = remapping_results$discharge_unmapped,
    claim_status_unmapped = remapping_results$claim_status_unmapped,
    discard_rvs_one = clean_clin_col_res$discard_rvs_one,
    discard_rvs_two = clean_clin_col_res$discard_rvs_two,
    empty_strings_replaced_1 = empty_strings_replaced_1
  )

  return(
    list(
      # data to return for further processing
      return_data = dt,
      # summary to return for checks and output
      return_summary = return_summary_list
    )
  )
}

# Define function to map RVS codes to ICD9 codes
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)

  return(
    list(
      # main return variable (a column) to save back to dt
      icd9_list = get_icd9_codes(clin_rvs, rvs_map_solo_env),
      # other return variables that are for checks and outputs
      rvs_map_list = rvs_maps$rvs_map_list,
      rvss = unique(unlist(clin_rvs)),
      mappable_rvs = intersect(unique(unlist(clin_rvs)), rvs_icd9$rvs),
      unmappable_rvs = setdiff(unique(unlist(clin_rvs)), rvs_icd9$rvs),
      multi_mapped_rvs = intersect(unique(unlist(clin_rvs)), names(rvs_maps$rvs_map_list)),
      without_drg = unique(rvs_icd9[!rvs %in% names(rvs_maps$rvs_map_list)]$rvs)
    )
  )
}

implement_icd10_mapping <- function(c1, c2, clin_icd, tdrg_icd10) {
  # Step 1: Get all unique ICD codes from the provided columns (c1, c2, and clin_icd)
  icds <- get_unique_icd_codes(c1, c2, clin_icd)

  # Step 2: Create an environment for Thai ICD10 codes for faster lookup
  # This uses the unique set of ICD10 codes in the tdrg_icd10 table.
  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )

  # Step 3: Create a second environment for Thai ICD10 neoplasm codes (those with slashes '/')
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  # Step 4: Find direct matches between the provided ICD codes (icds) and the Thai ICD10 environment
  direct_match_codes <- find_direct_icd_matches(
    icds, thai_icd10_env
  )

  # Step 5: Generate the full ICD10 mapping for the ICD codes,
  # considering both Thai ICD10 environment and neoplasms environment.
  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env, covid_rvs
  )
  # Extract the mapping and the count of modified mappings
  icd_mapping <- icd_mapping_info$icd_mapping_res

  modified_count <- icd_mapping_info$modified_count

  # Step 6: Identify ICD codes that were not successfully mapped.
  unmatched_icds <- setdiff(icds, names(icd_mapping))

  # Step 7: If there are unmatched ICD codes, gather their source information (c1, c2, clin_icd)
  # and the count of occurrences in each column.
  if (length(unmatched_icds) > 0) {
    unmatched_sources <- data.table(
      code = unmatched_icds, source = NA_character_, count = 0
    )
    # Loop over the columns (c1, c2, clin_icd) to fill in source and count details for unmatched codes.
    for (col_name in c("c1", "c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[
        code %in% unlist(col_values),
        source := col_name
      ]
      unmatched_sources[
        code %in% unlist(col_values),
        count := count + table(unlist(col_values))[code]
      ]
    }
    # Order unmatched codes by their occurrence count in descending order
    unmatched_sources <- unmatched_sources[order(-count)]
  } else {
    # If there are no unmatched codes, return an empty data.table.
    unmatched_sources <- data.table()
  }

  # Step 8: Create a data.table containing the mapping between PHL (input) ICD10 codes
  # and Thai DRG ICD10 codes.
  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    tdrg_icd10 = unlist(icd_mapping)
  )

  # Step 9: If debugging is enabled, save the mapping to a CSV file in the cache directory.
  if (to_debug) {
    fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  }

  # Step 10: Create an environment from the ICD10 mapping for fast lookup during column mapping.
  icd10_env <- list2env(
    setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10)
  )

  # Step 11: Apply the ICD10 mapping to the columns c1, c2, and clin_icd
  # This updates these columns based on the generated ICD10 environment.
  mapped_columns <- apply_icd10_mapping_to_columns(
    c1, c2, clin_icd, icd10_env
  )

  # Step 12: Return a list containing the mapped columns and other information for further checks and outputs:
  # - The updated columns (c1, c2, clin_icd)
  # - The full ICD10 map (icd10_map_dt)
  # - The unique ICD codes
  # - Direct matches found
  # - Unmatched ICDs and their source information
  return(
    list(
      c1 = mapped_columns$c1,
      c2 = mapped_columns$c2,
      clin_icd = mapped_columns$clin_icd,
      icd10_map_dt = icd10_map,
      unique_icds = icds,
      direct_matches = direct_match_codes,
      unmatched = unmatched_icds,
      unmatched_sources = unmatched_sources,
      icd_mapping_res = icd_mapping
    )
  )
}

##################################################################################################################################
################################################### START OF PROCESS CHUNK #######################################################
##################################################################################################################################

# Define process_chunk (not to be confused with process_part) that processes each part in nthreads chunks
process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  # Step 1: Set up environment for viewing or suppressing output
  # If 'to_view_checks' is TRUE, you can enable message viewing (commented out here).
  # Otherwise, sink (redirect output) to a temporary file to suppress output.
  if (to_view_checks) {
    # message("\rViewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  # Step 2: Clean the data in the 'chunk'
  clean_result <- clean_data(chunk)
  chunk <- clean_result$return_data # Update chunk with cleaned data
  if (to_debug) print("checkpoint 1") # Debug checkpoint
  if (to_debug) print(unique(chunk$c1)) # Print unique values in 'c1' for debugging

  # Step 3: Optionally save intermediate result to a file (debugging)
  if (to_debug) fwrite(chunk, "test1.csv")

  # Step 4: Map clinical RVS (Relative Value Scale) codes to ICD9 using 'rvs_icd9'
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  # Store the mapped ICD9 list into the chunk
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  # Step 5: Optionally save another intermediate result to a file (debugging)
  if (to_debug) fwrite(chunk, "test2.csv")

  # Step 6: Extract columns c1, c2, and clin_icd for ICD10 mapping
  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd
  if (to_debug) print("checkpoint 2") # Debug checkpoint
  if (to_debug) print(unique(c1)) # Print unique values in 'c1' for debugging

  # Step 7: Perform ICD10 mapping using the extracted columns and 'tdrg_icd10' mapping data
  icd10_mapping_result <- implement_icd10_mapping(
    c1, c2, clin_icd, tdrg_icd10
  )

  # Update chunk with the mapped ICD10 codes
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  # Step 8: Replace any empty strings with NA values, returning a summary of replacements
  res2 <- replace_empty_with_na(dt = chunk, to_view_checks)
  chunk <- res2$return_data # Update chunk with cleaned data
  empty_strings_replaced_2 <- res2$return_replacement_summary # Store replacement summary

  if (to_debug) print("checkpoint 3") # Debug checkpoint
  if (to_debug) print(unique(chunk$c1)) # Print unique values in 'c1' for debugging

  # Step 9: Define a function to remove all whitespace from character vectors
  remove_whitespace <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_) # Return NA for NULL or empty lists
    } else {
      return(gsub("\\s+", "", x)) # Remove all whitespace characters
    }
  }

  # Step 10: Apply the remove_whitespace function to the list columns 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  if (to_debug) print("checkpoint 4") # Debug checkpoint
  if (to_debug) print(unique(chunk$c1)) # Print unique values in 'c1' for debugging

  # Step 11: Optionally save a CSV file containing the column names and their types (for debugging)
  if (to_debug) fwrite(data.table(Column = colnames(chunk), Class = sapply(chunk, class)), "class.csv")

  # Step 12: Apply a function to find the primary diagnosis (pdx) based on 'c1', 'c2', and 'clin_icd'
  pdx_result <- apply_find_pdx(
    chunk$c1, chunk$c2, chunk$clin_icd, acc_pdx
  )
  # Store the primary diagnosis (pdx) and its code into the chunk
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Step 13: Optionally save another intermediate result to a file (debugging)
  if (to_debug) fwrite(chunk, "test2c.csv")
  if (to_debug) print("checkpoint 5") # Debug checkpoint
  if (to_debug) print(unique(chunk$c1)) # Print unique values in 'c1' for debugging

  # Step 14: Define a function to remove the primary diagnosis (pdx) from list columns (c1, c2, clin_icd)
  remove_pdx_from_list <- function(pdx, lst) {
    if (!is.na(pdx)) {
      # Remove the primary diagnosis from the list
      lst <- setdiff(lst, pdx)
    }
    return(lst)
  }

  # Step 15: Apply the 'remove_pdx_from_list' function to each row of 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], c1[[i]])))]
  chunk[, c2 := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], c2[[i]])))]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], clin_icd[[i]])))]

  if (to_debug) print("checkpoint 6") # Debug checkpoint
  if (to_debug) print(unique(chunk$c1)) # Print unique values in 'c1' for debugging

  # Step 16: Create a summary by combining clean results and ICD10 mapping information
  chunk_summary <- modifyList(
    clean_result$return_summary,
    list(
      unique_icds = icd10_mapping_result$unique_icds,
      direct_matches = icd10_mapping_result$direct_matches,
      unmatched = icd10_mapping_result$unmatched,
      unmatched_sources = icd10_mapping_result$unmatched_sources,
      icd10_map_dt = icd10_mapping_result$icd10_map_dt,
      rvss = rvs_mapping_result$rvss,
      mappable_rvs = rvs_mapping_result$mappable_rvs,
      unmappable_rvs = rvs_mapping_result$unmappable_rvs,
      multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
      without_drg = rvs_mapping_result$without_drg
    )
  )

  # Step 17: Optionally trigger garbage collection to reduce memory usage
  if (to_dec_mem_usage) gc() # Trigger garbage collection if needed

  # Step 18: Return the processed chunk and summary information
  return(
    list(
      return_chunk = chunk, # Return the processed chunk data
      return_summary = chunk_summary # Return the summary for checks and outputs
    )
  )
}

##################################################################################################################################
#################################################### END OF PROCESS CHUNK ########################################################
##################################################################################################################################

main_logic_func <- function() {
  # Start main execution logic
  ####################################################################################################################################
  ################################################## START OF SPLIT AND SAVE PART ####################################################
  ####################################################################################################################################

  separator <- if (file_type == ".tsv") "\t" else ","

  # Step 1: Read the header of the full claims file
  full_header <<- fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE, encoding = encode # , sep = separator
  )

  # Step 2: Check if the split part file already exists. If not, read the full claims file.
  if (!file.exists(here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_parts),
    "_of_", split_parts, ".rds"
  )))) {
    # Read the full file into memory

    full_file <<- fread(
      file = full_claims_file, colClasses = "character",
      header = TRUE, encoding = encode # , sep = separator
    )
  }

  # Function to split the file into chunks and save them
  split_and_save <- function(split_and_save_part) {
    rows_per_part <- ceiling(total_rows / split_parts) # Calculate how many rows per part
    chunk_file <- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", split_and_save_part),
      "_of_", split_parts, ".rds"
    ))

    # Only process if the part does not already exist
    if (!file.exists(chunk_file)) {
      # Determine start and end rows for this chunk
      start_row <- (split_and_save_part - 1) * rows_per_part + 1
      end_row <- min(split_and_save_part * rows_per_part, total_rows)

      # Extract chunk of data for processing
      chunk_dt <- full_file[start_row:end_row]

      # Debug print the first 2 rows if in debug mode
      if (to_debug) print(head(chunk_dt), 2)

      # Save the chunk as an RDS file
      saveRDS(chunk_dt, chunk_file, compress = TRUE)

      # Optionally reduce memory usage
      if (to_dec_mem_usage) rm(chunk_dt)
      if (to_dec_mem_usage) gc()

      # Save processing time for this part
      split_processing_times[[split_loop_part]] <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )

      # Print status update and estimate remaining time
      print_status_update(split_loop_part, split_parts, split_processing_times, "split")
    }
  }

  # Step 3: Split the file into parts and save them
  start_time <<- Sys.time() # Record start time
  for (split_loop_part in 1:split_parts) split_and_save(split_loop_part)

  ####################################################################################################################################
  ################################################## END OF SPLIT AND SAVE PART ######################################################
  ####################################################################################################################################

  # Step 4: Set up parallelization if required (Unix and non-Unix systems handled differently)
  if (to_parallel && !is_unix) plan(multisession, workers = nthreads)

  # Step 5: Loop through each part and process the partial files
  for (loop_part in 1:split_parts) {
    ##################################################################################################################################
    ################################################### START OF PROCESS PART ########################################################
    ##################################################################################################################################

    start_time <- Sys.time() # Record start time for processing
    partial_claims_file <<- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))

    # Step 6: Handle sampling logic if applicable
    if (to_sample) {
      sampled_claims_file <<- here(raw_claims_samples_path, paste0(
        "sampled_claims_", year_to_load, "_", sample_size,
        "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      ))
      ensure_sample_files_exist(loop_part) # Ensure sample files exist
    }

    # Step 7: Read the appropriate file (sample or full)
    read_result <- read_appropriate_file(loop_part, to_sample)
    read_in_dt <- read_result$read_result_dt # The data to process
    read_in_replacement_summary <- read_result$read_result_replacement_summary # Any replacements summary

    ##################################################################################################################################
    ############################################ START OF PARALLELIZE AND SUMMARIZE DATA #############################################
    ##################################################################################################################################

    # Step 8: Split the data into chunks for parallel processing
    chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
    chunks <- split(read_in_dt, rep(1:nthreads, each = chunk_size, length.out = nrow(read_in_dt)))

    # Step 9: Apply parallel processing (Unix uses 'mclapply', non-Unix uses 'future_lapply')
    if (to_parallel && is_unix) {
      if (to_debug) message("Conducting mclapply")
      parallel_results <- mclapply(
        chunks, process_chunk,
        mc.cores = nthreads,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    } else if (to_parallel && !is_unix) {
      if (to_debug) message("Conducting future_lapply")
      parallel_results <- future_lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx,
        future.seed = global_seed
      )
    } else {
      if (to_debug) message("Conducting lapply")
      parallel_results <- lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    }

    # Step 10: Combine results from all parallel chunks
    parallel_summaries <- lapply(parallel_results, function(res) res$return_summary)
    rbound_dt <- rbindlist(lapply(parallel_results, function(res) res$return_chunk))

    combined_chunk_summary <- combine_chunk_summaries(
      parallel_summaries, tmp_nrow
    )

    # Step 11: Check for invalid primary diagnoses (PDx) and update the summary
    acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
    for (code in acc_pdx) {
      assign(code, TRUE, envir = acc_pdx_env)
    }
    invalid_pdx_indices <- which(
      !is.na(rbound_dt$pdx) & rbound_dt$pdx != "" &
        !sapply(rbound_dt$pdx, function(x) exists(x, acc_pdx_env))
    )
    if (length(invalid_pdx_indices) > 0) {
      message(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
      combined_chunk_summary$pdx_success <- FALSE
    } else {
      combined_chunk_summary$pdx_success <- TRUE
    }

    if (to_dec_mem_usage) gc() # Reduce memory usage if necessary

    ##################################################################################################################################
    ############################################## END OF PARALLELIZE AND SUMMARIZE DATA #############################################
    ##################################################################################################################################

    summarized_dt <- rbound_dt # Store the summarized data
    combined_parallel_summary <- combined_chunk_summary # Store combined summary
    combined_parallel_summary$replacement_summary <- read_in_replacement_summary

    # Step 12: Write processed data to checkpoint file if required
    if (to_write) {
      saveRDS(
        summarized_dt, here(checkpoint_1_path, paste0(
          checkpoint_1_prefix, year_to_load, suffix,
          "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
        )),
        compress = TRUE
      )
    }

    # Step 13: Collect summaries for each part
    all_parts_summaries[[loop_part]] <- combined_parallel_summary
    processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    # Step 14: Update status and ETA
    print_status_update(loop_part, split_parts, processing_times, "clean")

    if (loop_part == 1) dim_dt <<- dim(summarized_dt)
    nrow_end[[loop_part]] <<- nrow(summarized_dt)

    # Step 15: Clean up memory after processing each part
    if (to_dec_mem_usage) {
      rm(read_in_dt, rbound_dt, summarized_dt)
      gc()
    }
  }

  # Step 16: Ensure that row counts match between parts
  for (nrow_part in 1:split_parts) {
    if (nrow_start[[nrow_part]] != nrow_end[[nrow_part]]) {
      warning(
        "WARNING: Row Count Mismatch! Part ", nrow_part,
        " has ", nrow_start[[nrow_part]], " starting rows and ",
        nrow_end[[nrow_part]], " ending rows\n"
      )
      stop("ERROR: Row Count Mismatch")
    }
  }
  message("\nRow Counts Match for All Parts\n")

  # Step 17: Combine all parts into a master data table if required
  if (to_combine) {
    for (read_part in 1:split_parts) {
      master_dt_list[[read_part]] <- readRDS(here(checkpoint_1_path, paste0(
        checkpoint_1_prefix, year_to_load, suffix,
        "part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
      )))
    }
    master_dt <<- rbindlist(master_dt_list)

    if (to_debug) print(head(master_dt))
    if (to_dec_mem_usage) {
      if (to_group) rm(master_dt_list) else rm(master_dt_list)
      gc()
    }
    if (to_write) {
      saveRDS(master_dt, here(checkpoint_2_path, paste0(
        checkpoint_2_prefix, year_to_load, suffix, ".rds"
      )), compress = TRUE)
    }
  }

  # Step 18: Print final summaries
  print_summary_tables(
    combine_parts_summaries(all_parts_summaries, tmp_nrow),
    end_nrow
  )

  # Step 19: End parallelization session if on non-Unix system
  if (to_parallel && !is_unix) plan(sequential)

  # Return master data table for debugging purposes
  if (to_debug) {
    return(master_dt)
  }
}
