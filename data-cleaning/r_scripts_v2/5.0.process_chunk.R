process_chunk <- function(chunk,
                          yr_to_load = year_to_load,
                          col_maps = column_mappings,
                          manual_patterns_to_repl = manual_patterns_to_replace,
                          manual_code_repl = manual_code_replacements,
                          known_vals = known_values,
                          rvsicd9 = rvs_icd9,
                          thai_icd10 = tdrg_icd10,
                          accpdx = acc_pdx,
                          remap_cols = remapped_column) {
  # Step 1: Perform data cleaning and transformation on the chunk

  available_columns <- colnames(chunk)

  # Convert source year to integer if needed
  if (!"ADMISSION_YEAR" %in% available_columns) {
    chunk[, SRC_YR := as.integer(yr_to_load)]
  }

  # Rename columns based on col_maps
  old_names <- available_columns[available_columns %in% names(col_maps)]
  new_names <- sapply(old_names, function(col) col_maps[[col]])
  setnames(chunk, old = old_names, new = new_names)
  rename_success <- all(new_names %in% colnames(chunk))

  chunk[, id_series := trimws(id_series)]
  chunk[, id_pin := trimws(id_pin)]
  chunk[, c1 := clin_c1]
  chunk[, c2 := clin_c2]

  # Convert time columns
  chunk[, time_adm := ifelse(
    grepl("AM|PM", time_adm),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_adm), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
    time_adm
  )]
  chunk[, time_dis := ifelse(
    grepl("AM|PM", time_dis),
    format(as.POSIXct(sub("\\.\\d+ ", " ", time_dis), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
    time_dis
  )]

  # Identify ICD and RVS columns
  icd_col_names <- grep("^clin_icd\\d+$", names(chunk), value = TRUE)
  rvs_col_names <- grep("^clin_rvs\\d+$", names(chunk), value = TRUE)
  clin_icd_cols <- lapply(icd_col_names, function(col_name) chunk[[col_name]])
  clin_rvs_cols <- lapply(rvs_col_names, function(col_name) chunk[[col_name]])

  # Collapse and clean ICD and RVS columns
  col_list <- collapse_and_clean_icd_rvs_cols(clin_icd_cols, clin_rvs_cols)
  chunk[, clin_icd := col_list$clin_icd]
  chunk[, clin_rvs := col_list$clin_rvs]
  chunk[, (icd_col_names) := NULL]
  chunk[, (rvs_col_names) := NULL]

  # Clean and compare c1 and c2 columns
  c1_result <- clean_column(chunk$c1)
  chunk[, c1_orig := c1]
  chunk[, c1 := c1_result$cleaned_col]
  is_covid_c1 <- c1_result$is_covid

  c1_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c1_orig, toString),
    new_code = sapply(chunk$c1, toString)
  )[old_code != new_code, .(old_code, new_code, count = .N), by = .(old_code, new_code)]

  c2_result <- clean_column(chunk$c2)
  chunk[, c2_orig := c2]
  chunk[, c2 := c2_result$cleaned_col]
  is_covid_c2 <- c2_result$is_covid

  c2_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c2_orig, toString),
    new_code = sapply(chunk$c2, toString)
  )[old_code != new_code, .(old_code, new_code, count = .N), by = .(old_code, new_code)]

  # Update the is_covid flag
  chunk[, is_covid := is_covid_c1 | is_covid_c2]

  # Apply manual replacements
  manual_replacement <- function(text) {
    stri_replace_all_regex(text, manual_patterns_to_repl, manual_code_repl, vectorize_all = FALSE)
  }
  chunk[, clin_icd := lapply(clin_icd, manual_replacement)]
  chunk[, c1 := lapply(c1, manual_replacement)]
  chunk[, c2 := lapply(c2, manual_replacement)]

  chunk[, c1 := split_to_vector(remove_lumped_icd_codes(c1))]
  chunk[, c2 := split_to_vector(remove_lumped_icd_codes(c2))]

  # Concatenate clin_icd with c1 and c2
  chunk[, clin_icd := lapply(seq_len(.N), function(i) c(clin_icd[[i]], c1[[i]], c2[[i]]))]

  # Process RVS codes
  c1_results <- append_and_remove_rvs(chunk$clin_rvs, chunk$c1, rvsicd9)
  chunk[, clin_rvs := c1_results$clin_rvs]
  chunk[, c1 := c1_results$col]
  c1_discarded_rvs <- c1_results$discarded_rvs

  c2_results <- append_and_remove_rvs(chunk$clin_rvs, chunk$c2, rvsicd9)
  chunk[, clin_rvs := c2_results$clin_rvs]
  chunk[, c2 := c2_results$col]
  c2_discarded_rvs <- c2_results$discarded_rvs

  # Replace empty strings with NA and remap patient data
  replace_result <- replace_empty_with_na(chunk)
  chunk <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  # Call the remap_patient_data function
  remap_res <- remap_patient_data(
    pat_type = chunk$pat_type,
    pat_memcat_parent = chunk$pat_memcat_parent,
    pat_memcat_child = chunk$pat_memcat_child,
    clin_discharge = chunk$clin_discharge,
    claim_status = chunk$claim_status,
    known_values = known_vals,
    remapped_column = remap_cols
  )

  # Assign the remapped columns back to chunk
  chunk[, pat_type := remap_res$remapped$pat_type]
  chunk[, pat_memcat_parent := remap_res$remapped$pat_memcat_parent]
  chunk[, pat_memcat_child := remap_res$remapped$pat_memcat_child]
  chunk[, clin_discharge := remap_res$remapped$clin_discharge]
  chunk[, claim_status := remap_res$remapped$claim_status]

  # Step 3: DEPRECATED

  # Define function to map RVS codes to ICD9 codes

  # Step 4:
  # Map clinical RVS (Relative Value Scale) codes to ICD9 using 'rvsicd9'
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs)
  # Store the mapped ICD9 list into the chunk
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  # Step 5: DEPRECATED

  # Step 6: Extract columns c1, c2, and clin_icd for ICD10 mapping
  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd

  # Step 7:
  # Perform ICD10 mapping using the extracted columns
  # and 'thai_icd10' mapping data
  icd10_mapping_result <- map_icd10(
    c1, c2, clin_icd
  )

  # Update chunk with the mapped ICD10 codes
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  # Step 8:
  # Replace any empty strings with NA values,
  # returning a summary of replacements
  # See cleaning-functions.R
  res2 <- replace_empty_with_na(dt = chunk)
  # Update chunk with cleaned data
  chunk <- res2$return_data
  # Store replacement summary
  empty_strings_replaced_2 <- res2$return_replacement_summary

  # Step 9: Define a function to remove all whitespace from character vectors

  # Step 10:
  # Apply the remove_whitespace function to the
  # list columns 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  # Step 11: DEPRECATED

  # Step 12: Apply a function to find the primary
  # diagnosis (pdx) based on 'c1', 'c2', and 'clin_icd'
  pdx_result <- find_pdx(
    chunk$c1, chunk$c2, chunk$clin_icd
  )

  # Step 13: Store the primary diagnosis (pdx) and its code into the chunk
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Step 14: Define a function to remove the primary
  # diagnosis (pdx) from list columns (c1, c2, clin_icd)

  # Step 15: Apply the 'remove_pdx_from_list' function
  # to each row of 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], c1[[i]]))
  )]
  chunk[, c2 := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], c2[[i]]))
  )]
  chunk[, clin_icd := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], clin_icd[[i]]))
  )]

  # Step 16: Create a summary by combining clean
  # results and ICD10 mapping information
  chunk_summary <- list(
    # Summary from the column renaming and cleaning steps
    rename_success = rename_success,
    ICD_replacements_1 = c1_cleaning_comparison,
    ICD_replacements_2 = c2_cleaning_comparison,

    # Mapped values from remapping
    pat_type_mapped = remap_res$pat_type_mapped,
    pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
    clin_discharge_mapped = remap_res$clin_discharge_mapped,
    claim_status_mapped = remap_res$claim_status_mapped,

    # Unmapped values from remapping
    pat_type_unmapped = remap_res$pat_type_unmapped,
    memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
    memcat_child_unmapped = remap_res$memcat_child_unmapped,
    discharge_unmapped = remap_res$discharge_unmapped,
    claim_status_unmapped = remap_res$claim_status_unmapped,

    # Discarded RVS data
    discard_rvs_one = c1_discarded_rvs,
    discard_rvs_two = c2_discarded_rvs,
    empty_strings_replaced_1 = empty_strings_replaced_1,

    # ICD10 mapping results
    unique_icds = icd10_mapping_result$unique_icds,
    direct_matches = icd10_mapping_result$direct_matches,
    unmatched = icd10_mapping_result$unmatched,
    unmatched_sources = icd10_mapping_result$unmatched_sources,
    icd10_map_dt = icd10_mapping_result$icd10_map_dt,

    # RVS mapping results
    rvss = rvs_mapping_result$rvss,
    mappable_rvs = rvs_mapping_result$mappable_rvs,
    unmappable_rvs = rvs_mapping_result$unmappable_rvs,
    multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
    without_drg = rvs_mapping_result$without_drg
  )

  # Track invalid age corrections
  invalid_age_before <- nrow(chunk[pat_age < -1 | pat_age > 124, .(id_series)])

  fwrite(
    chunk[pat_age <= -1, .(id_series, pat_age, c1, c2)],
    here(debug_path, "age_less_than_or_equal_to_neg_one.csv")
  )

  fwrite(
    chunk[pat_age < 0 & pat_age > -1, .(id_series, pat_age, c1, c2)],
    here(debug_path, "age_between_zero_and_neg_one.csv")
  )

  # Use c1_orig and c2_orig as clin_c1 and clin_c2
  chunk[, clin_c1 := c1_orig]
  chunk[, clin_c2 := c2_orig]
  chunk[, c("c1_orig", "c2_orig") := NULL]

  # Use icd9_list as clin_proc
  chunk[, clin_proc := icd9_list]
  chunk[, icd9_list := NULL]

  # Remove the primary diagnosis from the
  # list of secondary diagnoses
  chunk[, clin_icd := Map(
    function(pdx_var,
             sdx_var) {
      sdx_var[sdx_var != pdx_var]
    }, pdx, clin_icd
  )]
  chunk[, clin_sdx := clin_icd]
  chunk[, clin_icd := NULL]

  if (!"pat_bdate" %in% colnames(chunk)) {
    chunk[, pat_bdate := NA_Date_]
  }

  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )

  chunk[, (date_cols) := lapply(
    .SD,
    function(x) {
      converted_dates <- as.Date(x, format = "%m/%d/%Y")
      # Replace dates before 1900-01-01 with NA
      converted_dates[converted_dates < as.Date("1900-01-01")] <- NA_Date_
      return(converted_dates)
    }
  ), .SDcols = date_cols]

  # Process time columns
  time_cols <- c("time_adm", "time_dis")

  chunk[, (time_cols) := lapply(
    .SD,
    function(x) {
      # Ensure valid times with "HH:MM:00" format
      x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
      as.character(x) # No need to convert to ITime if output is HH:MM:SS
    }
  ), .SDcols = time_cols]

  # Convert date_adm and date_dis from Asia/Manila to UTC
  chunk[, date_adm := as.POSIXct(
    paste(date_adm, time_adm),
    format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
  )]
  chunk[, date_dis := as.POSIXct(
    paste(date_dis, time_dis),
    format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
  )]

  # Process logical columns
  chunk[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
  chunk[, clin_emergency := as.logical(as.integer(clin_emergency))]

  # Process numeric columns
  if (!"pat_bwt" %in% colnames(chunk)) {
    chunk[, pat_bwt := NA_real_]
  }
  num_cols <- c(
    "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
    "claim_charge", "id_year", "pdx_code"
  )

  chunk[, (num_cols) := lapply(
    .SD,
    as.numeric
  ), .SDcols = num_cols]


  # Process integer columns
  int_cols <- c("clin_discharge", "id_year", "pdx_code")

  chunk[, (int_cols) := lapply(
    .SD,
    as.integer
  ), .SDcols = int_cols]

  # Process character columns
  char_cols <- c(
    "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
    "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx"
  )

  chunk[, (char_cols) := lapply(
    .SD,
    as.character
  ), .SDcols = char_cols]

  # initialize age
  chunk[, pat_ageday := NA_integer_]

  # START OF AGE AND BDAY CORRECTION
  # Age correction logic
  invalid_ages_before_correction <- chunk[pat_age < 0 | pat_age > 124, .N]
  invalid_age_ids_before <- chunk[pat_age < 0 | pat_age > 124, id_series]


  # Step 1: Fix pat_age for specific ranges
  chunk[!is.na(pat_age) & pat_age > 0, pat_age := floor(pat_age)]
  # Set ages between -1 and 0 to 0
  chunk[pat_age < 0 & pat_age >= -1, pat_age := 0]
  # Set pat_age to NA if greater than 124 or less than -1
  chunk[pat_age < -1 | pat_age > 124, pat_age := NA_integer_]

  # Step 2: Recalculate pat_age only if necessary
  # Subset the rows that meet the condition before recalculation
  recalculated_rows <- chunk[
    !is.na(pat_bdate) & !is.na(pat_age) &
      pat_age != floor(as.numeric(
        as.Date(date_adm) - pat_bdate
      ) / 365.25)
  ]

  # Print the rows where recalculation is going to happen (before recalculation)
  # cat("Rows where pat_age is being recalculated (Before):\n")
  # print(recalculated_rows[, .(pat_bdate, date_adm, pat_age)])

  # Perform the recalculation and store the
  # new values in a separate column for comparison
  chunk[
    !is.na(pat_bdate) & !is.na(pat_age) &
      pat_age != floor(as.numeric(
        as.Date(date_adm) - pat_bdate
      ) / 365.25),
    pat_age_recalculated := floor(as.numeric(
      as.Date(date_adm) - pat_bdate
    ) / 365.25)
  ]

  # Show before and after recalculated pat_age
  # cat("Before and After Recalculation:\n")
  # print(chunk[
  #   !is.na(pat_age_recalculated),
  #   .(pat_bdate, date_adm, pat_age, pat_age_recalculated)
  # ])

  # Save pat_age_recalculated to pat_age,
  # then delete pat_age_recalculated
  chunk[
    !is.na(pat_age_recalculated) & pat_age_recalculated > 0,
    pat_age := pat_age_recalculated
  ]
  # Remove the recalculated column
  chunk[, pat_age_recalculated := NULL]

  # Regenerate or correct DOB
  invalid_bdate_before <- chunk[is.na(pat_bdate), .N]
  invalid_bdate_ids_before <- chunk[is.na(pat_bdate), id_series]

  # Save invalid age rows to CSV
  invalid_age_path <- here("data-cleaning", "debug", "invalid_age.csv")
  fwrite(data.table(id_series = invalid_age_ids_before), invalid_age_path)

  # Save invalid birthdate rows to CSV
  invalid_bdate_path <- here("data-cleaning", "debug", "invalid_bdate.csv")
  fwrite(data.table(id_series = invalid_bdate_ids_before), invalid_bdate_path)

  # Print messages for invalid ages corrected
  invalid_ages_after_correction <- chunk[pat_age < 0 | pat_age > 124, .N]
  #   message(
  #     "Number of invalid ages corrected: ",
  #     invalid_ages_before_correction - invalid_ages_after_correction,
  #     ". Invalid ages are those with a value less than 0 or greater than 124,
  # which were reset to NA or corrected."
  #   )
  # END OF AGE AND BDAY CORRECTION

  # # Assuming acc_icd_env is an environment containing acc_icd codes
  # acc_icd_env <- new.env(hash = TRUE, parent = emptyenv())
  # for (code in acc_icd) {
  #   assign(code, TRUE, envir = acc_icd_env)
  # }

  # Modify the data.table operation to use mget with the acc_icd_env
  acc_icd_set <- unique(acc_icd) # Ensure acc_icd is a unique vector
  chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
    valid_codes <- codes[codes %in% acc_icd_set]
    if (length(valid_codes) > 0) {
      return(valid_codes)
    } else {
      return(NA_character_)
    }
  })]

  # Optionally unlist each element of clin_sdx
  chunk[, clin_sdx := lapply(
    clin_sdx,
    function(x) if (is.null(x)) character(0) else unlist(x)
  )]

  na_replaced_result <- replace_empty_with_na(chunk)

  chunk <- na_replaced_result$return_data

  # Process character columns and convert to UTF-8
  chunk[, (char_cols) := lapply(
    .SD,
    function(col) iconv(col, from = "", to = "UTF-8")
  ), .SDcols = char_cols]

  # Identify and process character columns
  char_cols <- names(chunk)[sapply(chunk, is.character)]

  # Apply parallel processing for character columns (if on Unix-like systems)
  chunk[, (char_cols) := lapply(.SD, function(col) {
    # Replace "None" and empty strings with NA
    col[col %in% c("None", "")] <- NA_character_
    return(col)
  }), .SDcols = char_cols]

  # Identify and process numeric columns
  num_cols <- names(chunk)[sapply(chunk, is.numeric)]

  # Apply parallel processing for numeric columns
  chunk[, (num_cols) := lapply(.SD, function(col) {
    # Replace NaN values with NA
    col[is.nan(col)] <- NA_real_
    return(col)
  }), .SDcols = num_cols]

  # Identify and process list columns
  list_cols <- names(chunk)[sapply(chunk, is.list)]

  # Apply parallel processing for list columns
  chunk[, (list_cols) := lapply(.SD, function(col) {
    # Replace "None" and empty strings in character elements of lists
    lapply(col, function(x) {
      if (is.character(x)) x[x %in% c("None", "")] <- NA_character_
      return(x)
    })
  }), .SDcols = list_cols]

  # Convert string columns to arrays, handling different delimiters:
  # comma, comma with space, single pipe, and double pipe
  array_columns <- c("id_hcp")
  # Regex pattern to handle commas, single pipes, and double pipes
  split_pattern <- "\\s*,\\s*|\\|\\||\\|"

  chunk[, (array_columns) := lapply(.SD, function(x) {
    # Split based on the specified pattern
    # (comma, comma with space, single pipe, or double pipe)
    x <- strsplit(x, split_pattern)
    # Handle empty or NA entries
    lapply(x, function(y) {
      if (length(y) == 0L || all(is.na(y))) {
        character(0)
      } else {
        y
      }
    })
  }), .SDcols = array_columns]

  # Ensure 'clin_sdx', 'clin_proc', and 'id_hcp' are not NULL
  list_columns <- c("clin_sdx", "clin_proc", "id_hcp")
  chunk[, (list_columns) := lapply(.SD, function(col) {
    lapply(col, function(x) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) {
        character(0)
      } else {
        x
      }
    })
  }), .SDcols = list_columns]


  setnames(chunk, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))

  setcolorder(chunk, c(
    "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
    "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
    "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
    "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent",
    "pat_memcat_child", "is_covid", "claim_status", "claim_payout",
    "claim_charge", "clin_discharge", "clin_outpatient", "clin_emergency",
    "clin_acc", "clin_c1", "c1", "clin_c2", "c2", "clin_sdx", "clin_proc",
    "clin_rvs", "clin_pdx", "clin_pdx_source"
  ))

  chunk[, clin_discharge := as.integer(clin_discharge)]

  # Step 17: Optionally trigger garbage collection to reduce memory usage
  gc()

  # Step 18: Return the processed chunk and summary information

  # Step N: Return the final chunk and summary
  return(list(
    return_chunk = chunk,
    return_summary = chunk_summary
  ))
}
