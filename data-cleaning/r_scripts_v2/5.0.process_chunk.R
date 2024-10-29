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
  # Step 1: Initialize variables and check for required columns

  # Get the list of available columns in the chunk
  available_columns <- colnames(chunk)

  # If 'ADMISSION_YEAR' is not in the chunk, add 'SRC_YR' column with the source year
  if (!"ADMISSION_YEAR" %in% available_columns) {
    chunk[, SRC_YR := as.integer(yr_to_load)]
  }

  # Step 2: Rename columns based on column mappings

  # Find columns that need to be renamed
  old_names <- available_columns[available_columns %in% names(col_maps)]
  # Get the new names for those columns
  new_names <- sapply(old_names, function(col) col_maps[[col]])
  # Rename the columns in the chunk
  setnames(chunk, old = old_names, new = new_names)
  # Check if all new names are successfully renamed
  renamesuccess <- all(new_names %in% colnames(chunk))

  # Step 3: Trim whitespace in certain columns

  # Trim whitespace from 'id_series' and 'id_pin'
  chunk[, id_series := trimws(id_series)]
  chunk[, id_pin := trimws(id_pin)]

  # Step 4: Duplicate 'clin_c1' and 'clin_c2' to 'c1' and 'c2'

  chunk[, c1 := clin_c1]
  chunk[, c2 := clin_c2]
  # Step 5: Convert 'time_adm' and 'time_dis' columns to proper time format

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

  # Step 6: Collapse and clean ICD and RVS columns

  # Identify ICD columns (columns that match the pattern 'clin_icd<number>')
  icd_col_names <- grep("^clin_icd\\d+$", names(chunk), value = TRUE)
  # Identify RVS columns (columns that match the pattern 'clin_rvs<number>')
  rvs_col_names <- grep("^clin_rvs\\d+$", names(chunk), value = TRUE)

  # Extract the ICD columns
  clin_icd_cols <- lapply(icd_col_names, function(col_name) chunk[[col_name]])
  # Extract the RVS columns
  clin_rvs_cols <- lapply(rvs_col_names, function(col_name) chunk[[col_name]])

  # Collapse and clean the ICD and RVS columns using 'collapse_and_clean_icd_rvs_cols' function
  col_list <- collapse_and_clean_icd_rvs_cols(clin_icd_cols, clin_rvs_cols)

  # Update 'clin_icd' and 'clin_rvs' columns in the chunk with the cleaned data
  chunk[, clin_icd := col_list$clin_icd]

  # print(chunk[all(!is.na(clin_icd)),clin_icd])

  chunk[, clin_rvs := col_list$clin_rvs]

  # Remove the original individual ICD and RVS columns from the chunk
  chunk[, (icd_col_names) := NULL]
  chunk[, (rvs_col_names) := NULL]

  # Step 7: Clean 'c1' and 'c2' columns
  # Helper function to remove periods for comparison
  remove_periods_and_whitespaces <- function(x) {
    x <- gsub("\\.", "", x)
    x <- gsub("\\s", "", x)
    return(x)
  }

  # Clean 'c1' column using 'clean_column' function
  c1_result <- clean_column(chunk$c1)
  chunk[, c1_orig := c1]
  chunk[, c1 := c1_result$cleaned_col]
  is_covid_c1 <- c1_result$is_covid

  # Create a comparison table for 'c1' cleaning
  c1_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c1_orig, toString),
    new_code = sapply(chunk$c1, toString)
  )[
    remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code), # Exclude changes due to periods and whitespace
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]

  # Clean 'c2' column using 'clean_column' function
  c2_result <- clean_column(chunk$c2)
  chunk[, c2_orig := c2]
  chunk[, c2 := c2_result$cleaned_col]
  is_covid_c2 <- c2_result$is_covid

  # Create a comparison table for 'c2' cleaning
  c2_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c2_orig, toString),
    new_code = sapply(chunk$c2, toString)
  )[
    remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code), # Exclude changes due to periods and whitespace
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]

  # Update 'is_covid' flag based on 'c1' and 'c2'
  chunk[, is_covid := is_covid_c1 | is_covid_c2]

  # Step 8: Apply manual replacements to ICD codes

  # Define a function for manual replacement using patterns and replacements
  manual_replacement <- function(text) {
    stri_replace_all_regex(text, manual_patterns_to_repl, manual_code_repl, vectorize_all = FALSE)
  }

  # Apply manual replacements to 'clin_icd', 'c1', and 'c2' columns
  chunk[, clin_icd := lapply(clin_icd, manual_replacement)]
  chunk[, c1 := lapply(c1, manual_replacement)]
  chunk[, c2 := lapply(c2, manual_replacement)]

  # Step 9: Remove lumped ICD codes and split into vectors

  chunk[, c1 := split_to_vector(remove_lumped_icd_codes(c1))]
  chunk[, c2 := split_to_vector(remove_lumped_icd_codes(c2))]

  # Step 10: Concatenate 'clin_icd', 'c1', and 'c2' into a single list per row

  chunk[, clin_icd := lapply(seq_len(.N), function(i) c(clin_icd[[i]], c1[[i]], c2[[i]]))]

  # Step 11: Process RVS codes in 'c1' and 'c2'

  # Process 'c1' to append RVS codes and remove them from 'c1'
  c1_results <- append_and_remove_rvs(chunk$clin_rvs, chunk$c1, rvsicd9)
  chunk[, clin_rvs := c1_results$clin_rvs]
  chunk[, c1 := c1_results$col]
  c1_discarded_rvs <- c1_results$discarded_rvs

  # Process 'c2' to append RVS codes and remove them from 'c2'
  c2_results <- append_and_remove_rvs(chunk$clin_rvs, chunk$c2, rvsicd9)
  chunk[, clin_rvs := c2_results$clin_rvs]
  chunk[, c2 := c2_results$col]
  c2_discarded_rvs <- c2_results$discarded_rvs

  # Step 12: Replace empty strings with NA in the chunk

  replace_result <- replace_empty_with_na(chunk)
  chunk <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  # Step 13: Remap patient data using 'remap_patient_data' function

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

  # Step 14: Map clinical RVS codes to ICD9 codes

  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  # Step 15: Map ICD codes to ICD10 codes using 'map_icd10' function

  # Extract columns from chunk
  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- map_icd10(c1, c2, clin_icd)

  # Update chunk with the mapped ICD10 codes
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  # Step 16: Replace empty strings with NA again

  res2 <- replace_empty_with_na(dt = chunk)
  chunk <- res2$return_data
  empty_strings_replaced_2 <- res2$return_replacement_summary

  # Step 17: Remove whitespace from 'c1', 'c2', and 'clin_icd' columns

  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  # Step 18: Find the primary diagnosis (pdx) from 'c1', 'c2', and 'clin_icd'

  pdx_result <- find_pdx(chunk$c1, chunk$c2, chunk$clin_icd)
  chunk[, pdx := pdx_result$pdx]
  chunk[, pdx_code := pdx_result$pdx_code]

  # Step 19: Remove the primary diagnosis from 'c1', 'c2', and 'clin_icd' lists

  chunk[, c1 := lapply(seq_len(.N), function(i) {
    lst <- c1[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]

  chunk[, c2 := lapply(seq_len(.N), function(i) {
    lst <- c2[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]

  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    lst <- clin_icd[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]

  # Step 20: Create a summary of the processing steps
  if (to_debug) {
    cat("rename_success: ")
    str(renamesuccess)
    cat("c1_cleaning_comparison: ")
    str(c1_cleaning_comparison)
    cat("c2_cleaning_comparison: ")
    str(c2_cleaning_comparison)
    cat("remap_res$pat_type_mapped: ")
    str(remap_res$pat_type_mapped)
    cat("remap_res$pat_memcat_parent_mapped: ")
    str(remap_res$pat_memcat_parent_mapped)
    cat("remap_res$pat_memcat_child_mapped: ")
    str(remap_res$pat_memcat_child_mapped)
    cat("remap_res$clin_discharge_mapped: ")
    str(remap_res$clin_discharge_mapped)
    cat("remap_res$claim_status_mapped: ")
    str(remap_res$claim_status_mapped)
    cat("remap_res$pat_type_unmapped: ")
    str(remap_res$pat_type_unmapped)
    cat("remap_res$memcat_parent_unmapped: ")
    str(remap_res$memcat_parent_unmapped)
    cat("remap_res$memcat_child_unmapped: ")
    str(remap_res$memcat_child_unmapped)
    cat("remap_res$discharge_unmapped: ")
    str(remap_res$discharge_unmapped)
    cat("remap_res$claim_status_unmapped: ")
    str(remap_res$claim_status_unmapped)
    cat("c1_discarded_rvs: ")
    str(c1_discarded_rvs)
    cat("c2_discarded_rvs: ")
    str(c2_discarded_rvs)
    cat("empty_strings_replaced_1: ")
    str(empty_strings_replaced_1)
    cat("empty_strings_replaced_2: ")
    str(empty_strings_replaced_2)
    cat("icd10_mapping_result$unique_icds: ")
    str(icd10_mapping_result$unique_icds)
    cat("icd10_mapping_result$direct_match_count: ")
    str(icd10_mapping_result$direct_match_count)
    cat("icd10_mapping_result$unmatched_codes: ")
    str(icd10_mapping_result$unmatched_codes)
    cat("icd10_mapping_result$unmatched_sources: ")
    str(icd10_mapping_result$unmatched_sources)
    cat("icd10_mapping_result$icd10_map_dt: ")
    str(icd10_mapping_result$icd10_map_dt)
    cat("rvs_mapping_result$rvss: ")
    str(rvs_mapping_result$rvss)
    cat("rvs_mapping_result$mappable_rvs: ")
    str(rvs_mapping_result$mappable_rvs)
    cat("rvs_mapping_result$unmappable_rvs: ")
    str(rvs_mapping_result$unmappable_rvs)
    cat("rvs_mapping_result$multi_mapped_rvs: ")
    str(rvs_mapping_result$multi_mapped_rvs)
    cat("rvs_mapping_result$without_drg: ")
    str(rvs_mapping_result$without_drg)
  }

  chunk_summary <- list(
    rename_success = renamesuccess,
    ICD_replacements_1 = c1_cleaning_comparison,
    ICD_replacements_2 = c2_cleaning_comparison,
    pat_type_mapped = remap_res$pat_type_mapped,
    pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
    clin_discharge_mapped = remap_res$clin_discharge_mapped,
    claim_status_mapped = remap_res$claim_status_mapped,
    pat_type_unmapped = remap_res$pat_type_unmapped,
    memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
    memcat_child_unmapped = remap_res$memcat_child_unmapped,
    discharge_unmapped = remap_res$discharge_unmapped,
    claim_status_unmapped = remap_res$claim_status_unmapped,
    discard_rvs_one = c1_discarded_rvs,
    discard_rvs_two = c2_discarded_rvs,
    empty_strings_replaced_1 = empty_strings_replaced_1,
    empty_strings_replaced_2 = empty_strings_replaced_2,
    unique_icds = icd10_mapping_result$unique_icds,
    direct_matches = icd10_mapping_result$direct_matches,
    unmatched_codes = icd10_mapping_result$unmatched_codes,
    unmatched_sources = icd10_mapping_result$unmatched_sources,
    icd10_map_dt = icd10_mapping_result$icd10_map_dt,
    modified_matches = icd10_mapping_result$modified_matches,
    rvss = rvs_mapping_result$rvss,
    mappable_rvs = rvs_mapping_result$mappable_rvs,
    unmappable_rvs = rvs_mapping_result$unmappable_rvs,
    multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
    without_drg = rvs_mapping_result$without_drg
  )

  # Step 21: Track invalid age corrections

  # Count the number of records with invalid ages before correction
  invalid_age_before <- nrow(chunk[pat_age < -1 | pat_age > 124, .(id_series)])

  # Save records with age less than or equal to -1 to a CSV file
  fwrite(
    chunk[pat_age <= -1, .(id_series, pat_age, c1, c2)],
    here(debug_path, "age_less_than_or_equal_to_neg_one.csv")
  )

  # Save records with age between -1 and 0 to a CSV file
  fwrite(
    chunk[pat_age < 0 & pat_age > -1, .(id_series, pat_age, c1, c2)],
    here(debug_path, "age_between_zero_and_neg_one.csv")
  )

  # Step 22: Prepare columns for output

  # Use 'c1_orig' and 'c2_orig' as 'clin_c1' and 'clin_c2'
  chunk[, clin_c1 := c1_orig]
  chunk[, clin_c2 := c2_orig]
  chunk[, c("c1_orig", "c2_orig") := NULL]

  # Use 'icd9_list' as 'clin_proc' and remove 'icd9_list'
  chunk[, clin_proc := icd9_list]
  chunk[, icd9_list := NULL]

  # Remove the primary diagnosis from 'clin_icd' and store in 'clin_sdx'
  chunk[, clin_icd := Map(function(pdx_var, sdx_var) {
    sdx_var[sdx_var != pdx_var]
  }, pdx, clin_icd)]
  chunk[, clin_sdx := clin_icd]
  chunk[, clin_icd := NULL]

  # Step 23: Ensure 'pat_bdate' column exists

  if (!"pat_bdate" %in% colnames(chunk)) {
    chunk[, pat_bdate := NA_Date_]
  }

  # Step 24: Convert date columns to Date format

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

  # Step 25: Process time columns

  time_cols <- c("time_adm", "time_dis")

  chunk[, (time_cols) := lapply(
    .SD,
    function(x) {
      # Ensure valid times with "HH:MM:00" format
      x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
      as.character(x) # No need to convert to ITime if output is HH:MM:SS
    }
  ), .SDcols = time_cols]

  # Step 26: Convert 'date_adm' and 'date_dis' to POSIXct datetime objects

  chunk[, date_adm := as.POSIXct(
    paste(date_adm, time_adm),
    format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
  )]
  chunk[, date_dis := as.POSIXct(
    paste(date_dis, time_dis),
    format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
  )]

  # Step 27: Process logical columns

  chunk[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
  chunk[, clin_emergency := as.logical(as.integer(clin_emergency))]

  # Step 28: Process numeric columns

  if (!"pat_bwt" %in% colnames(chunk)) {
    chunk[, pat_bwt := NA_real_]
  }
  num_cols <- c(
    "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
    "claim_charge", "id_year", "pdx_code"
  )

  chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # Step 29: Process integer columns

  int_cols <- c("clin_discharge", "id_year", "pdx_code")

  chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]

  # Step 30: Process character columns

  char_cols <- c(
    "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
    "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx"
  )

  chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]

  # Step 31: Initialize 'pat_ageday' column

  chunk[, pat_ageday := NA_integer_]

  # Step 32: Correct invalid ages and recalculate if necessary

  # Count invalid ages before correction
  invalid_ages_before_correction <- chunk[pat_age < 0 | pat_age > 124, .N]
  invalid_age_ids_before <- chunk[pat_age < 0 | pat_age > 124, id_series]

  # Step 32a: Fix 'pat_age' for specific ranges
  chunk[!is.na(pat_age) & pat_age > 0, pat_age := floor(pat_age)]
  chunk[pat_age < 0 & pat_age >= -1, pat_age := 0]
  chunk[pat_age < -1 | pat_age > 124, pat_age := NA_integer_]

  # Step 32b: Recalculate 'pat_age' using 'pat_bdate' and 'date_adm' if necessary
  recalculated_rows <- chunk[
    !is.na(pat_bdate) & !is.na(pat_age) &
      pat_age != floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
  ]

  chunk[
    !is.na(pat_bdate) & !is.na(pat_age) &
      pat_age != floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25),
    pat_age_recalculated := floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
  ]

  # Update 'pat_age' with recalculated values if valid
  chunk[
    !is.na(pat_age_recalculated) & pat_age_recalculated > 0,
    pat_age := pat_age_recalculated
  ]
  # Remove the temporary 'pat_age_recalculated' column
  chunk[, pat_age_recalculated := NULL]

  # Step 33: Handle invalid birthdates

  invalid_bdate_before <- chunk[is.na(pat_bdate), .N]
  invalid_bdate_ids_before <- chunk[is.na(pat_bdate), id_series]

  # Save invalid age rows to CSV
  invalid_age_path <- here("data-cleaning", "debug", "invalid_age.csv")
  fwrite(data.table(id_series = invalid_age_ids_before), invalid_age_path)

  # Save invalid birthdate rows to CSV
  invalid_bdate_path <- here("data-cleaning", "debug", "invalid_bdate.csv")
  fwrite(data.table(id_series = invalid_bdate_ids_before), invalid_bdate_path)

  # Count invalid ages after correction
  invalid_ages_after_correction <- chunk[pat_age < 0 | pat_age > 124, .N]

  # Step 34: Filter 'clin_sdx' to only include codes in 'acc_icd'

  acc_icd_set <- unique(acc_icd) # Ensure 'acc_icd' is a unique vector

  chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
    valid_codes <- codes[codes %in% acc_icd_set]
    if (length(valid_codes) > 0) {
      return(valid_codes)
    } else {
      return(NA_character_)
    }
  })]

  # Unlist elements of 'clin_sdx' if necessary
  chunk[, clin_sdx := lapply(clin_sdx, function(x) if (is.null(x)) character(0) else unlist(x))]

  # Step 35: Replace empty values with NA

  na_replaced_result <- replace_empty_with_na(chunk)
  chunk <- na_replaced_result$return_data

  # Step 36: Convert character columns to UTF-8 encoding

  chunk[, (char_cols) := lapply(.SD, function(col) iconv(col, from = "", to = "UTF-8")), .SDcols = char_cols]

  # Step 37: Clean character columns by replacing 'None' and empty strings with NA

  char_cols <- names(chunk)[sapply(chunk, is.character)]

  chunk[, (char_cols) := lapply(.SD, function(col) {
    col[col %in% c("None", "")] <- NA_character_
    return(col)
  }), .SDcols = char_cols]

  # Step 38: Clean numeric columns by replacing NaN with NA

  num_cols <- names(chunk)[sapply(chunk, is.numeric)]

  chunk[, (num_cols) := lapply(.SD, function(col) {
    col[is.nan(col)] <- NA_real_
    return(col)
  }), .SDcols = num_cols]

  # Step 39: Clean list columns by replacing 'None' and empty strings with NA in elements

  list_cols <- names(chunk)[sapply(chunk, is.list)]

  chunk[, (list_cols) := lapply(.SD, function(col) {
    lapply(col, function(x) {
      if (is.character(x)) x[x %in% c("None", "")] <- NA_character_
      return(x)
    })
  }), .SDcols = list_cols]

  # Step 40: Convert string columns to arrays, handling different delimiters

  array_columns <- c("id_hcp")
  split_pattern <- "\\s*,\\s*|\\|\\||\\|"

  chunk[, (array_columns) := lapply(.SD, function(x) {
    x <- strsplit(x, split_pattern)
    lapply(x, function(y) {
      if (length(y) == 0L || all(is.na(y))) {
        character(0)
      } else {
        y
      }
    })
  }), .SDcols = array_columns]

  # Step 41: Ensure certain list columns are not NULL

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

  # Step 42: Rename columns for output consistency

  setnames(chunk, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))

  # Step 43: Reorder columns

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

  # Step 44: Convert 'clin_discharge' to integer

  chunk[, clin_discharge := as.integer(clin_discharge)]

  # Step 45: Trigger garbage collection to reduce memory usage

  gc()

  # Step 46: Return the processed chunk and summary information

  return(list(
    return_chunk = chunk,
    return_summary = chunk_summary
  ))
}
