# Define process_chunk (not to be confused with process_part)
# that processes each part in nthreads chunks
process_chunk <- function(chunk,
                          to_view_checks = to_view_checks,
                          rvs_icd9 = rvs_icd9,
                          tdrg_icd10 = tdrg_icd10,
                          acc_pdx = acc_pdx) {
  # Step 1: Define main clean data function,
  # which does majority of the data cleaning on the claims file

  # Step 2: Clean the data in the 'chunk'
  # See function(s) above
  clean_result <- clean_data(chunk)
  chunk <- clean_result$return_data # Update chunk with cleaned data

  # Step 3: DEPRECATED

  # Define function to map RVS codes to ICD9 codes

  # Step 4:
  # Map clinical RVS (Relative Value Scale) codes to ICD9 using 'rvs_icd9'
  # See function(s) above
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
  # and 'tdrg_icd10' mapping data
  # See function(s) above
  icd10_mapping_result <- implement_icd10_mapping(
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
  # See function(s) above
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  # Step 11: DEPRECATED

  # Step 12: Apply a function to find the primary
  # diagnosis (pdx) based on 'c1', 'c2', and 'clin_icd'
  # See function(s) above
  pdx_result <- apply_find_pdx(
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
      x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
      as.ITime(x)
    }
  ), .SDcols = time_cols]

  # Convert ITime object to character in HH:MM:SS format
  chunk[, time_adm := strftime(time_adm, format = "%H:%M:%S")]
  chunk[, time_dis := strftime(time_dis, format = "%H:%M:%S")]

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
  if (!"pat_bwt" %in% colnames(dt)) {
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
        seconds(as.Date(date_adm) - pat_bdate)
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
        seconds(as.Date(date_adm) - pat_bdate)
      ) / 365.25),
    pat_age_recalculated := floor(as.numeric(
      seconds(as.Date(date_adm) - pat_bdate)
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

  # Assuming acc_icd_env is an environment containing acc_icd codes
  acc_icd_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_icd) {
    assign(code, TRUE, envir = acc_icd_env)
  }

  # Modify the data.table operation to use mget with the acc_icd_env
  chunk[, clin_sdx := lapply(clin_sdx, function(row) {
    codes <- unlist(row)
    valid_codes <- codes[
      !is.na(
        mget(
          codes,
          envir = acc_icd_env,
          ifnotfound = NA_character_
        )
      )
    ]
    if (length(valid_codes) > 0) {
      return(valid_codes)
    } else {
      return(NA_character_)
    }
  })]

  # Optionally unlist each element of clin_sdx
  chunk[, clin_sdx := lapply(clin_sdx, unlist)]

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

  # Use a temporary column to avoid self-reference
  chunk[, temp_clin_discharge := as.integer(clin_discharge)]

  # Assign the temp column back to clin_discharge
  chunk[, clin_discharge := temp_clin_discharge]

  # Remove the temporary column
  chunk[, temp_clin_discharge := NULL]

  # Step 17: Optionally trigger garbage collection to reduce memory usage
  gc()

  # Step 18: Return the processed chunk and summary information
  return(
    list(
      # Return the processed chunk data
      return_chunk = chunk,
      # Return the summary for checks and outputs
      return_summary = chunk_summary
    )
  )
}
