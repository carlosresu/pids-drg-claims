ensure_sample_files_exist <- function(sample_part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given sample_part and creates them if they don't.
  #' @param sample_part integer. The sample_part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  #'
  set.seed(global_seed)
  if (!file.exists(sampled_claims_file)) {
    dt <- readRDS(
      here(raw_claims_parts_path, paste0(
        full_claims_prefix, year_to_load,
        "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
      ))
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    # setnames(dt, colnames(full_header))
    saveRDS(dt, sampled_claims_file, compress = FALSE)
  }
}

read_appropriate_file <- function(read_part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample)
  #' for a given read_part, drops specified columns, and casts column types.
  #' @param read_part integer. The read_part number to process.
  #' @param to_sample logical. Whether to read the sample file or the
  #' full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  }

  dt <- readRDS(chunk_file)

  available_columns <- colnames(dt)

  if (to_debug) print(head(dt), 2) # debug

  # Drop columns
  if (any(drop_cols %in% available_columns)) {
    dt <- dt[, (drop_cols) := NULL]
  }

  if (any(drop_cols_manual %in% available_columns)) {
    dt <- dt[, (drop_cols_manual) := NULL]
  }


  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  replacement_summary <- replace_result$return_replacement_summary

  if (to_debug) print(head(dt), 2) # debug

  ## Apply column classes only to the columns that exist in the data
  col_classes <- sapply(available_columns, function(col) {
    if (col %in% unlist(expected_types["character"])) {
      return("character")
    }
    if (col %in% unlist(expected_types["integer"])) {
      return("integer")
    }
    if (col %in% unlist(expected_types["factor"])) {
      return("factor")
    }
    if (col %in% unlist(expected_types["numeric"])) {
      return("numeric")
    }
  })

  if (to_debug) print(col_classes)

  # Cast column types with checks
  for (col in names(col_classes)) {
    original_values <- dt[[col]]

    dt[[col]] <- switch(col_classes[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = {
        levels <- unique(dt[[col]])
        as.factor(dt[[col]])
      },
      "integer" = {
        suppressWarnings(as.integer(dt[[col]]))
      },
      "numeric" = {
        suppressWarnings(as.numeric(dt[[col]]))
      },
      dt[[col]] # Default case: no conversion if unrecognized type
    )

    # Check for NA coercion
    coerced_to_na <- which(is.na(dt[[col]]) & !is.na(original_values))
    if (length(coerced_to_na) > 0) {
      cat(sprintf(
        "Column '%s' coerced %d values to NA. First few original values: %s\n",
        col, length(coerced_to_na), paste(original_values[coerced_to_na][1:5],
          collapse = ", "
        )
      ))
    }
  }

  nrow_start[[read_part]] <<- nrow(dt)

  if (to_debug) print(paste0("Available Columns: ", available_columns))

  # # Inspect a few rows before and after conversion
  if (to_debug) print(head(dt$ADMISSION_TIME))
  if (to_debug) print(head(dt$DISCHARGE_TIME))

  return(
    list(
      read_result_dt = dt,
      read_result_replacement_summary = replacement_summary
    )
  )
}

export_for_grouper <- function(dt, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt_thai <- data.table()

  # Create CASEID column
  output_dt_thai[, CASEID := as.character(1:nrow(dt))]

  # Format Date of Birth (DOB)
  output_dt_thai[, DOB := as.character(format(as.Date(dt$pat_bdate), "%d/%m/%Y"))]

  # Ensure both data.tables have the correct keys for joining
  setkey(output_dt_thai, CASEID)
  setkey(pat_bdate_recomputed, id_series)

  # Step 1: Capture rows where DOB is NA and will be filled with recomputed DOB (before updating)
  recomputed_dob_rows_before <- output_dt_thai[
    is.na(DOB) & CASEID %in% pat_bdate_recomputed$id_series,
    .(CASEID, DOB_before = DOB)
  ]

  # Print rows before the DOB update
  cat("Rows where DOB will be updated from recomputed values (Before):\n")
  print(recomputed_dob_rows_before)

  # Step 2: Join recomputed DOB values
  output_dt_thai <- merge(output_dt_thai, pat_bdate_recomputed, by.x = "CASEID", by.y = "id_series", all.x = TRUE, all.y = FALSE)

  # Step 3: Update DOB with recomputed DOB where applicable
  output_dt_thai[
    is.na(DOB) & !is.na(pat_bdate_recomputed),
    DOB := as.character(format(as.Date(pat_bdate_recomputed), "%d/%m/%Y"))
  ]

  # Step 4: Capture rows after the DOB update (only the updated ones)
  recomputed_dob_rows_after <- output_dt_thai[
    CASEID %in% recomputed_dob_rows_before$CASEID,
    .(CASEID, DOB_after = DOB)
  ]

  # Print rows after the DOB update
  cat("Rows where DOB was updated from recomputed values (After):\n")
  print(recomputed_dob_rows_after)

  # Format DOB
  output_dt_thai[, pat_bdate_recomputed := NULL]
  # Format Sex
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]

  # Format Admission Date and Time
  output_dt_thai[, DateAdm := format(as.Date(dt$date_adm), "%d/%m/%Y")]
  output_dt_thai[, TimeAdm := format(as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M")]

  # Format Discharge Date and Time
  output_dt_thai[, DateDsc := format(as.Date(dt$date_dis), "%d/%m/%Y")]
  output_dt_thai[, TimeDsc := format(as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M")]

  # Discharge Type
  output_dt_thai[, DischT := dt$clin_discharge]

  # Admission Weight
  output_dt_thai[, AdmWt := dt$pat_bwt]

  # Principal Diagnosis Code
  output_dt_thai[, PDx := dt$clin_pdx]

  # Secondary Diagnosis Codes (SDx1 to SDx12)
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12 # Ensure there are 12 elements
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt_thai[, (icd_cols) := icd_codes]

  # Procedure Codes (Proc1 to Proc20)
  proc_codes_list <- lapply(dt$clin_proc, function(proc_str) {
    codes <- unlist(proc_str)
    length(codes) <- 20 # Ensure there are 20 elements
    codes
  })
  proc_codes <- as.data.table(do.call(rbind, proc_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt_thai[, (proc_cols) := proc_codes]

  # Replace NA values with '--'
  output_dt_thai[is.na(output_dt_thai)] <- "--"

  str(output_dt_thai)

  # Write the data.table to a file with vertical bar (|) as delimiter
  fwrite(output_dt_thai, output_txt_file, sep = "|", col.names = TRUE)

  if (to_debug) {
    return(NULL)
  }
}
