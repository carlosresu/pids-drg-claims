ensure_partial_files_exist <- function(partial_part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a
  #' given partial_part and creates them if they don't.
  #' @param partial_part integer. The partial_part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- partial_claims_file
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (partial_part - 1) * rows_per_part + 1
    end_row <- min(partial_part * rows_per_part, total_rows)
    dt <- fread(
      file = full_claims_file,
      skip = start_row,
      nrows = end_row - start_row + 1,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(full_header))
    fwrite(dt, chunk_file, quote = TRUE)
  }
}

ensure_sample_files_exist <- function(sample_part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given sample_part and creates them if they don't.
  #' @param sample_part integer. The sample_part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  #'
  set.seed(global_seed)
  if (!file.exists(sampled_claims_file)) {
    dt <- fread(
      here(raw_claims_parts_path, paste0(
        full_claims_prefix, year_to_load,
        "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".csv"
      )),
      skip = 1, na.strings = na_values,
      colClasses = "character", header = FALSE, encoding = encode, sep = sep
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(full_header))
    fwrite(dt, sampled_claims_file, quote = TRUE)
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
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".csv"
    ))
  }

  dt <- fread(chunk_file,
    na.strings = na_values, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

  if (to_debug) print(head(dt), 2) # debug

  # Drop columns
  if (any(drop_cols %in% colnames(dt))) {
    dt <- dt[, (drop_cols) := NULL]
  }

  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  replacement_summary <- replace_result$return_replacement_summary

  if (to_debug) print(head(dt), 2) # debug

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
      dt[[col]]
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

  return(
    list(
      read_result_dt = dt,
      read_result_replacement_summary = replacement_summary
    )
  )
}

# # Function to export data for batch grouper
# export_for_grouper <- function(
#   dt, year_to_load, output_txt_file #, loop_part
# ) {
#   #' @title Export Data for Batch Grouper
#   #'
#   #' @description This function exports data for batch grouper,
#   #' generating necessary columns and formatting them accordingly.
#   #'
#   #' @param dt data.table. The input data table.
#   #' @param year_to_load integer. The year to load.
#   #' @param output_txt_file character. The path to the output text file.
#   #'
#   #' @return NULL.

#   output_dt <- data.table(CASEID = 1:nrow(dt))
#   output_dt[, DOB := generate_dob(dt$pat_bdate, dt$pat_age, dt$date_adm)]
#   output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
#   output_dt[, DateAdm := format(ymd(dt$date_adm), "%d/%m/%Y")]
#   output_dt[, TimeAdm := gsub(":", "", dt$time_adm)]
#   output_dt[, DateDsc := format(ymd(dt$date_dis), "%d/%m/%Y")]
#   output_dt[, TimeDsc := gsub(":", "", dt$time_dis)]
#   output_dt[, DischT := dt$clin_discharge]
#   output_dt[, AdmWt := dt$pat_bwt]
#   output_dt[, PDx := dt$pdx]

#   icd_codes_list <- lapply(dt$clin_icd, function(icd_str) {
#     codes <- unlist(icd_str)
#     length(codes) <- 12
#     codes
#   })
#   icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
#   icd_cols <- paste0("SDx", 1:12)
#   output_dt[, (icd_cols) := icd_codes]

#   rvs_codes_list <- lapply(dt$icd9_list, function(rvs_str) {
#     codes <- unlist(rvs_str)
#     length(codes) <- 20
#     codes
#   })
#   rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
#   proc_cols <- paste0("Proc", 1:20)
#   output_dt[, (proc_cols) := rvs_codes]

#   # Replace NA values with '--'
#   output_dt[is.na(output_dt)] <- "--"
#   # Convert list columns to comma-separated strings
#   for (col in names(output_dt)) {
#     if (is.list(output_dt[[col]])) {
#       output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
#     }
#   }

#   # Write the data.table to a file
#   # if (to_combine) master_grouper_input_list[[loop_part]] <<- output_dt
#   fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)

#   if (to_dec_mem_usage) rm(output_dt) # debug
#   if (to_dec_mem_usage) gc() # debug
#   if (to_debug) {
#     return(NULL)
#   } # debug
# }

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

  output_dt <- data.table(CASEID = 1:nrow(dt))
  # Format Date of Birth (DOB) and Age
  output_dt[, DOB := format(ymd(dt$pat_bdate), "%d/%m/%Y")]

  # Format Sex
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]

  # Format Admission Date and Time
  output_dt[, DateAdm := format(ymd(dt$date_adm), "%d/%m/%Y")]
  output_dt[, TimeAdm := format(as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M")]

  # Format Discharge Date and Time
  output_dt[, DateDsc := format(ymd(dt$date_dis), "%d/%m/%Y")]
  output_dt[, TimeDsc := format(as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M")]

  # Discharge Type
  output_dt[, DischT := dt$clin_discharge]
  # Admission Weight
  output_dt[, AdmWt := dt$pat_bwt]
  # Principal Diagnosis Code
  output_dt[, PDx := dt$clin_pdx]

  # Secondary Diagnosis Codes (SDx1 to SDx12)
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  # Procedure Codes (Proc1 to Proc20)
  rvs_codes_list <- lapply(dt$clin_rvs, function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20
    codes
  })
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  output_dt[is.null(output_dt)] <- "--"
  # # Convert list columns to comma-separated strings
  # for (col in names(output_dt)) {
  #   if (is.list(output_dt[[col]])) {
  #     output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
  #   }
  # }
  # Write the data.table to a file with vertical bar (|) as delimiter

  str(output_dt)

  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)

  if (to_debug) {
    return(NULL)
  }
}
