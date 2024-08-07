ensure_partial_files_exist <- function(part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a
  #' given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- partial_claims_file
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (part - 1) * rows_per_part + 1
    end_row <- min(part * rows_per_part, total_rows)
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

ensure_sample_files_exist <- function(part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  if (!file.exists(sampled_claims_file)) {
    dt <- fread(
      here(raw_claims_parts_path, paste0(
        full_claims_prefix, year_to_load,
        "_part_", sprintf("%02d", part), "_of_", split_parts, ".csv"
      )),
      skip = 1, na.strings = na_values,
      colClasses = "character", header = FALSE, encoding = encode, sep = sep
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(full_header))
    fwrite(dt, sampled_claims_file, quote = TRUE)
  }
}

read_appropriate_file <- function(part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample)
  #' for a given part, drops specified columns, and casts column types.
  #' @param part integer. The part number to process.
  #' @param to_sample logical. Whether to read the sample file or the
  #' full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", part), "_of_", split_parts, ".csv"
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

  nrow_start[[part]] <<- nrow(dt)

  return(
    list(
      read_result_dt = dt,
      read_result_replacement_summary = replacement_summary
    )
  )
}

read_and_save_partial <- function(start_row, end_row, part) {
  #' @title Read and Save Partial Files
  #' @description This function reads and saves partial files from the
  #' full claims file.
  #' @param start_row integer. The starting row number.
  #' @param end_row integer. The ending row number.
  #' @param part integer. The part number of the file.
  #' @return NULL. The function is used for its side effect of reading and
  #' saving partial files.

  # cat(paste("Reading header from:", file = full_claims_file()))
  # cat(paste("Partial file path:", partial_claims_file))
  # cat(paste("Start row:", start_row, "End row:", end_row))

  dt <- NULL
  if (!file.exists(partial_claims_file)) {
    cat("Partial file does not exist. Creating partial file...")
    dt <- fread(
      file = full_claims_file,
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = start_row,
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(full_header))
    cat(paste("Number of rows read:", nrow(dt)))
    if (nrow(dt) > 0) {
      cat(paste("Writing partial file to:", partial_claims_file))
      fwrite(dt, partial_claims_file, quote = TRUE)
    } else {
      cat("No rows to save")
    }
  } else {
    cat(paste(
      "Partial file already exists. Skipping creation:",
      partial_claims_file
    ))
    dt <- fread(partial_claims_file,
      na.strings = na_values, colClasses = "character",
      encoding = encode, sep = sep
    )
  }

  if (to_sample) {
    sampled_claims_file_path <- sampled_claims_file
    cat(paste("Sampled file path:", sampled_file_path))
    if (!file.exists(sampled_file_path)) {
      cat("Sampled file does not exist. Creating new sample...")
      if (!is.null(dt) && nrow(dt) > 0) {
        sampled_dt <- sample_data(dt)
        setnames(sampled_dt, colnames(full_header))
        cat(paste("Writing sampled file to:", sampled_file_path))
        fwrite(sampled_dt, sampled_file_path, quote = TRUE)
      } else {
        stop("Failed to read partial file or no rows available for sampling")
      }
    } else {
      cat(paste(
        "Sampled file already exists. Skipping creation:",
        sampled_file_path
      ))
    }
  }
}

# Function to export data for batch grouper
export_for_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param year_to_load integer. The year to load.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob(dt$pat_bdate, dt$pat_age, dt$date_adm)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format(mdy(dt$date_adm), "%d/%m/%Y")]
  output_dt[, TimeAdm := gsub(":", "", dt$time_adm)]
  output_dt[, DateDsc := format(mdy(dt$date_dis), "%d/%m/%Y")]
  output_dt[, TimeDsc := gsub(":", "", dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]

  icd_codes_list <- lapply(dt$clin_icd, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  rvs_codes_list <- lapply(dt$icd9_list, function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20
    codes
  })
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }

  # Write the data.table to a file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)

  if (to_dec_mem_usage) rm(output_dt) # debug
  if (to_dec_mem_usage) gc() # debug
  if (to_debug) {
    return(NULL)
  } # debug
}
