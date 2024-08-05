split_and_save_parts <- function() {
  #' @title Split and save parts of the data
  #' @description This function splits the data into parts and
  #' saves them as separate files.
  #' @return NULL. The function is used for its side effect of
  #' splitting and saving the data.

  if (to_split) {
    rows_per_part <- ceiling(total_rows / split_parts)
    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
    if (to_split_read) {
      # Read the entire file in one go
      files_exist <- sapply(1:split_parts, function(part) {
        file.exists(full_claims_file(part))
      })
      if (any(!files_exist)) {
        full_data <- fread(full_claims_file(),
          na.strings = na_values,
          colClasses = "character", header = TRUE, encoding = encode, sep = sep
        )
      }
      split_and_save <- function(part) {
        chunk_file <- full_claims_file(part)
        if (!file.exists(chunk_file)) {
          start_row <- (part - 1) * rows_per_part + 1
          end_row <- min(part * rows_per_part, total_rows)
          dt <- full_data[start_row:end_row]
          setnames(dt, colnames(header))
          if (to_debug) print(head(dt), 2) # debug
          fwrite(dt, chunk_file, quote = TRUE)
          if (to_dec_mem_usage) rm(dt)
          if (to_dec_mem_usage) gc()
        }
      }
      lapply(1:split_parts, split_and_save)
      # Clean up the full data from memory
      if (exists("full_data")) {
        if (to_debug) print(head(full_data), 2) # debug
        if (to_dec_mem_usage) rm(full_data)
      }
      if (to_dec_mem_usage) gc()
    } else {
      split_and_save <- function(part) {
        chunk_file <- full_claims_file(part)
        if (!file.exists(chunk_file)) {
          start_row <- (part - 1) * rows_per_part + 1
          end_row <- min(part * rows_per_part, total_rows)
          dt <- fread(
            full_claims_file(),
            skip = start_row,
            nrows = end_row - start_row + 1,
            na.strings = na_values,
            colClasses = "character",
            header = FALSE,
            encoding = encode,
            sep = sep
          )
          setnames(dt, colnames(header))
          if (to_debug) print(head(dt), 2) # debug
          fwrite(dt, chunk_file, quote = TRUE)
          if (to_dec_mem_usage) rm(dt)
          if (to_dec_mem_usage) gc()
        }
      }
      lapply(1:split_parts, split_and_save)
    }
  }
}

ensure_partial_files_exist <- function(part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a
  #' given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- full_claims_file(part)
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (part - 1) * rows_per_part + 1
    end_row <- min(part * rows_per_part, total_rows)
    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
    dt <- fread(
      full_claims_file(),
      skip = start_row,
      nrows = end_row - start_row + 1,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(header))
    fwrite(dt, chunk_file, quote = TRUE)
  }
}

ensure_sample_files_exist <- function(part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  sampled_file <- sampled_claims_file(part)
  if (!file.exists(sampled_file)) {
    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
    dt <- fread(full_claims_file(part),
      skip = 1, na.strings = na_values,
      colClasses = "character", header = FALSE, encoding = encode, sep = sep
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(header))
    fwrite(dt, sampled_file, quote = TRUE)
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
    sampled_claims_file(part)
  } else {
    full_claims_file(part)
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

  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  replacement_summary <- replace_result$replacement_summary

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

  return(list(dt = dt, replacement_summary = replacement_summary))
}

# Function to suppress warnings for integer and numeric conversions
suppressedWarnings <- function(expr) {
  suppressWarnings({
    res <- eval(expr)
  })
  return(res)
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
  partial_file_path <- full_claims_file(part, fileext = TRUE)
  header <- fread(full_claims_file(),
    nrows = 1, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

  cat(paste("Reading header from:", full_claims_file()))
  cat(paste("Partial file path:", partial_file_path))
  cat(paste("Start row:", start_row, "End row:", end_row))

  dt <- NULL
  if (!file.exists(partial_file_path)) {
    cat("Partial file does not exist. Creating partial file...")
    dt <- fread(full_claims_file(),
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = start_row,
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(header))
    cat(paste("Number of rows read:", nrow(dt)))
    if (nrow(dt) > 0) {
      cat(paste("Writing partial file to:", partial_file_path))
      fwrite(dt, partial_file_path, quote = TRUE)
    } else {
      cat("No rows to save")
    }
  } else {
    cat(paste(
      "Partial file already exists. Skipping creation:",
      partial_file_path
    ))
    dt <- fread(partial_file_path,
      na.strings = na_values, colClasses = "character",
      encoding = encode, sep = sep
    )
  }

  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part)
    cat(paste("Sampled file path:", sampled_file_path))
    if (!file.exists(sampled_file_path)) {
      cat("Sampled file does not exist. Creating new sample...")
      if (!is.null(dt) && nrow(dt) > 0) {
        sampled_dt <- sample_data(dt)
        setnames(sampled_dt, colnames(header))
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

write_intermediate_file <- function(to_write, part, dt) {
  #' @title Write Intermediate File
  #' @description This function writes the intermediate data table to a file.
  #' @param part integer. The part number of the data being processed.
  #' @param dt data.table. The data table to be written.
  #' @return NULL. The function is used for its side effect of writing the
  #' data table to a file.
  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE), quote = TRUE)
  }
}
