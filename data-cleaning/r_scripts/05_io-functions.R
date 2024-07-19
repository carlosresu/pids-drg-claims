handle_sampling <- function(dt = NULL) {
  sampled_file <- sampled_claims_file(part)

  if (file.exists(sampled_file)) {
    if (to_view_checks) {
      print("Sampled file exists. Reading the sampled file...")
    }
    dt <- read_sampled_file(sampled_file)
    if (nrow(dt) != sample_size) {
      if (to_view_checks) {
        print(paste(
          "Sampled file does not match sample size. Expected:",
          sample_size, "Found:", nrow(dt), "Re-sampling..."
        ))
      }
      dt <- resample_data()
    } else if (to_view_checks) {
      print("Sampled file matches sample size.")
    }
  } else {
    if (to_view_checks) {
      print("Sampled file does not exist. Creating new sample...")
    }
    dt <- resample_data()
  }

  return(dt)
}

# Function to resample data with initial read logic
resample_data <- function() {
  dt <- read_entire_file(full_claims_file(part), initial_read = is_partial_file(part))
  dt <- sample_data(dt)
  if (to_write) {
    if (to_view_checks) {
      print(paste(
        "to_write is TRUE. Writing the new sample data to file:",
        sampled_claims_file(part)
      ))
    }
    fwrite(dt, sampled_claims_file(part))
  } else if (to_view_checks) {
    print("to_write is FALSE. Not writing the sample data to file.")
  }
  return(dt)
}

# Function to read the entire file or a specific chunk with initial reading logic
read_entire_file <- function(file, initial_read = TRUE) {
  header <- fread(file, nrows = 1)
  if (initial_read) {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      skip = 1
    )
    setnames(dt, names(header))
  } else {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      skip = 1
    )
    setnames(dt, names(header))
    dt <- dt[, (drop_cols) := NULL]
    for (col in names(col_classes_after_drop)) {
      dt[[col]] <- switch(col_classes_after_drop[[col]],
        "character" = as.character(dt[[col]]),
        "factor" = as.factor(dt[[col]]),
        "integer" = as.integer(dt[[col]]),
        "numeric" = as.numeric(dt[[col]]),
        dt[[col]]
      )
    }
  }
  return(dt)
}

# Function to read a sampled file
read_sampled_file <- function(file) {
  header <- fread(file, nrows = 1)
  dt <- fread(file,
    na.strings = na_values,
    colClasses = "character",
    header = FALSE,
    skip = 1
  )
  setnames(dt, names(header))
  dt <- dt[, (drop_cols) := NULL]
  for (col in names(col_classes_after_drop)) {
    dt[[col]] <- switch(col_classes_after_drop[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = as.factor(dt[[col]]),
      "integer" = as.integer(dt[[col]]),
      "numeric" = as.numeric(dt[[col]]),
      dt[[col]]
    )
  }
  return(dt)
}

# Function to sample data
sample_data <- function(dt) {
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

main_read_function <- function(file = NA) {
  if (is.na(file)) {
    if (to_read) {
      file <- full_claims_file(part)
      if (to_view_checks) {
        print("Reading the entire file...")
        print(paste("Full claims file path:", file))
      }
      dt <- read_entire_file(file, initial_read = is_partial_file(part))

      if (to_sample) {
        dt <- handle_sampling(dt)
      }
    } else if (to_sample) {
      dt <- handle_sampling()
    } else {
      stop("Cannot proceed: to_read is FALSE and to_sample is FALSE. At least one must be TRUE.")
    }
  } else {
    dt <- read_entire_file(file, initial_read = is_partial_file(part))
  }

  return(dt)
}

# read_and_save_partial <- function(start_row, end_row, part_num) {
#   partial_file_path <- full_claims_file(part = part_num, fileext = TRUE)
#   header <- fread(full_claims_file(), nrows = 1, header = TRUE) # Always read the header
#   if (!file_exists(partial_file_path)) {
#     skip_rows <- if (part_num == 1) start_row else start_row - 1
#     dt <- fread(full_claims_file(),
#       na.strings = na_values,
#       colClasses = "character",
#       nrows = end_row - start_row + 1,
#       skip = skip_rows,
#       header = FALSE
#     )
#     setnames(dt, colnames(header))
#     if (nrow(dt) > 0) {
#       print(paste("Saving partial file:", partial_file_path))
#       fwrite(dt, partial_file_path, quote = TRUE)
#     } else {
#       print(paste("No rows to save for part:", part_num))
#     }
#   } else {
#     print(paste("File already exists, skipping creation:", partial_file_path))
#   }
#   if (to_sample) {
#     sampled_file_path <- sampled_claims_file(part_num)
#     if (!file_exists(sampled_file_path)) {
#       print(paste("Creating sampled file:", sampled_file_path))
#       if (exists("dt") && nrow(dt) > 0) {
#         sampled_dt <- dt[sample(.N, min(sample_size, .N))]
#       } else {
#         # Read the partial file again if dt doesn't exist
#         dt <- fread(partial_file_path, na.strings = na_values, colClasses = "character")
#         sampled_dt <- dt[sample(.N, min(sample_size, .N))]
#       }
#       setnames(sampled_dt, colnames(header))
#       fwrite(sampled_dt, sampled_file_path, quote = TRUE)
#     } else {
#       print(paste("Sampled file already exists, skipping creation:", sampled_file_path))
#     }
#   }
# }

read_and_save_partial <- function(start_row, end_row, part_num) {
  partial_file_path <- full_claims_file(part = part_num, fileext = TRUE)
  header <- fread(full_claims_file(), nrows = 1, header = TRUE) # Always read the header

  if (!file_exists(partial_file_path)) {
    skip_rows <- if (part_num == 1) start_row else start_row - 1
    dt <- fread(full_claims_file(),
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = skip_rows,
      header = FALSE
    )
    setnames(dt, colnames(header))
    if (nrow(dt) > 0) {
      print(paste("Saving partial file:", partial_file_path))
      fwrite(dt, partial_file_path, quote = TRUE)
    } else {
      print(paste("No rows to save for part:", part_num))
    }
  } else {
    print(paste("File already exists, skipping creation:", partial_file_path))
  }

  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part_num)
    if (!file_exists(sampled_file_path)) {
      print(paste("Creating sampled file:", sampled_file_path))
      if (exists("dt") && nrow(dt) > 0) {
        sampled_dt <- sample_data(dt)
      } else {
        # Read the partial file again if dt doesn't exist
        dt <- read_entire_file(partial_file_path, initial_read = is_partial_file(part))
        sampled_dt <- sample_data(dt)
      }
      setnames(sampled_dt, colnames(header))
      fwrite(sampled_dt, sampled_file_path, quote = TRUE)
    } else {
      print(paste("Sampled file already exists, skipping creation:", sampled_file_path))
    }
  }
}