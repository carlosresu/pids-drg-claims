# Function to handle sampling
handle_sampling <- function(dt = NULL, part) {
  sampled_file <- sampled_claims_file(part)

  if (file.exists(sampled_file)) {
    dt <- read_sampled_file(sampled_file)
    if (nrow(dt) != sample_size) {
      dt <- resample_data(part)
    }
  } else {
    dt <- resample_data(part)
  }
  return(dt)
}

# Function to resample data
resample_data <- function(part) {
  dt <- read_entire_file(full_claims_file(part),
    initial_read = is_partial_file(part)
  )
  dt <- sample_data(dt)
  if (to_write) {
    fwrite(dt, sampled_claims_file(part))
  }
  return(dt)
}

# Function to read the full file with all columns
read_entire_file <- function(file, initial_read = TRUE) {
  header <- fread(file, nrows = 1, header = TRUE)
  if (initial_read) {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character", header = FALSE, skip = 1
    )
    setnames(dt, names(header))
  } else {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character", header = FALSE, skip = 1
    )
    setnames(dt, names(header))
    dt <- dt[, (drop_cols) := NULL]
    for (col in names(col_classes)) {
      dt[[col]] <- switch(col_classes[[col]],
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

# Function to read the sampled file
read_sampled_file <- function(file) {
  header <- fread(file, nrows = 1)
  dt <- fread(file,
    na.strings = na_values,
    colClasses = "character", header = FALSE, skip = 1
  )
  setnames(dt, names(header))
  dt <- dt[, (drop_cols) := NULL]
  for (col in names(col_classes)) {
    dt[[col]] <- switch(col_classes[[col]],
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
  sampled_dt <- dt[sample(.N, min(sample_size, .N))]
  return(sampled_dt)
}


# Main function to read and process chunks
main_read_function <- function(file = NA) {
  if (is.na(file)) {
    if (to_read) {
      file <- full_claims_file(part)
      dt <- read_entire_file(file, initial_read = is_partial_file(part))

      if (to_sample) {
        dt <- handle_sampling(dt, part)
      }
    } else if (to_sample) {
      dt <- handle_sampling()
    } else {
      stop("Cannot proceed: to_read is FALSE and to_sample is FALSE.
      At least one must be TRUE.")
    }
  } else {
    dt <- read_entire_file(file, initial_read = is_partial_file(part))
  }

  return(dt)
}

# Function to read and save partial files
read_and_save_partial <- function(start_row, end_row, part) {
  partial_file_path <- full_claims_file(part, fileext = TRUE)
  header <- fread(full_claims_file(), nrows = 1, header = TRUE)

  print(paste("Reading header from:", full_claims_file()))
  print(paste("Partial file path:", partial_file_path))
  print(paste("Start row:", start_row, "End row:", end_row))

  dt <- NULL
  if (!file.exists(partial_file_path)) {
    print("Partial file does not exist. Reading partial data...")
    dt <- fread(full_claims_file(part),
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = start_row,
      header = FALSE
    )
    setnames(dt, colnames(header))
    print(paste("Number of rows read:", nrow(dt)))
    if (nrow(dt) > 0) {
      print(paste("Writing partial file to:", partial_file_path))
      fwrite(dt, partial_file_path, quote = TRUE)
    } else {
      print("No rows to save")
    }
  } else {
    print(paste(
      "Partial file already exists. Skipping creation:",
      partial_file_path
    ))
    dt <- fread(partial_file_path)
  }

  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part)
    print(paste("Sampled file path:", sampled_file_path))
    if (!file.exists(sampled_file_path)) {
      print("Sampled file does not exist. Creating new sample...")
      if (!is.null(dt) && nrow(dt) > 0) {
        sampled_dt <- sample_data(dt)
        setnames(sampled_dt, colnames(header))
        print(paste("Writing sampled file to:", sampled_file_path))
        fwrite(sampled_dt, sampled_file_path, quote = TRUE)
      } else {
        stop("Failed to read partial file or no rows available for sampling")
      }
    } else {
      print(paste(
        "Sampled file already exists. Skipping creation:",
        sampled_file_path
      ))
    }
  }
}
