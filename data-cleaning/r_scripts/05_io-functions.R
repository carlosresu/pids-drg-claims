# Function to handle sampling
handle_sampling <- function(dt = NULL, part) {
  #' @title Handle Sampling
  #'
  #' @description This function handles the sampling of data.
  #' If a sampled file exists, it reads the file and checks if
  #' the number of rows matches the sample size. If not, it resamples the data.
  #'
  #' @param dt data.table. The data table to be sampled.
  #' Default is NULL.
  #' @param part integer. The part number of the file.
  #'
  #' @return data.table. The sampled data table.

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
  #' @title Resample Data
  #'
  #' @description This function resamples the data from the full
  #' claims file and writes the sampled data to a new file.
  #'
  #' @param part integer. The part number of the file.
  #'
  #' @return data.table. The resampled data table.

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
  #' @title Read Entire File
  #'
  #' @description This function reads the entire file with all
  #' columns. It handles the initial read to get the header and
  #' then reads the data based on the header.
  #'
  #' @param file character. The file path to read.
  #' @param initial_read logical. Whether this is the initial
  #' read to get the header. Default is TRUE.
  #'
  #' @return data.table. The data table read from the file.

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
  #' @title Read Sampled File
  #'
  #' @description This function reads the sampled file with the
  #' header and handles the data based on the specified column classes.
  #'
  #' @param file character. The file path to read.
  #'
  #' @return data.table. The data table read from the sampled file.

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
  #' @title Sample Data
  #'
  #' @description This function samples data from the input data table.
  #'
  #' @param dt data.table. The data table to be sampled.
  #'
  #' @return data.table. The sampled data table.

  sampled_dt <- dt[sample(.N, min(sample_size, .N))]
  return(sampled_dt)
}

# Main function to read and process chunks
main_read_function <- function(file = NA) {
  #' @title Main Read Function
  #'
  #' @description This function reads and processes chunks of data from a file.
  #'
  #' @param file character. The file path to read.
  #' Default is NA.
  #'
  #' @return data.table. The processed data table.

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
  #' @title Read and Save Partial Files
  #'
  #' @description This function reads and saves partial files
  #' from the full claims file.
  #'
  #' @param start_row integer. The starting row number.
  #' @param end_row integer. The ending row number.
  #' @param part integer. The part number of the file.
  #'
  #' @return NULL. The function is used for its side effect of
  #' reading and saving partial files.

  partial_file_path <- full_claims_file(part, fileext = TRUE)
  header <- fread(full_claims_file(), nrows = 1, header = TRUE)

  print(paste("Reading header from:", full_claims_file()))
  print(paste("Partial file path:", partial_file_path))
  print(paste("Start row:", start_row, "End row:", end_row))

  dt <- NULL
  if (!file.exists(partial_file_path)) {
    print("Partial file does not exist. Creating partial file...")
    dt <- fread(full_claims_file(),
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

write_intermediate_file <- function(to_write, part, dt) {
  #' @title Write intermediate file
  #'
  #' @description This function writes the intermediate data table to a file.
  #'
  #' @param part integer. The part number of the data being processed.
  #' @param dt data.table. The data table to be written.
  #'
  #' @return NULL. The function is used for its side effect of
  #' writing the data table to a file.

  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE))
  }
}
