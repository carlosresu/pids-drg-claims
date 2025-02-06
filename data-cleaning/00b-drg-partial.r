# thread_offset <- 0

# sample_size_divisor <- 625
# # Whether to sample each split_part by sample_size_divisor
# # (useful when iterating through code runs in quick succession)
# to_sample <- FALSE
# # TODO: Add description here
# to_write <- TRUE
# # TODO: Add description here
# to_flush <- FALSE
# # TODO: Add description here
# to_parallel <- TRUE
# # TODO: Add description here
# to_debug <- FALSE
# verbose_output <- if (to_debug) TRUE else FALSE

source("~/drg-pipeline/data-cleaning/00a-parameters.r")


# Update the grouper
system("git submodule update --init --recursive")

# List, install (if applicable), and load packages
## Required packages
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
  # , "docstring", "progress" # Comma is here so if I uncomment this line it
  # automatically works without having to type or delete a comma after haven
)

# Additional packages to install via remotes (GitHub), if not available
github_packages <- c("r-lib/styler")

# Number of CPU cores for parallel compilation
n_cores <- parallel::detectCores()

# Function to install and load packages quietly
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}

# Function to install packages from GitHub via remotes
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes", Ncpus = n_cores)
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}

# Apply the function to each required package
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)

# Install and load GitHub packages if not installed
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)


for (file in c(
  "0.1.0.params_fpaths.R",
  "2.0.split_and_save_part.R",
  "0.3.0.summary_helper_functions.R"
)) {
  source(here::here("data-cleaning/r_scripts_v2/", file))
}

# Define paths and constants
gcs_base <- "gs://phic-claims-raw/"
to_hash <- FALSE # Flag to enable or disable hashing

# Function to escape spaces for system commands
escape_spaces <- function(path) {
  gsub(" ", "\\\\ ", path)
}

# Process a single file
process_file <- function(year_to_load) {
  # Set file extension based on year_to_load
  file_type <- if (year_to_load %in% c(2022, 2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year_to_load, file_type)
  gcs_path <- paste0(gcs_base, file_name)

  # Local file paths with escaped spaces
  file_path <- here::here(raw_claims_path, file_name)
  file_path_escaped <- escape_spaces(file_path)
  md5_rds_path <- here::here(raw_claims_md5_path, paste0(year_to_load, "_md5.rds"))

  # Check if the file and its MD5 RDS already exist
  if (file.exists(file_path) && (to_hash == FALSE || file.exists(md5_rds_path))) {
    if (to_debug) message(paste("File", file_name, "and its MD5 already exist. Skipping download and hash generation.\n"))
    return(NULL)
  }

  # Download the file if it does not exist
  if (!file.exists(file_path)) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(paste("gsutil cp", escape_spaces(gcs_path), file_path_escaped), intern = FALSE, ignore.stderr = FALSE)
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  }

  # If hashing is disabled, skip further operations
  if (!to_hash) {
    if (to_debug) message(paste("Hashing disabled. Skipping MD5 operations for", file_name, "\n"))
    return(NULL)
  }

  # Generate MD5 hash for the local file in a memory-efficient manner
  md5_hex <- digest::digest(file(file_path, "rb"), algo = "md5", file = TRUE)
  md5_raw <- as.raw(as.numeric(strtoi(substring(md5_hex, seq(1, nchar(md5_hex), 2), seq(2, nchar(md5_hex), 2)), 16)))
  md5_base64 <- base64enc::base64encode(md5_raw)
  local_md5 <- list(md5_hex = md5_hex, md5_base64 = md5_base64)

  # Save MD5 hash as RDS for future checks in the MD5 directory
  saveRDS(local_md5, file = md5_rds_path)

  # Debug output for MD5
  if (to_debug) {
    cat("Generated MD5 for local file:\n")
    print(local_md5)
  }

  # Retrieve and compare MD5 hash from GCS
  gcs_md5_output <- system(paste("gsutil hash", escape_spaces(gcs_path)), intern = TRUE)
  md5_line <- gcs_md5_output[grepl("Hash \\(md5\\):", gcs_md5_output)]

  # Check for match
  if (length(md5_line) == 0) {
    stop(paste("GCS MD5 hash not found for", file_name))
  } else if (!grepl(md5_base64, md5_line)) {
    stop(paste("Mismatch detected for", file_name))
  } else {
    message(paste("MD5 match confirmed for", file_name))
  }

  # Clean up memory
  rm(md5_hex, md5_raw, local_md5, gcs_md5_output, md5_line)
  gc()
}

# Process files sequentially
years <- 2018:2023
for (year in years) {
  process_file(year)
}

cat("All files are up-to-date and verified.\n")


print(split_parts)


for (year_to_load in c(2018:2023)) {
  year_to_load <<- year_to_load
  invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
  # Step 1: Read the header of the full claims file
  full_header <- data.table::fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE
  )

  partial_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_parts),
    "_of_", split_parts, ".rds"
  ))

  # Step 2: Check if the split part file already exists.
  # If not, read the full claims file.
  if (!file.exists(partial_file)) {
    # Read the full file into memory
    full_file <- data.table::fread(
      file = full_claims_file, colClasses = "character",
      header = TRUE, encoding = "Latin-1", sep = separator
    )
  }

  # Step 3: Split the file into parts and save them
  # start_time <- Sys.time() # Record start time
  # for (split_loop_part in 1:split_parts) split_and_save_part(split_loop_part)
  parallel::mclapply(
    1:split_parts,
    split_and_save_part,
    mc.cores = nthreads / 2
  )

  # Function to print objects with memory usage greater than 0.000 GB
  print_memory_usage_gb <- function(env = .GlobalEnv) {
    obj_names <- ls(envir = env)
    obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)) / (1024^3)) # Convert bytes to GB
    obj_info <- data.frame(
      Object = obj_names,
      Size_GB = round(obj_sizes, 3) # Round to 3 decimal places for readability
    )

    # Filter for objects taking up more than 0.000 GB and sort in descending order
    obj_info <- obj_info[obj_info$Size_GB > 0, ]
    obj_info <- obj_info[order(obj_info$Size_GB, decreasing = TRUE), ]

    print(obj_info, row.names = FALSE)
  }

  # Call the function to display objects taking up more than 0.000 GB
  print_memory_usage_gb()

  if (exists("full_file")) rm(full_file)
  invisible(gc())
}


# Define paths
hash_cache_dir <- here::here("data-cleaning/cache/partial_md5")
dir.create(hash_cache_dir, recursive = TRUE, showWarnings = FALSE)

# Function to calculate and save MD5 hash for a given file
calculate_md5 <- function(file_path) {
  md5sum <- digest::digest(file = file_path, algo = "md5")
  return(md5sum)
}

# Function to check if MD5 hashes have changed, returning TRUE if no change
check_md5_changes <- function(year_to_load) {
  hash_file_path <- here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds"))

  # Generate new MD5 hashes for each part
  current_hashes <- sapply(1:split_parts, function(part) {
    part_file <- here::here(
      raw_claims_parts_path,
      paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
    )
    calculate_md5(part_file)
  })

  # Check if saved hashes exist
  if (file.exists(hash_file_path)) {
    saved_hashes <- readRDS(hash_file_path)
    # Return TRUE if hashes match, indicating no changes
    if (identical(saved_hashes, current_hashes)) {
      message(paste("No changes in partial files for year", year_to_load))
      return(TRUE)
    }
  }
  return(FALSE)
}

# Function to validate and save MD5 if row counts match
check_and_save_md5 <- function(year_to_load) {
  # Skip if MD5 hashes are unchanged
  if (check_md5_changes(year_to_load)) {
    return(TRUE)
  }

  # Perform row count validation
  total_rows_check <- 0
  for (part in 1:split_parts) {
    part_rows <- nrow(
      readRDS(
        here::here(
          raw_claims_parts_path,
          paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
        )
      )
    )
    total_rows_check <- total_rows_check + part_rows
  }

  # Expected row count
  expected_total_rows <- readRDS(here::here("data-cleaning/cache/total_rows", paste0("total_rows_", year_to_load, ".rds")))

  # If row counts match, save new MD5 hashes and return TRUE
  if (total_rows_check == expected_total_rows) {
    message(paste("Row count matches for year", year_to_load, "- saving MD5 hashes."))

    # Generate MD5 hashes and save to cache
    current_hashes <- sapply(1:split_parts, function(part) {
      part_file <- here::here(
        raw_claims_parts_path,
        paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
      )
      calculate_md5(part_file)
    })
    saveRDS(current_hashes, here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds")))
    return(TRUE)
  } else {
    message(paste("Row count mismatch for year", year_to_load, "- skipping MD5 save."))
    return(FALSE)
  }
}

# Run the validation and MD5 save in parallel for each year
results <- parallel::mclapply(2018:2023, check_and_save_md5, mc.cores = nthreads)

# Final check and message output
if (all(unlist(results))) {
  message("All row counts match and MD5 hashes are updated.")
} else {
  message("Discrepancies found in row counts or updates.")
}


invisible(source(here::here("data-cleaning/r_scripts_v2/3.0.create_sample_files.R")))

# Step 6: Handle sampling logic by sample size divisor first
for (sample_size_divisor in c(625, 125, 25, 5)) {
  sample_size_divisor <<- sample_size_divisor
  message(paste0("Starting sampling for size ÷", sample_size_divisor))

  # Loop over each year for the current sample_size_divisor
  for (year_to_load in c(2018:2023)) {
    year_to_load <<- year_to_load
    message(paste0("Generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))

    # Load parameter file for current year and sample size divisor
    invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))

    # Parallel processing for each split part
    parallel::mclapply(
      1:split_parts,
      function(mclapply_part) {
        sampled_claims_file <- here::here(raw_claims_samples_path, paste0(
          "sampled_claims_", year_to_load, "_", sample_size_divisor,
          "_part_", sprintf("%02d", mclapply_part), "_of_", split_parts, ".rds"
        ))

        # Create sample files if they don't already exist
        if (!file.exists(sampled_claims_file)) {
          create_sample_files(mclapply_part, sampled_claims_file)
        }
      },
      mc.cores = nthreads
    )
    message(paste0("Done generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))
  }
  message(paste0("Finished sampling for size ÷", sample_size_divisor, " for all years"))
}

