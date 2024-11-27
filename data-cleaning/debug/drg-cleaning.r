# thread_offset <- 0

# sample_size_divisor <- 125
# # Whether to sample each split_part by sample_size_divisor
# # (useful when iterating through code runs in quick succession)
# to_sample <- TRUE
# # TODO: Add description here
# to_write <- TRUE
# # TODO: Add description here
# to_flush <- FALSE
# # TODO: Add description here
# to_parallel <- TRUE
# # TODO: Add description here
# to_debug <- FALSE
# verbose_output <- if (to_debug) TRUE else FALSE

# to_bq <- FALSE

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
  "rmarkdown", "digest", "base64enc", "arrow"
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


# Source each file sequentially
for (file in list.files(here::here("data-cleaning/r_scripts_v2"), pattern = "\\.R$", full.names = TRUE)) invisible(source(file))

message(year_to_load)


# Enable caching and printing options for data mapping
to_use_cache <- FALSE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping data tables

# Helper function to load data from cache or query from BigQuery if not cached
load_or_query <- function(query, var_name) {
  rds_path <- here(cache_path, "mapping", paste0(var_name, ".rds"))
  if (to_use_cache && file.exists(rds_path)) {
    if (verbose_output) message("Loading ", var_name, " from cache...")
    # Load data from .rds file if cache exists
    return(readRDS(rds_path))
  } else {
    if (verbose_output) message("Querying ", var_name, " from BigQuery...")
    # Query data from BigQuery if not cached
    # Query execution function (BigQuery to data.table)
    dt <- query_bq_to_dt(query)
    saveRDS(dt, rds_path) # Save queried data to .rds cache file
    return(dt)
  }
}

# Helper function to print all rows of a data.table if
# to_print_mapping_data is enabled
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}

# 1. Query and load the `grouper_v5.proc` table
# This table contains procedure codes and attributes
# like description, classification, and site
proc_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.proc`")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type

# 2. Query and load the `phic.acr_rvs_map` table
# This table maps RVS codes to ICD-9-CM codes,
# used for healthcare billing purposes
rvs_icd9_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_rvs_map`")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")

# Convert RVS and ICD9CM columns to character type
# and adjust ICD9CM for multiplication
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]

# Merge the RVS-ICD9 mapping with the proc table for DRG classification
rvs_icd9 <- merge(
  rvs_icd9,
  proc[, .(CODE, DRGUSE)], # Select CODE and DRGUSE columns for merging
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
  # Merge on icd9cm and CODE columns
)

# Filter and annotate DRG-related codes, removing unnecessary DRGUSE column
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][
  !is.na(rvs) & !is.na(icd9cm), -"DRGUSE"
]

# 3. Query and load `phic.acr_procedure` table
# This table contains RVS codes, relative value units (RVUs),
# and descriptions for procedures
acr_rvs_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_procedure`")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")

# 4. Query and load `grouper_v5.i10` table
# This table contains ICD-10 codes with DRG grouping data,
# including codes marked as "accepted" (ACCPDX = "Y")
i10_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10`")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups

# Extract unique accepted ICD-10 codes for diagnosis
# (ACCPDX == "Y") and store in acc_pdx
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])

# Create an environment for quick lookup of accepted diagnosis codes
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  # Assign each accepted code to the environment
  assign(code, TRUE, envir = acc_pdx_env)
}

# 5. Query and load `icd.phl_icd10` table
# This table lists diseases and their corresponding
# ICD-10 codes specific to the Philippines
phl_icd10_query <- paste0("SELECT * FROM `", gcp_proj, ".icd.phl_icd10`")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")

# Filter and process neoplasm codes by extracting
# specific codes from complex ICD-10 notations
neoplasms_dt_actual <- as.data.table(phl_icd10[
  # Select rows with '/' in icd10, indicating neoplasm codes
  grepl("/", icd10), .(icd10)
  # Extract relevant part
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])


# 6. Query and load `grouper_v5.i10vx` table
# This table contains an expanded version of ICD-10 codes with validation flags
i10vx_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10vx`")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)

# 7. Query and load `hci.temp_hci` table
# This table lists healthcare institutions with details
# like ownership, category, and location
hci_query <- paste0("SELECT * FROM `", gcp_proj, ".hci.temp_hci`")
hci <- load_or_query(hci_query, "hci")

# 8. Define global variables for use later in the script:
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes

neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")

phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)

# Function to create an environment from a vector of unique values
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}

# 1. Create environment for `proc` table data if specific values are needed
# Here we assume `proc$CODE` is the field of interest
proc_env <- create_env_from_vector(proc$CODE)

# 2. Create environment for `rvs_icd9` table data based on `rvs` and `icd9cm`
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)

# 3. Environment for `acr_rvs` table (assuming `rvs` is the field of interest)
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)

# 4. Environment for accepted ICD-10 codes (from `tdrg_icd10`)
acc_pdx_env <- create_env_from_vector(acc_pdx)

# 5. Environment for `phl_icd10` ICD-10 codes (e.g., neoplasm codes)
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)

# 6. Environment for expanded ICD-10 codes (`i10vx`)
acc_icd_env <- create_env_from_vector(i10vx$code)

# 7. Environment for `hci` table data if needed for specific fields (e.g., `id_hci`)
# Assuming `hci$id_hci` is the identifier of interest
hci_env <- create_env_from_vector(hci$id_hci)

# 8. Other specific environments for global variables
neoplasm_env <- create_env_from_vector(neoplasm_codes)
covid_env <- create_env_from_vector(covid_codes)
rvs_codes_env <- create_env_from_vector(rvs_codes)
phil_icds_env <- create_env_from_vector(phil_icds)
icd_codes_env <- create_env_from_vector(icd_codes)

# Combine COVID and neoplasm codes into a single environment
covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)

# Combine COVID, RVS, and neoplasm codes into a single environment for efficient lookup
covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)


# Create a combined regular expression pattern to match COVID,
# RVS, and neoplasm codes in data processing
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)
# if (to_print_mapping_data) print(covid_rvs_neoplasm_pattern)
# # Print regex pattern if enabled

# Helper function to save all specified data tables into a
# single text file for debugging
save_all_data_to_file <- function(file_path, ...) {
  args <- list(...)
  sink(file_path) # Redirect output to the specified file
  cat("\n--- All Data Tables in One View ---\n") # Header for the file
  for (name in names(args)) {
    cat("\n---", name, "---\n") # Print table name as a header within the file
    # Print all rows of each data.table
    print(args[[name]], nrow = Inf, max.print = Inf)
  }
  sink() # Stop redirecting output to the file
  if (verbose_output) message("All data tables saved to ", file_path) # Confirmation message
}

# Set the file path for the output text file, where all
# data tables will be saved
output_file <- here(debug_path, "mapping_data.txt")

# If enabled, save all processed data tables to a single specified
# file for debugging and verification.
# Each table represents a different aspect of the medical coding,
# classification, and healthcare provider data.
if (to_print_mapping_data) {
  options(max.print = 999999)
  save_all_data_to_file(
    output_file,

    # Data table containing procedure codes and attributes
    # for each procedure code.
    # Columns include CODE (unique procedure identifier),
    # DRGUSE (flag indicating if the procedure is used for DRG grouping),
    # and several other attributes related to procedure
    # classification, gender applicability, site, and level of care.
    grouper_v5_proc = proc,

    # Mapping between ICD-9-CM codes and RVS (Relative Value Scale)
    # codes used in medical billing.
    # Includes `is_drg` column to mark codes used in DRG grouping
    # after merging with the `grouper_v5.proc` table.
    # This table links standard ICD-9 procedure codes to specific
    # RVS codes for billing purposes.
    phic_acr_rvs_map = rvs_icd9,

    # Table of RVS codes and their associated RVU (Relative Value Units)
    # which represent the value of a procedure.
    # Also includes a detailed description of each procedure, such as type,
    #  category, or specific details about the procedure.
    # This table is essential for understanding the cost/value of each
    # RVS-coded procedure in medical billing.
    phic_acr_procedure = acr_rvs,

    # ICD-10 table with additional classification details relevant for
    #  DRG (Diagnosis Related Group) mapping.
    # Columns include CODE (ICD-10 diagnosis code), ACCPDX
    # (accepted primary diagnosis flag), and other grouping
    # variables like MDC (Major Diagnostic Category) and CC
    # (Complication/Comorbidity), which help classify the severity or
    # complexity of cases for healthcare reimbursement.
    grouper_v5_i10 = tdrg_icd10,

    # A unique list of ICD-10 codes flagged as ACCPDX (accepted
    # primary diagnosis codes) for use in DRG classification.
    # This list is derived from `grouper_v5.i10` and is used as
    # a quick reference to check if a diagnosis is eligible as a primary code.
    acc_pdx = acc_pdx,

    # Data specific to the Philippines for ICD-10 codes, containing
    # disease names and the corresponding ICD-10 codes.
    # This table includes the field `remarks`, which provides
    # special notes or guidance for each code, such as diagnostic
    # instructions or clarifications. This dataset is used to
    # manage and classify diseases in line with local health regulations.
    icd_phl_icd10 = phl_icd10,

    # Processed subset of neoplasm codes extracted from
    # `icd.phl_icd10`.
    # Contains ICD-10 codes specifically formatted to represent
    # malignant, benign, and other tumor types.
    # Useful for oncology-specific mappings in DRG processing
    # or cancer-related case management.
    neoplasms_dt_actual = neoplasms_dt_actual,

    # Expanded ICD-10 dataset with validation flags indicating
    # whether each code is valid.
    # Includes columns such as `validcode` (flag for validation status)
    # and `todel` (marker for codes that may need removal).
    # This dataset helps verify the validity of ICD-10 codes and
    #  manage code deprecation or updates.
    grouper_v5_i10vx = i10vx,

    # List of unique ICD-10 codes extracted from `grouper_v5.i10vx`
    # for quick access.
    # Acts as a condensed reference of all validated ICD-10 codes
    # available in the `grouper_v5.i10vx` dataset.
    # Useful for ensuring consistency and accuracy in ICD-10 code
    # usage across processes.
    acc_icd = acc_icd,

    # A directory of healthcare institutions (HCI), containing
    # detailed information about each provider,
    # including their institution name, ownership type (e.g.,
    # government, private), category, geographical details,
    # and provider classification. This table enables linkage
    # between clinical data and provider-specific data,
    # allowing for enhanced reporting and analytics on healthcare
    # service providers.
    hci_temp_hci = hci
  )
  # print(rvs_pattern)
  options(max.print = 1000)
}


# Step 5: Loop through each part and process the partial files
# saveWidget(profvis({
for (loop_part in 1:split_parts) {
  # loop_part <- 1
  start_time <- Sys.time() # Record start time for processing
  # Step 7: Read the appropriate file (sample or full)
  message(paste0("Start reading part ", loop_part, " of ", split_parts))
  read_result <- read_appropriate_file(loop_part)
  # The data to process
  read_in_dt <- read_result$read_result_dt
  # Any replacements summary
  read_in_replacement_summary <- read_result$read_result_replacement_summary

  message(paste0("Finished reading part ", loop_part, " of ", split_parts))

  message(paste0("Start chunking part ", loop_part, " of ", split_parts))
  # Step 8: Split the data into chunks for parallel processing
  chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
  chunks <- split(
    read_in_dt,
    rep(
      1:nthreads,
      each = chunk_size,
      length.out = nrow(read_in_dt)
    )
  )
  message(paste0("Finished chunking part ", loop_part, " of ", split_parts))

  message(paste0("Start processing part ", loop_part, " of ", split_parts))
  # Step 9: Apply parallel processing
  # See function(s) before the loop
  if (to_parallel) {
    parallel_results <- mclapply(
      chunks, process_chunk,
      mc.cores = nthreads
    )
  } else {
    if (!to_debug) parallel_results <- lapply(chunks, process_chunk) else parallel_results <- list(process_chunk(chunks[[1]]))
  }

  rbound_dt <- rbindlist(lapply(
    parallel_results,
    function(res) {
      res$return_chunk
    }
  ))

  # Step 11: Check for invalid primary diagnoses (PDx) and update the summary
  invalid_pdx_indices <- which(
    !is.na(rbound_dt$pdx) & rbound_dt$pdx != "" &
      !sapply(rbound_dt$pdx, function(x) exists(x, acc_pdx_env))
  )
  if (length(invalid_pdx_indices) > 0) {
    message(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
    pdx_success_list[[loop_part]] <- FALSE
  } else {
    pdx_success_list[[loop_part]] <- TRUE
  }

  for (i in seq_along(parallel_results)) {
    parallel_results[[i]]$return_summary$pdx_success <- pdx_success_list[[loop_part]]
    parallel_results[[i]]$return_summary$replacement_summary <- read_in_replacement_summary
  }

  # Step 10: Combine results from all parallel chunks
  parallel_summaries <- lapply(
    parallel_results,
    function(res) {
      res$return_summary
    }
  )

  summarized_dt <- rbound_dt # Store the summarized data
  combined_chunk_summary[[loop_part]] <- parallel_summaries

  # Step 12: Write processed data to checkpoint file if required
  if (to_write) {
    saveRDS(
      summarized_dt, here(checkpoint_1_path, paste0(
        checkpoint_1_prefix, year_to_load, suffix,
        "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      )),
      compress = TRUE
    )
  }

  # Step 13: Collect summaries for each part
  all_parts_summaries[[loop_part]] <- combined_chunk_summary[[loop_part]]
  processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(),
    start_time,
    units = "secs"
  ))

  # Step 14: Update status and ETA
  print_status_update(loop_part, split_parts, processing_times, "clean")

  if (loop_part == 1) dim_dt <- dim(summarized_dt)

  nrow_end[[loop_part]] <- nrow(summarized_dt)
  # Step 15: Clean up memory after processing each part
  rm(read_in_dt, rbound_dt, summarized_dt)
  invisible(gc())
}
# }), profvis_fpath)

print_summary_tables(aggregate_all_summaries(all_parts_summaries))


# Step 2: Combine all parts into a master data table
master_dt_list <- lapply(1:split_parts, function(read_part) {
  cat(paste("\rStarted reading part", read_part))
  flush.console()
  return_dt <- readRDS(here(checkpoint_1_path, paste0(
    checkpoint_1_prefix, year_to_load, suffix,
    "part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
  )))
  # message(colnames(return_dt))
  cat(paste("\rFinished reading prt", read_part))
  flush.console()
  return(return_dt)
})
message("Commencing rbindlist")
master_dt <- rbindlist(master_dt_list, fill = TRUE)
rm(master_dt_list)
invisible(gc())
message("Finished rbindlist")
# Step 1: Validate row counts across parts
# Initialize variable to track total row counts across parts
total_start_rows <- 0
total_end_rows <- 0

for (nrow_part in 1:split_parts) {
  # Sum up row counts for each part
  total_start_rows <- total_start_rows + nrow_start[[nrow_part]]
  total_end_rows <- total_end_rows + nrow_end[[nrow_part]]

  # Check if rows match for each part
  if (nrow_start[[nrow_part]] != nrow_end[[nrow_part]]) {
    warning(
      "WARNING: Row Count Mismatch! Part ", nrow_part,
      " has ", nrow_start[[nrow_part]], " starting rows and ",
      nrow_end[[nrow_part]], " ending rows\n"
    )
    stop("ERROR: Row Count Mismatch")
  }
}

# Check if the total rows match
if (if (to_sample) total_rows / sample_size_divisor else total_rows == nrow(master_dt)) {
  message("\nRow Counts Match for All Parts and Sum to Total Rows\n")
} else {
  stop("ERROR: Total Row Count Mismatch")
}

# Step 3: Save the combined master data table
if (to_write) {
  message("Commencing saveRDS")
  saveRDS(master_dt, here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ), compress = TRUE)
  message("Finished saveRDS")
}


if (exists("master_dt")) {
  message("master_dt exists, making a copy and deleting it")
  result <- data.table::copy(master_dt)
  rm(master_dt)
  invisible(gc())
  message("copied master_dt to result, deleted master_dt")
} else {
  message(paste0("master_dt doesn't exist, reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
  result <- readRDS(here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ))
  invisible(gc())
  message(paste0("finished reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
}

message(paste0("Saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
), compress = TRUE)
message(paste0("Finished saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))


if (to_post_cleaning_checks) {
  # Ensure acc_pdx is a set (i.e., unique values for faster lookup)
  acc_pdx_set <- unique(acc_pdx)

  dt <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))

  str(dt)

  # Find entries in dt$clin_pdx that are not in acc_pdx_set
  not_in_acc_pdx <- dt$clin_pdx[!dt$clin_pdx %in% acc_pdx_set & !is.na(dt$clin_pdx)]

  # Check if all clin_pdx entries are in acc_pdx_set
  all_in_acc_pdx <- length(not_in_acc_pdx) == 0

  # Output the result
  if (all_in_acc_pdx) {
    message("All entries in dt$clin_pdx are in acc_pdx.")
  } else {
    message("Not all entries in dt$clin_pdx are in acc_pdx. Entries not in acc_pdx are:")
    print(unique(not_in_acc_pdx)) # Print unique entries not in acc_pdx
  }
  rm(all_in_acc_pdx, not_in_acc_pdx)
  invisible(gc())
}


if (to_post_cleaning_checks) {
  # Assuming dt is your data.table
  # Calculate counts per category and total count
  count_data <- dt[, .N, by = clin_pdx_source]
  setorder(count_data, clin_pdx_source)
  count_data[, clin_pdx_source := factor(clin_pdx_source, levels = c(1, 2, 3, 6, 99))]

  # Calculate total count
  total_count <- sum(count_data$N)

  # Plot histogram with counts on top of each bar and total count below
  ggplot(count_data, aes(x = clin_pdx_source, y = N)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Histogram of clin_pdx_source", x = "clin_pdx_source", y = "Count") +
    theme_minimal() +
    scale_x_discrete(drop = FALSE) + # Ensures all categories are shown
    geom_text(aes(label = N), vjust = -0.5) + # Display count above each bar
    annotate("text", x = Inf, y = -Inf, label = paste("Total N =", total_count), hjust = 1.1, vjust = -1.5) # Display total count below
  rm(count_data, total_count)
  invisible(gc())
}


if (to_post_cleaning_checks) {
  # Capture the combined structure output for each column into a single text variable
  output <- capture.output({
    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("dt\n")
    str(dt)

    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("c1:\n")
    str(dt[!is.na(c1) & sapply(c1, function(x) length(x) > 0 && any(nzchar(x)))]$c1)

    cat("\nc2:\n")
    str(dt[!is.na(c2) & sapply(c2, function(x) length(x) > 0 && any(nzchar(x)))]$c2)

    cat("\nclin_sdx:\n")
    str(dt[!is.na(clin_sdx) & sapply(clin_sdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_sdx)

    cat("\nclin_pdx:\n")
    str(dt[!is.na(clin_pdx) & sapply(clin_pdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_pdx)

    cat("\nclin_proc:\n")
    str(dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]$clin_proc)
  })

  # Print the output as a single message or save it to a file
  cat(paste(output, collapse = "\n"))
  rm(output)
  invisible(gc())
}


if (to_post_cleaning_checks) {
  # Flatten the list, get unique values, and omit NA
  unique_values <- unique(unlist(dt$clin_rvs))
  unique_values <- unique_values[!is.na(unique_values) & unique_values != "NA"]

  # Filter values that are also in rvs_icd9$rvs
  matched_values <- unique_values[unique_values %in% rvs_icd9$rvs]

  # Print the matched values, separated by line breaks
  cat(paste(matched_values, collapse = "\n"))
  rm(unique_values, matched_values)
  invisible(gc())
}


if (to_post_cleaning_checks) {
  # Assuming `dt` is your data.table
  # Filter rows where clin_proc is not NA and does not contain character(0) or empty strings
  non_empty_clin_proc_rows <- dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]

  # Print the result
  print(non_empty_clin_proc_rows)
  rm(non_empty_clin_proc_rows)
  invisible(gc())
}


# result <- readRDS(here(
#   checkpoint_2_path,
#   paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
# ))

# # Convert both id_hci and PMCC_NO to characters
# # to ensure consistency for joining
# result[, id_hci := as.character(id_hci)]
# hci[, PMCC_NO := as.character(PMCC_NO)]

# # Remove leading zeros from numeric PMCC_NO values,
# # while preserving non-numeric values
# hci[, PMCC_NO_stripped := fifelse(
#   grepl("^[0-9]+$", PMCC_NO),
#   sub("^0+", "", PMCC_NO), # Remove leading zeros from numeric strings
#   PMCC_NO # Keep non-numeric values unchanged
# )]

# # Keep only necessary columns from hci for the join
# hci_subset <- hci[, .(
#   PMCC_NO_stripped,
#   SOC_SECTOR, INST_NAME, CAT_24,
#   REGION_NAME, PROVINCE_NAME
# )]

# # Perform a left join, keeping all rows in
# # result and only matching rows from hci
# result <- merge(
#   result,
#   hci_subset,
#   by.x = "id_hci",
#   by.y = "PMCC_NO_stripped",
#   all.x = TRUE, # Keep all rows from result
#   all.y = FALSE # Only include matching rows from hci
# )

# # ----- Filter rows by allowed CAT_24 categories -----
# allowed_categories <- c(
#   "INFIRMARY/DISPENSARY",
#   "LEVEL 1 HOSPITAL", "LEVEL 2 HOSPITAL", "LEVEL 3 HOSPITAL"
# )

# # Count rows where CAT_24 is not in allowed categories
# count_excluded <- result[
#   !(CAT_24 %in% allowed_categories), .N
# ]
# cat(
#   "Number of rows where CAT_24 is not in the allowed categories:",
#   count_excluded, "\n"
# )


# # Drop rows where CAT_24 is not in the allowed categories
# result <- result[CAT_24 %in% allowed_categories]
# # ----- Filter rows where clin_outpatient is TRUE -----
# # Count rows where clin_outpatient is TRUE
# count_clin_outpatient_true <- result[
#   clin_outpatient == TRUE, .N
# ]
# cat(
#   "Number of rows where clin_outpatient is TRUE:",
#   count_clin_outpatient_true, "\n"
# )

# # Drop rows where clin_outpatient is TRUE
# result <- result[clin_outpatient != TRUE]
# # ----- Filter rows where claim_status is not "G" -----
# # Count rows where claim_status is not "G"
# count_claim_status_not_g <- result[
#   claim_status != "G", .N
# ]
# cat(
#   "Number of rows where claim_status is not 'G':",
#   count_claim_status_not_g, "\n"
# )

# # Drop rows where claim_status is not "G"
# result <- result[claim_status == "G"]
# # Convert 'c1' from a list of character vectors
# # into a single concatenated string
# result[, c1_spc := sapply(c1, function(x) {
#   if (is.null(x) || all(is.na(x))) {
#     return(NA_character_) # Return NA if the list is empty or all values are NA
#   } else {
#     # Sort, remove duplicates, and concatenate
#     return(paste(sort(unique(x)), collapse = ","))
#   }
# })]

# # ----- Handle duplicate rows based on specific columns -----
# duplicate_columns <- c(
#   "id_pin", "pat_type", "pat_age",
#   "pat_sex", "date_adm", "date_dis", "c1_spc", "claim_payout"
# )

# # Count duplicate rows based on the specified columns
# count_duplicates <- result[
#   duplicated(result[, ..duplicate_columns]), .N
# ]
# cat(
#   "Number of duplicated rows based on specified columns:",
#   count_duplicates, "\n"
# )

# # Drop duplicate rows, keeping only the first occurrence
# result <- result[!duplicated(result[, ..duplicate_columns])]
# # ----- Remove unnecessary columns and reorder remaining columns -----
# # Remove unnecessary columns
# result[, c("c1", "c1_spc", "c2", "clin_rvs") := NULL]

# # Perform garbage collection to free up memory
# invisible(gc())

# # Set the column order to a specified structure
# setcolorder(result, c(
#   "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
#   "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
#   "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
#   "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent", "pat_memcat_child",
#   "is_covid", "claim_status", "claim_payout", "claim_charge", "clin_discharge",
#   "clin_outpatient", "clin_emergency", "clin_acc", "clin_c1", "clin_c2",
#   "clin_sdx", "clin_proc", "clin_pdx", "clin_pdx_source", "SOC_SECTOR",
#   "CAT_24", "INST_NAME", "REGION_NAME", "PROVINCE_NAME"
# ))

# saveRDS(
#   result,
#   here(
#     checkpoint_2_path,
#     paste0(checkpoint_2_prefix, year_to_load, suffix, "stata", ".rds")
#   ),
#   compress = TRUE
# )


# message("You may now run drg-grouping for spc analysis (use to_spc <- TRUE)")


if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))

  setkey(result, NULL) # Removes any existing key
  print(result[id_series %like% "e"])
  print(result[id_pin %like% "e"])
  print(result[id_hci %like% "e"])

  # Flatten clin_sdx and check if "A" is present in any element
  flattened_clin_sdx <- unlist(result$clin_sdx, use.names = FALSE, recursive = TRUE)

  # Check if "A" is in any element using %chin% (fast for exact matches)
  if ("A" %chin% flattened_clin_sdx) {
    cat("Found 'A' in clin_sdx\n")
    # Optionally, filter rows that contain "A" in clin_sdx and print progress every 100,000 rows
    rows_with_A <- result[sapply(result$clin_sdx, function(x) any("A" %chin% x))]

    for (i in seq_len(nrow(rows_with_A))) {
      print(rows_with_A[i])
    }
  } else {
    cat("No 'A' found in clin_sdx\n")
  }
}


if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))

  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )

  # Find rows where any date column has a date before 1900-01-01
  rows_with_old_dates <- result[Reduce(`|`, lapply(
    .SD,
    function(x) x < as.Date("1900-01-01")
  )), .SDcols = date_cols]

  # Print the resulting rows
  print(rows_with_old_dates)
}


result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
))
print(nrow(result))
# result <- result[clin_outpatient == FALSE, ] # DONT SUBSET OUTPATIENT CLAIMS
print(nrow(result))
result[, is_covid := {
  # Start with FALSE
  covid_found <- rep(FALSE, .N)

  # Check each condition sequentially, updating only rows not yet marked TRUE
  not_found <- !covid_found
  covid_found[not_found] <- clin_c1[not_found] %chin% covid_rvs

  not_found <- !covid_found
  covid_found[not_found] <- clin_c2[not_found] %chin% covid_rvs

  not_found <- !covid_found
  covid_found[not_found] <- c2[not_found] %chin% covid_rvs

  not_found <- !covid_found
  covid_found[not_found] <- c1[not_found] %chin% covid_rvs

  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_rvs[not_found], function(row) any(row %chin% covid_rvs))

  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_sdx[not_found], function(row) any(row %chin% covid_rvs))

  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_proc[not_found], function(row) any(row %chin% covid_rvs))

  # Return the result
  covid_found
}]

# result <- result[is_covid == FALSE, ] # DONT SUBSET COVID CLAIMS
print(nrow(result))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))


if (to_post_cleaning_checks) {
  before <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, ".rds")
  ))
  after <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
  ))

  # Ensure both data.tables have the same key columns for comparison
  setkey(before, id_series)
  setkey(after, id_series)

  # Identify rows where pat_age is different
  # between the two tables, handling NA values
  pat_age_diff_na <- before[after,
    on = .(id_series), nomatch = 0,
    # Explicitly name pat_age_before as coming from "before"
    .(id_series, pat_bdate,
      pat_age_before = x.pat_age,
      pat_age_after = i.pat_age
    ),
    by = .EACHI
  ]

  # Filter to show rows where one value is NA and
  # the other is not or the values are simply different
  pat_age_diff_na <- pat_age_diff_na[
    (is.na(pat_age_before) & !is.na(pat_age_after)) |
      (!is.na(pat_age_before) & is.na(pat_age_after)) |
      (pat_age_before != pat_age_after)
  ]

  # Print the differences
  cat("Rows where pat_age is NA in one table but
not in the other, or where the values differ:\n")
  print(pat_age_diff_na)

  rm(before, after)
  invisible(gc())
}


result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm, time_adm,
  date_dis, time_dis, date_rec, date_ref, date_check, pat_type, pat_rel, pat_bdate,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")
))
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm,
  date_dis, date_rec, date_ref, date_check, pat_type, pat_rel,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))


result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))


if (to_bq) {
  if (!to_sample) bq_table <- paste0("claims_", year_to_load)

  # Check if the table should be dropped and replaced
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      # If the table does not exist, just continue
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        # If it's a different error, re-throw the error
        stop(e)
      }
    }
  )

  # Attempt to create the table
  tryCatch(
    {
      bq_table_create(
        bq_table(gcp_proj, bq_dataset, bq_table),
        fields = fromJSON(here(
          "data-cleaning/r_scripts_v2",
          "bq_schema_cleaning.json"
        ), simplifyDataFrame = FALSE)
      )
      message("Table created successfully.\n")
    },
    error = function(e) {
      # Check if the error message indicates that the table already exists
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        # If it's a different error, re-throw the error
        stop(e)
      }
    }
  )

  # Upload to BQ only if table is empty
  if (to_write) {
    chunk_size <- 250000 # Adjust the chunk size based on memory availability
    num_chunks <- ceiling(nrow(result) / chunk_size)

    for (i in seq_len(num_chunks)) {
      cat(paste("\rUploading chunk no.:", i))
      flush.console()
      chunk <- result[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result)),
      ]

      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = chunk,
        write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
      )
      cat(paste("\rUploaded chunk no.:", i))
      flush.console()
    }
  }
}


# concatenate_r_files <- function(input_path, output_file) {
#   # List all .R files in the directory
#   r_files <- list.files(
#     path = input_path,
#     pattern = "\\.R$", full.names = TRUE
#   )

#   # Delete the existing output file if it exists
#   if (file.exists(output_file)) {
#     file.remove(output_file)
#   }

#   # Read and concatenate contents
#   file_contents <- lapply(r_files, readLines)
#   concatenated_content <- unlist(file_contents)

#   # Write concatenated content to the output file
#   cat(concatenated_content, file = output_file, sep = "\n")
# }

# # Consolidate all r_scripts scripts into debug.R; useful for debugging
# concatenate_r_files(
#   here::here("data-cleaning/r_scripts_v2"),
#   here::here("data-cleaning/debug/r_scripts_v2.R")
# )

# system(
#   paste(
#     "cd ~/drg-pipeline &&",
#     "jupyter nbconvert",
#     "--no-prompt",
#     "--to script data-cleaning/01-drg-cleaning-v2.ipynb",
#     "--output debug/drg-cleaning-v2"
#   )
# )


# # Assign values using regular assignment (no need for <<- if declared globally)
# if (TRUE) {
#   to_debug <- FALSE
#   to_flush_master <- FALSE
#   to_flush_partial <- FALSE
# }

# # Define the paths and their corresponding conditions
# paths <- list(
#   to_flush_master = c(
#     "data-cleaning/cache",
#     "data-cleaning/data/profvis",
#     "data-cleaning/data/aux-files",
#     "data-cleaning/data/checkpoints",
#     "data-cleaning/debug"
#   ),
#   to_flush_partial = c(
#     "data-cleaning/data/claims/raw/parts",
#     "data-cleaning/data/claims/raw/samples"
#   )
# )

# # Iterate over the paths and delete directories if the corresponding condition is true
# for (condition in names(paths)) {
#   if (get(condition, envir = .GlobalEnv)) { # Ensure the variables are accessed in the global environment
#     system(paste(
#       "rm -r",
#       paste(here::here(unlist(paths[[condition]])), collapse = " ")
#     ))
#   }
# }

# # Clean up the environment and run garbage collection if debugging is enabled
# if (to_debug) {
#   rm(list = ls(), envir = .GlobalEnv) # Ensure global environment is cleared
#   invisible(gc())
# }


# paths <- list(
#   input_notebook = here::here("data-cleaning", "01-drg-cleaning-v2.ipynb"),
#   output_rscript = here::here("data-cleaning", "debug", "drg-cleaning")
# )

# system(paste(
#   "jupyter nbconvert --no-prompt --to script",
#   paths$input_notebook, "--output", paths$output_rscript
# ))

