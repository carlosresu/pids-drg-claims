source(here::here("data-cleaning", "00a-parameters.r"))


# Update the grouper
system("git submodule update --init --recursive")

# List required packages
required_packages <- c(
  "data.table", # Fast data manipulation
  "here", # Simplifies file path management
  "tictoc", # Timing code execution
  "stringr", # String manipulation
  "stringi", # Unicode string processing
  "lubridate", # Date-time handling
  "profvis", # Profiling R code
  "hash", # Hashing utility
  "future", # Parallel processing
  "future.apply", # Parallelized apply functions
  "knitr", # Dynamic report generation
  "htmlwidgets", # Interactive HTML widgets
  "parallelly", # Advanced parallel computing
  "stringdist", # String distance calculations
  "parallel", # Base parallel computing
  "reticulate", # Interface to Python
  "bigrquery", # BigQuery client
  "jsonlite", # JSON parsing
  "googleCloudStorageR", # Google Cloud Storage access
  "haven", # Read/write Stata, SPSS, SAS files
  "fst", # Fast serialization
  "httr", # HTTP requests
  "ggplot2", # Data visualization
  "rmarkdown", # Dynamic markdown documents
  "digest", # Create cryptographic hashes
  "base64enc", # Base64 encoding/decoding
  "arrow", # Apache Arrow for fast data storage
  "tidyverse", # Collection of data science packages,
  "fasttime", # for fastPOSIXct
  "glue", # for string pasting
  "progressr" # live progress and ETA
)

github_packages <- c(
  "r-lib/styler" # Code formatting
)

# Installation commands (commented out, for reference)
invisible(lapply(
  required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
  }
))
invisible(lapply(
  github_packages, function(repo) {
    if (!require(basename(repo), character.only = TRUE)) {
      remotes::install_github(repo)
    }
  }
))

# Load packages (assumes they are already installed)
invisible(lapply(required_packages, library, character.only = TRUE))
invisible(lapply(basename(github_packages), library, character.only = TRUE))


year <- as.numeric(fread("~/drg-pipeline/data-cleaning/debug/cache/year.txt"))

# Source each file sequentially
for (file in list.files(
  here::here("data-cleaning/r_scripts_v2"),
  pattern = "\\.R$", full.names = TRUE
)) {
  invisible(source(file))
}

message(year)


# Enable caching and printing options for data mapping
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping data tables

# Helper function to load data from cache or query from BigQuery if not cached
load_or_query <- function(
    query, var_name, year = NULL,
    overwrite_cache = FALSE) {
  rds_path <- here(
    cache_path, "mapping",
    paste0(ifelse(var_name == "hci" & !is.null(year),
      paste0("hci_", year),
      ifelse(var_name == "claims" & !is.null(year),
        paste0("claims_", year),
        var_name
      )
    ), ".rds")
  )


  if (!overwrite_cache && to_use_cache && file.exists(rds_path)) {
    return(readRDS(rds_path))
  }

  dt <- query_bq_to_dt(query)

  if (to_use_cache) saveRDS(dt, rds_path)
  return(dt)
}

# Helper function to print all rows of a data.table if
# to_print_mapping_data is enabled
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}

# 1. Query and load the grouper_v5.proc table
# This table contains procedure codes and attributes
# like description, classification, and site
proc_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.proc")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type

# 2. Query and load the phic.acr_rvs_map table
# This table maps RVS codes to ICD-9-CM codes,
# used for healthcare billing purposes
rvs_icd9_query <- paste0(
  "SELECT * FROM ",
  gcp_proj, ".phic_libraries.acr_rvs_map"
)
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

# 3. Query and load phic.acr_procedure table
# This table contains RVS codes, relative value units (RVUs),
# and descriptions for procedures
acr_rvs_query <- paste0(
  "SELECT * FROM ",
  gcp_proj, ".phic_libraries.acr_procedure"
)
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")

# 4. Query and load grouper_v5.i10 table
# This table contains ICD-10 codes with DRG grouping data,
# including codes marked as "accepted" (ACCPDX = "Y")
i10_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10")
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

# 5. Query and load icd.phl_icd10 table
# ICD-10 codes specific to the Philippines
phl_icd10_query <- paste0("SELECT * FROM ", gcp_proj, ".icd.phl_icd10")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")

# Filter and process neoplasm codes by extracting
# specific codes from complex ICD-10 notations
neoplasms_dt_actual <- as.data.table(phl_icd10[
  # Select rows with '/' in icd10, indicating neoplasm codes
  grepl("/", icd10), .(icd10)
  # Extract relevant part
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])


# 6. Query and load grouper_v5.i10vx table
# This table contains an expanded version of ICD-10 codes with validation flags
i10vx_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10vx")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)

# 7. Query and load hci.temp_hci table
# This table lists healthcare institutions with details
# like ownership, category, and location
# Query and cache HCI data per year
# hci_query <- paste0("SELECT * FROM ", gcp_proj, paste0(".phic_hci.hci_", year))
# hci <- load_or_query(hci_query, "hci", year)
hci_query <- paste0("SELECT * FROM ", gcp_proj, paste0(".phic_hci.hci_full"))
hci <- load_or_query(hci_query, "hci_full")

# 8. Define global variables for use later in the script:
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes

neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")

phil_icds <- unique(gsub(
  "[^A-Za-z0-9]", "",
  phl_icd10[!grepl("/", icd10), icd10]
))
icd_codes <- unique(tdrg_icd10$CODE)

# Function to create an environment from a vector of unique values
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}

# 1. Create environment for proc table data if specific values are needed
# Here we assume proc$CODE is the field of interest
proc_env <- create_env_from_vector(proc$CODE)

# 2. Create environment for rvs_icd9 table data based on rvs and icd9cm
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)

# 3. Environment for acr_rvs table (assuming rvs is the field of interest)
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)

# 4. Environment for accepted ICD-10 codes (from tdrg_icd10)
acc_pdx_env <- create_env_from_vector(acc_pdx)

# 5. Environment for phl_icd10 ICD-10 codes (e.g., neoplasm codes)
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)

# 6. Environment for expanded ICD-10 codes (i10vx)
acc_icd_env <- create_env_from_vector(i10vx$code)

# 7. Environment for hci table data if needed for specific fields (e.g., id_hci)
# Assuming hci$id_hci is the identifier of interest
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

# Combine COVID, RVS, and neoplasm codes into a
# single environment for efficient lookup
covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)

# Create a combined regular expression pattern to match COVID,
# RVS, and neoplasm codes in data processing
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)

claims_query <- paste0("
SELECT
    c.id_series
FROM `drg-pipeline.phic_claims.claims_", year, "` c
JOIN `drg-pipeline.phic_hci.hci_full` h
    ON c.id_hci = h.id_hci
WHERE
    c.claim_status = 'G'
    AND c.is_covid = FALSE
    AND c.clin_outpatient = FALSE
    AND h.inst_level IN ('INF', 'L1', 'L2', 'L3');")

claims <- load_or_query(claims_query, "claims", year)

hci_filter <- hci[inst_level %chin% c("INF", "L1", "L2", "L3"), id_hci]


# Define process_chunk
process_chunk <- function(
    chunk, yr_to_load = year, col_maps = column_mappings,
    known_vals = known_values, remap_master = col_remap_master,
    avail_cols = available_columns,
    looppart = loop_part) {
  ##############################################################################
  # 1. Input/Year Standardization
  ##############################################################################

  # Rename Columns
  setnames(chunk,
    old = avail_cols[avail_cols %in% names(col_maps)],
    new = unlist(col_maps[avail_cols[avail_cols %in% names(col_maps)]])
  )

  # Merge na_values and na_like_strings into a single set for efficiency
  na_values_combined <- unique(c(na_values, na_like_strings))

  # # Clinical Code Presence Flags (already existing logic)
  # chunk[, cr1_present := FALSE]
  # chunk[
  #   !is.na(clin_c1) & !clin_c1 %chin% na_values_combined,
  #   cr1_present := TRUE
  # ]

  # chunk[, cr2_present := FALSE]
  # chunk[
  #   !is.na(clin_c2) & !clin_c2 %chin% na_values_combined,
  #   cr2_present := TRUE
  # ]

  # # Clinical Code Presence Flags (already existing logic)
  # chunk[, clin_icd_present := FALSE]
  # chunk[
  #   !is.na(clin_icd1) & !clin_icd1 %chin% na_values_combined,
  #   clin_icd_present := TRUE
  # ]

  # chunk[, clin_rvs_present := FALSE]
  # chunk[
  #   !is.na(clin_rvs1) & !clin_rvs1 %chin% na_values_combined,
  #   clin_rvs_present := TRUE
  # ]

  # Remove original clinical columns efficiently
  # clin_icd_colnames <- grep("^clin_icd\\d+", names(chunk), value = TRUE)
  # clin_rvs_colnames <- grep("^clin_rvs\\d+", names(chunk), value = TRUE)
  # chunk[, (c(clin_icd_colnames, clin_rvs_colnames)) := NULL]

  # # Safeguard: If pat_bdate doesn't exist, set it to NA_Date_
  # if (!"pat_bdate" %in% names(chunk)) {
  #   set(chunk, j = "pat_bdate", value = NA_Date_)
  # }

  ##############################################################################
  # 2. Reformatting
  ##############################################################################

  # # Convert necessary columns to character type
  # char_cols <- intersect(names(chunk), unlist(expected_types["character"]))
  # chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]

  # # Convert numerical columns
  # int_cols <- intersect(names(chunk), unlist(expected_types["integer"]))
  # num_cols <- intersect(names(chunk), unlist(expected_types["numeric"]))
  # chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  # chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # # Patient Age Presence Flag (already existing)
  # chunk[, pat_age_present := FALSE]
  # chunk[
  #   !is.na(pat_age),
  #   pat_age_present := TRUE
  # ]

  ##############################################################################
  # 3. Date and Time Handling
  ##############################################################################

  # # Define original date & time columns and their replacements
  # cols <- c(
  #   "date_adm_orig", "date_dis_orig", "date_rec_orig", "date_ref_orig",
  #   "date_check_orig", "pat_bdate_orig", "date_ext_orig", "time_adm_orig",
  #   "time_dis_orig"
  # )
  # vals <- c(
  #   "date_adm", "date_dis", "date_rec", "date_ref",
  #   "date_check", "pat_bdate", "date_ext", "time_adm", "time_dis"
  # )

  # # Copy values from `vals` columns to `cols`
  # for (i in seq_along(cols)) set(chunk, j = cols[i], value = chunk[[vals[i]]])

  # # Generate presence flags for date and time columns
  # for (col in cols) {
  #   chunk[, paste0(col, "_present") := FALSE]
  #   chunk[
  #     !is.na(get(col)) & !get(col) %chin% na_values_combined,
  #     paste0(col, "_present") := TRUE
  #   ]
  # }

  # # Convert date columns from m/d/y format and clean invalid dates
  # date_cols <- c(
  #   "date_adm", "date_dis", "date_rec", "date_ref",
  #   "date_check", "pat_bdate", "date_ext"
  # )
  # chunk[, (date_cols) := lapply(.SD, function(x) {
  #   x <- sub("\\.\\d+ ", " ", x) # Remove decimal seconds
  #   dt <- as.POSIXct(x, format = "%m/%d/%Y", tz = "UTC")
  #   dt[dt < as.POSIXct("1900-01-01", tz = "UTC")] <- NA
  #   as.Date(dt)
  # }), .SDcols = date_cols]

  # if (!"age_gap_flag" %in% names(chunk)) {
  #   chunk[, age_gap_flag := NA] # Create it with NA if missing
  # }

  # chunk[
  #   !is.na(date_adm) & !is.na(pat_bdate) & !is.na(pat_age) &
  #     abs((as.numeric(as.Date(date_adm) - pat_bdate) / 365.25) - pat_age) >= 1,
  #   age_gap_flag := TRUE
  # ]

  # # Convert and clean time columns
  # time_cols <- c("time_adm", "time_dis")
  # chunk[, (time_cols) := lapply(.SD, function(x) {
  #   x <- ifelse(is.na(x), "00:00", x) # Default missing times to "00:00"
  #   ifelse(nchar(x) <= 5, paste0(x, ":00"), x) # Ensure seconds are included
  # }), .SDcols = time_cols]

  ##############################################################################
  # 4. Final Processing & Return
  ##############################################################################

  return(chunk)
}


# Data Cleaning Pipeline for DRG Processing
# This script processes large datasets in parts, applying
# parallel processing for efficiency.
# It reads, chunks, processes, and consolidates data before
# saving intermediate and final outputs.
abs_start_time <- as.character(Sys.time())
setkey(claims, id_series) # Do this ONCE before looping over chunks
for (loop_part in 1:split_parts) {
  start_time <- Sys.time() # Record start time for processing
  # Step 1: Read the appropriate file
  read_in_dt <- read_appropriate_file(loop_part)
  if ("PSEUDO_CLAIMSERIES" %in% names(read_in_dt)) {
    read_in_dt <- read_in_dt[, id_series := trimws(as.character(PSEUDO_CLAIMSERIES))][claims, nomatch = 0, on = "id_series"]
  } else {
    read_in_dt <- read_in_dt[, id_series := trimws(as.character(CLAIM_SERIES_ID))][claims, nomatch = 0, on = "id_series"]
  }

  # Step 2: Split the data into chunks for parallel processing
  chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
  chunks <- split(read_in_dt, rep(1:nthreads,
    each = chunk_size,
    length.out = nrow(read_in_dt)
  ))

  # Step 3: Process chunks in parallel or sequentially
  cat(paste0("\rStart processing part  ", loop_part, " of ", split_parts))
  flush.console()

  parallel_results <-
    if (to_parallel) {
      mclapply(chunks, process_chunk, mc.cores = nthreads)
    } else if (!to_debug) {
      lapply(chunks, process_chunk)
    } else if (to_debug) {
      list(process_chunk(chunks[[1]]))
    } else {
      stop("Invalid parameters")
    }


  # Consolidate processed chunks
  summarized_dt <- rbindlist(parallel_results)

  # Step 4: Save processed data if required
  if (to_write) {
    saveRDS(
      summarized_dt, here(chkpt_1_path, paste0(
        chkpt_1_prefix, year, suffix, "bdate_",
        "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      )),
      compress = TRUE
    )
  }

  # Step 5: Log processing time and update status
  processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(),
    start_time,
    units = "secs"
  ))
  print_status_update(loop_part, split_parts, processing_times, "clean")
  # Cleanup memory
  rm(read_in_dt, summarized_dt)
  invisible(gc())
}

# Step 6: Combine all processed parts into a master data table
master_dt_list <- mclapply(1:split_parts,
  function(split_part) {
    read_part <- readRDS(
      here(chkpt_1_path, paste0(
        chkpt_1_prefix, year, suffix, "bdate_", "part_",
        sprintf("%02d", split_part), "_of_", split_parts, ".rds"
      ))
    )
    return(read_part)
  },
  mc.cores = nthreads
)

# Merge all parts into a single data table
master_dt <- rbindlist(master_dt_list, fill = TRUE)
rm(master_dt_list)
invisible(gc())

# Step 7: Save final processed data
if (to_write) {
  saveRDS(master_dt, here(
    chkpt_2_path, paste0(
      chkpt_2_prefix, year, suffix, "bdate_", ".rds"
    )
  ), compress = FALSE)
}

# Save a pre-final version of the master dataset
# saveRDS(master_dt, here(
#   chkpt_2_path,
#   paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
# ), compress = FALSE)


# Ensure column names are unique
colnames(master_dt) <- make.names(colnames(master_dt), unique = TRUE)

# Define NA-like values
na_values_combined <- unique(c(na_values, na_like_strings))

# Function to compute top 20 unique values with frequency and percentage
get_top_20 <- function(df, column) {
  df %>%
    filter(!(.data[[column]] %in% na_values_combined)) %>% # Exclude NA-like values
    count(!!sym(column), name = "frequency") %>%
    arrange(desc(frequency)) %>%
    mutate(percentage = round(100 * frequency / sum(frequency), 2)) %>%
    head(20)
}

# Function to compute the percentage of non-null values, frequency, and denominator
get_non_null_stats <- function(df, column) {
  total_rows <- nrow(df)
  non_null_count <- sum(!(df[[column]] %in% na_values_combined), na.rm = TRUE)
  percentage_non_null <- round(100 * non_null_count / total_rows, 2)

  tibble(
    Column = column,
    Frequency = non_null_count,
    Denominator = total_rows,
    Non_Null_Percentage = percentage_non_null
  )
}

# Compute non-null statistics for both columns
non_null_table <- bind_rows(
  get_non_null_stats(master_dt, "clin_icd1"),
  get_non_null_stats(master_dt, "clin_rvs1")
)

# Print results
# print(get_top_20(master_dt, "clin_icd1"))
# print(get_top_20(master_dt, "clin_rvs1"))
# print(as.data.table(non_null_table))


# str(master_dt)
# print((nrow(master_dt[clin_pdx_source == 99]) / nrow(master_dt)) * 100)
# print(nrow(master_dt[clin_pdx_source == 99]))
# print(nrow(master_dt[is.na(pat_bdate)]))
# print(nrow(master_dt[is.na(pat_age)]))


# fwrite(readRDS(here(
#   chkpt_2_path,
#   paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
# )), "~/drg-pipeline/data-cleaning/debug/refactor.csv")


# Final preparations for BQ upload
# Load the dataset from the tmp chkpt
result <- readRDS(here(
  chkpt_2_path,
  paste0(chkpt_2_prefix, year, suffix, "bdate_", ".rds")
))

# # Add is_covid variable
# # Identifies COVID-related claims by checking multiple clinical fields
# result[, is_covid := {
#   covid_found <- rep(FALSE, .N) # Initialize all rows as FALSE

#   # Check each field sequentially, marking matches as TRUE
#   not_found <- !covid_found
#   # Check primary diagnosis
#   covid_found[not_found] <- clin_c1[not_found] %chin% covid_rvs

#   not_found <- !covid_found
#   # Check secondary diagnosis
#   covid_found[not_found] <- clin_c2[not_found] %chin% covid_rvs

#   not_found <- !covid_found
#   # Check coded diagnosis
#   covid_found[not_found] <- c2[not_found] %chin% covid_rvs

#   not_found <- !covid_found
#   # Check additional coded diagnosis
#   covid_found[not_found] <- c1[not_found] %chin% covid_rvs

#   not_found <- !covid_found
#   covid_found[not_found] <- sapply(
#     clin_rvs[not_found],
#     function(row) any(row %chin% covid_rvs)
#   ) # Check procedure codes

#   not_found <- !covid_found
#   covid_found[not_found] <- sapply(
#     clin_sdx[not_found],
#     function(row) any(row %chin% covid_rvs)
#   ) # Check supporting diagnoses

#   not_found <- !covid_found
#   covid_found[not_found] <- sapply(
#     clin_proc[not_found],
#     function(row) any(row %chin% covid_rvs)
#   ) # Check performed procedures

#   covid_found # Return logical vector of COVID matches
# }]

# Save the processed dataset to a new chkpt before BQ upload
# saveRDS(result, here(
#   chkpt_2_path,
#   paste0(chkpt_2_prefix, year, suffix, "bdate_", "master", ".rds")
# ))

# Subset the dataset for BQ
# Keep only relevant columns needed for BigQuery upload
result <- result[, .(
  # Identifiers
  id_series,
  clin_c1,
  clin_c2
  # ,
  # cr1_present,
  # cr2_present

  # # Hardcoded flag columns
  # date_adm_orig_present,
  # date_dis_orig_present,
  # date_rec_orig_present,
  # date_ref_orig_present,
  # date_check_orig_present,
  # pat_bdate_orig_present,
  # date_ext_orig_present,
  # time_adm_orig_present,
  # time_dis_orig_present,

  # # Other flags
  # clin_icd_present,
  # clin_rvs_present,
  # age_gap_flag
)]

# Save the processed dataset to a new chkpt before BQ upload
saveRDS(result, here(
  chkpt_2_path,
  paste0(chkpt_2_prefix, year, suffix, "bdate_", "bq_subset", ".rds")
))


# # Calculate total row count
# n_total <- nrow(result)

# # Function to compute row counts and percentages for each flag column
# # (This simply uses the flag value already set in process_chunk)
# compute_stats <- function(column) {
#   n_present <- nrow(result[get(column) == TRUE])
#   percent_present <- (n_present / n_total) * 100
#   return(list(n_present = n_present, percent_present = round(percent_present, 2)))
# }

# # Compute stats for each `_present` column
# stats_list <- list(
#   "year" = year,
#   "n_total" = n_total,
#   "clin_c1_top_20" = paste(head(unique(result$clin_icd1), 20), collapse = ", "),
#   "clin_c2_top_20" = paste(head(unique(result$clin_rvs1), 20), collapse = ", "),
#   "cr1" = compute_stats("cr1_present")$n_present,
#   "cr1_percent" = compute_stats("cr1_present")$percent_present,
#   "cr2" = compute_stats("cr2_present")$n_present,
#   "cr2_percent" = compute_stats("cr2_present")$percent_present,
#   # Age Consistency Stats
#   "n_age_consistent" = nrow(result[is.na(age_gap_flag)]),
#   "percent_age_consistent" = round((nrow(result[is.na(age_gap_flag)]) / n_total) * 100, 2),

#   # Date & Time Presence Stats
#   "n_date_adm_orig_present" = compute_stats("date_adm_orig_present")$n_present,
#   "percent_date_adm_orig_present" = compute_stats("date_adm_orig_present")$percent_present,
#   "n_date_dis_orig_present" = compute_stats("date_dis_orig_present")$n_present,
#   "percent_date_dis_orig_present" = compute_stats("date_dis_orig_present")$percent_present,
#   "n_date_rec_orig_present" = compute_stats("date_rec_orig_present")$n_present,
#   "percent_date_rec_orig_present" = compute_stats("date_rec_orig_present")$percent_present,
#   "n_date_ref_orig_present" = compute_stats("date_ref_orig_present")$n_present,
#   "percent_date_ref_orig_present" = compute_stats("date_ref_orig_present")$percent_present,
#   "n_date_check_orig_present" = compute_stats("date_check_orig_present")$n_present,
#   "percent_date_check_orig_present" = compute_stats("date_check_orig_present")$percent_present,
#   "n_pat_bdate_orig_present" = compute_stats("pat_bdate_orig_present")$n_present,
#   "percent_pat_bdate_orig_present" = compute_stats("pat_bdate_orig_present")$percent_present,
#   "n_date_ext_orig_present" = compute_stats("date_ext_orig_present")$n_present,
#   "percent_date_ext_orig_present" = compute_stats("date_ext_orig_present")$percent_present,
#   "n_time_adm_orig_present" = compute_stats("time_adm_orig_present")$n_present,
#   "percent_time_adm_orig_present" = compute_stats("time_adm_orig_present")$percent_present,
#   "n_time_dis_orig_present" = compute_stats("time_dis_orig_present")$n_present,
#   "percent_time_dis_orig_present" = compute_stats("time_dis_orig_present")$percent_present,

#   # Clinical Code Presence Stats
#   # Note: The original columns (e.g., clin_icd1/clin_rvs1) have been deleted,
#   # so we simply use the pre-computed flag columns.
#   "n_icd_present" = compute_stats("clin_icd_present")$n_present,
#   "percent_icd_present" = compute_stats("clin_icd_present")$percent_present,
#   "n_rvs_present" = compute_stats("clin_rvs_present")$n_present,
#   "percent_rvs_present" = compute_stats("clin_rvs_present")$percent_present
# )

# # Convert list to a data.table and transpose it for proper CSV formatting
# bdate_stats <- data.table(Variable = names(stats_list), Value = unlist(stats_list))

# Write the statistics to CSV
fwrite(as.data.table(non_null_table), paste0("~/drg-pipeline/data-cleaning/debug/bdate_icd_rvs_stats_", year, suffix, "clin_icd_rvs_percent_non_null", ".csv"))


# BQ upload
# if (to_bq) {
#   # Define BQ table name
#   if (!to_sample) bq_table <- paste0("claims_", year)

#   # Attempt to delete the table if it exists
#   tryCatch(
#     bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table)),
#     error = function(e) {
#       if (grepl("Not found", e, ignore.case = TRUE)) {
#         message("Table does not exist, nothing to drop.")
#       } else {
#         stop(e)
#       }
#     }
#   )

#   # Create the BQ table if it does not exist
#   tryCatch(
#     bq_table_create(
#       bq_table(gcp_proj, bq_dataset, bq_table),
#       fields = fromJSON(here(
#         "data-cleaning/r_scripts_v2",
#         "bq_schema_cleaning.json"
#       ), simplifyDataFrame = FALSE)
#     ),
#     error = function(e) {
#       if (grepl("already exists", e, ignore.case = TRUE)) {
#         message("Table already exists. Skipping creation and upload.")
#       } else {
#         stop(e)
#       }
#     }
#   )

#   if (to_bq) {
#     # Define chunk size for upload
#     chunk_size <- 250000
#     # Calculate number of chunks
#     num_chunks <- ceiling(nrow(result) / chunk_size)

#     for (i in seq_len(num_chunks)) {
#       # Extract chunk
#       chunk <- result[
#         ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result)),
#       ]

#       # Upload chunk to BQ
#       bq_table_upload(
#         bq_table(gcp_proj, bq_dataset, bq_table),
#         values = chunk,
#         write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
#       )
#     }
#   }
# }

