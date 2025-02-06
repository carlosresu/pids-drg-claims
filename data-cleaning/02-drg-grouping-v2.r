# # Delete all R objects and run garbage collection so we start with a clean slate
# rm(list = ls())
# invisible(gc())

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
# cat("Parallelization:", to_parallel, "\n")
# # TODO: Add description here
# to_debug <- FALSE
# verbose_output <- if (to_debug) TRUE else FALSE

# to_generate_subset <- TRUE

# to_py_prompt <- TRUE
# to_python <- TRUE
# to_generate_py_fwrite <- TRUE
# to_generate_feather <- TRUE
# to_py_bq <- FALSE

# to_thai_prompt <- TRUE
# to_thai <- TRUE
# to_thai_bq <- FALSE
# to_generate_thai_txt <- TRUE
# to_thai_all_years <- FALSE

# to_spc <- FALSE

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

# Function to install and load packages quietly
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE)
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
      install.packages("remotes")
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo)
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

# detect available threads
nthreads <- parallelly::availableCores()


scripts_path <- here("data-cleaning/r_scripts_v2")

# List all R files in the directory with full paths, sorted by filename
r_files <- list.files(scripts_path, pattern = "\\.R$", full.names = TRUE)

# Source each file sequentially
for (file in r_files) {
  if (verbose_output) message(Sys.time(), " Sourcing: ", file)
  invisible(source(file))
}

message(year_to_load)

bq_dataset <- "drg_claims"


# Load raw claims from GCS only if they don't exist on the VM yet
for (year in 2018:2023) {
  # Assign the correct file extension based on the year
  file_type <- if (year %in% c(2022:2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year, file_type)
  bq_name <- paste0(full_claims_bq_prefix, year, file_type)

  # Check if the file exists in the target directory
  file_path <- here(raw_claims_path, file_name)
  exists <- file.exists(file_path)

  # If the file does not exist, run the gsutil cp command
  if (!exists) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(
        paste0(
          "cd .. && gsutil cp gs://phic-claims-raw/",
          bq_name, " ", raw_claims_path
        ),
        intern = FALSE, ignore.stderr = FALSE
      )
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  } else {
    next
    # message(paste(
    #   "File", file_name,
    #   "already exists in the target directory. Skipping download.\n"
    # ))
  }
}


# Enable caching and printing options for data mapping
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- TRUE # Set to TRUE to print mapping data tables

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


if (to_generate_subset && (to_python || (to_thai && to_generate_thai_txt) || to_spc)) {
  if (!to_thai_all_years) {
    cat("\rReading final\n")
    flush.console()
    result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")))

    print("Total Rows")
    print(nrow(result))
    result <- result[clin_outpatient == FALSE]
    print("Inpatient Rows")
    print(nrow(result))
    result <- result[is_covid == FALSE]
    print("Inpatient Non-Covid Rows")
    print(nrow(result))

    cat("\rComputing pat_bdate\n")
    # Impute missing birthdates (pat_bdate) based on admission date (date_adm) and age (pat_age)
    result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]

    # # Ensure birthdates (pat_bdate) are before the admission date (date_adm),
    # # adjusting by setting them to one day before the admission date if needed
    # result[
    #   !is.na(pat_bdate) & !is.na(date_adm) & pat_bdate >= as.Date(date_adm),
    #   pat_bdate := as.Date(date_adm) - 1
    # ]

    # Set pat_bdate to NA if it falls before the earliest valid date (January 1, 1900)
    result[!is.na(pat_bdate) & pat_bdate < as.Date("1900-01-01"), pat_bdate := NA_Date_]

    cat("\rComputing pat_ageday\n")
    result[, pat_ageday := NA_real_]
    result[
      !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & !is.na(date_adm) & !is.na(pat_bdate) & is.na(pat_ageday),
      pat_ageday := as.integer(difftime(as.Date(format(date_adm, "%Y-%m-%d")), as.Date(pat_bdate), units = "days"))
    ]
    result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday > 365 | pat_ageday < 0), pat_ageday := 0]
    result[
      !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday == 365),
      `:=`(
        pat_ageday = 364, # Update pat_ageday to 364
        pat_bdate = pat_bdate + 1 # Add 1 day to pat_bdate
      )
    ]
    cat("\rFlooring pat_ageday\n")
    result[!is.na(pat_ageday), pat_ageday := as.integer(floor(pat_ageday))]

    cat("\rComputing pat_bwt\n")
    bw_dist <- c(
      round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
      round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
      round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
      round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
      round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
      round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
      round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
      round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
    )

    # Create the zero_mask condition where pat_age is between 0 and 1 (newborns)
    zero_mask <- result[, pat_age >= 0 & pat_age < 1]

    # Apply bwt only if pat_bwt is NA and zero_mask is TRUE
    result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, function(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
    result[!zero_mask, pat_bwt := NA_real_]

    cat("\rWriting final\n")
    flush.console()
    saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
  } else if (to_thai_all_years) {
    for (year_to_load in c(2018:2023)) {
      year_to_load <<- year_to_load
      year_to_load <- year_to_load
      cat("\rReading final\n")
      flush.console()
      result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")))

      print("Total Rows")
      print(nrow(result))
      result <- result[clin_outpatient == FALSE]
      print("Inpatient Rows")
      print(nrow(result))
      result <- result[is_covid == FALSE]
      print("Inpatient Non-Covid Rows")
      print(nrow(result))

      cat("\rComputing pat_bdate\n")
      # Impute missing birthdates (pat_bdate) based on admission date (date_adm) and age (pat_age)
      result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]

      # # Ensure birthdates (pat_bdate) are before the admission date (date_adm),
      # # adjusting by setting them to one day before the admission date if needed
      # result[
      #   !is.na(pat_bdate) & !is.na(date_adm) & pat_bdate >= as.Date(date_adm),
      #   pat_bdate := as.Date(date_adm) - 1
      # ]

      # Set pat_bdate to NA if it falls before the earliest valid date (January 1, 1900)
      result[!is.na(pat_bdate) & pat_bdate < as.Date("1900-01-01"), pat_bdate := NA_Date_]

      cat("\rComputing pat_ageday\n")
      result[, pat_ageday := NA_real_]
      result[
        !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & !is.na(date_adm) & !is.na(pat_bdate) & is.na(pat_ageday),
        pat_ageday := as.integer(difftime(as.Date(format(date_adm, "%Y-%m-%d")), as.Date(pat_bdate), units = "days"))
      ]
      result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday > 365 | pat_ageday < 0), pat_ageday := 0]
      result[
        !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & (pat_ageday == 365),
        `:=`(
          pat_ageday = 364, # Update pat_ageday to 364
          pat_bdate = pat_bdate + 1 # Add 1 day to pat_bdate
        )
      ]
      cat("\rFlooring pat_ageday\n")
      result[!is.na(pat_ageday), pat_ageday := as.integer(floor(pat_ageday))]

      cat("\rComputing pat_bwt\n")
      bw_dist <- c(
        round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
        round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
        round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
        round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
        round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
        round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
        round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
        round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
      )

      # Create the zero_mask condition where pat_age is between 0 and 1 (newborns)
      zero_mask <- result[, pat_age >= 0 & pat_age < 1]

      # Apply bwt only if pat_bwt is NA and zero_mask is TRUE
      result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, function(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
      result[!zero_mask, pat_bwt := NA_real_]

      cat("\rWriting final\n")
      flush.console()
      saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
    }
  }
  # Check for duplicates in id_series
  if (!to_thai_all_years && any(duplicated(result$id_series))) {
    # Identify duplicates
    duplicate_ids <- result$id_series[duplicated(result$id_series)]

    # Extract rows with duplicate id_series
    duplicate_rows <- result[id_series %in% duplicate_ids, ]

    # Print rows with duplicates
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)

    # Stop execution
    stop("The 'id_series' column contains duplicates. Execution stopped.")
  }
  print(nrow(result))
  # Check for duplicates in id_series
  if (!to_thai_all_years && any(duplicated(result$caseid))) {
    # Identify duplicates
    duplicate_ids <- result$caseid[duplicated(result$caseid)]

    # Extract rows with duplicate id_series
    duplicate_rows <- result[caseid %in% duplicate_ids, ]

    # Print rows with duplicates
    cat("Rows with duplicate 'caseid':\n")
    print(duplicate_rows)

    # Stop execution
    stop("The 'caseid' column contains duplicates. Execution stopped.")
  }
  print(nrow(result))
}


set.seed(123)

library(ggplot2)

bw_dist <- c(
  round(runif(2, 0.5, 0.9), 3), # Random bwt between 0.5 and 0.9 for 2 newborns
  round(runif(8, 1.1, 1.4), 3), # Random bwt between 1.1 and 1.4 for 8 newborns
  round(runif(19, 1.6, 1.9), 3), # Random bwt between 1.6 and 1.9 for 19 newborns
  round(runif(95, 2.1, 2.4), 3), # Random bwt between 2.1 and 2.4 for 95 newborns
  round(runif(381, 2.6, 2.9), 3), # Random bwt between 2.6 and 2.9 for 381 newborns
  round(runif(375, 3.1, 3.4), 3), # Random bwt between 3.1 and 3.4 for 375 newborns
  round(runif(115, 3.5, 4.0), 3), # Random bwt between 3.5 and 4.0 for 115 newborns
  round(runif(6, 0.5, 4.0), 3) # Random bwt between 0.5 and 4.0 for 6 newborns
)


# Create a data frame from the vector (required for ggplot2)
bw_data <- data.frame(bw_dist = bw_dist)

# Create the histogram using ggplot2
ggplot(bw_data, aes(x = bw_dist)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(
    title = "Generated Live Filipino Infant Birthweights (Bin Width 0.5 kg)",
    x = "Newborn Birthweight (in kg)",
    y = "Number of Cases"
  ) +
  theme_minimal()


# library(ggplot2)

# # Birthweight data
# birthweight_categories <- c(
#   "Less than 1,000",
#   "1,000 - 1,499",
#   "1,500 - 1,999",
#   "2,000 - 2,499",
#   "2,500 - 2,999",
#   "3,000 - 3,499",
#   "3,500 & Over"
# )

# birthweight_totals <- c(
#   2060, # Less than 1,000
#   12685, # 1,000 - 1,499
#   27989, # 1,500 - 1,999
#   143252, # 2,000 - 2,499
#   569250, # 2,500 - 2,999
#   533340, # 3,000 - 3,499
#   158838 # 3,500 & Over
# )

# # Midpoints of each range for the Gaussian approximation
# midpoints <- c(750, 1250, 1750, 2250, 2750, 3250, 3750)

# # Create a data frame
# birthweight_data <- data.frame(
#   midpoints = midpoints,
#   totals = birthweight_totals
# )

# # Fit a Gaussian distribution
# mean_weight <- sum(midpoints * birthweight_totals) / sum(birthweight_totals) # Weighted mean
# sd_weight <- sqrt(sum(birthweight_totals * (midpoints - mean_weight)^2) / sum(birthweight_totals)) # Weighted SD

# # Generate data for the bell curve
# x_vals <- seq(500, 4000, length.out = 500) # Range of birthweights
# bell_curve <- data.frame(
#   x = x_vals,
#   y = dnorm(x_vals, mean = mean_weight, sd = sd_weight) * sum(birthweight_totals) * (midpoints[2] - midpoints[1])
# )

# # Plot the histogram with the bell curve overlay
# ggplot() +
#   geom_col(
#     data = birthweight_data,
#     aes(x = midpoints, y = totals),
#     fill = "skyblue",
#     color = "black",
#     width = 500
#   ) +
#   geom_line(
#     data = bell_curve,
#     aes(x = x, y = y),
#     color = "red",
#     size = 1
#   ) +
#   labs(
#     title = "Distribution of Birth Weights with Bell Curve Overlay",
#     x = "Birth Weight (in grams)",
#     y = "Number of Cases"
#   ) +
#   theme_minimal()


# set.seed(123)

# library(ggplot2)
# library(truncnorm)

# # Step 1: Define the birthweight data
# birthweight_categories <- c(
#   "Less than 1,000",
#   "1,000 - 1,499",
#   "1,500 - 1,999",
#   "2,000 - 2,499",
#   "2,500 - 2,999",
#   "3,000 - 3,499",
#   "3,500 & Over"
# )

# birthweight_totals <- c(
#   2060, # Less than 1,000
#   12685, # 1,000 - 1,499
#   27989, # 1,500 - 1,999
#   143252, # 2,000 - 2,499
#   569250, # 2,500 - 2,999
#   533340, # 3,000 - 3,499
#   158838 # 3,500 & Over
# )

# # Midpoints of each category
# midpoints <- c(750, 1250, 1750, 2250, 2750, 3250, 3750)

# # Step 2: Calculate the weighted mean and standard deviation
# mean_weight <- sum(midpoints * birthweight_totals) / sum(birthweight_totals) # Weighted mean
# sd_weight <- sqrt(sum(birthweight_totals * (midpoints - mean_weight)^2) / sum(birthweight_totals)) # Weighted SD

# # Step 3: Transform mean and SD to match the [0.5, 4.0] scale
# mean_weight_scaled <- mean_weight / 1000 # Convert mean to [0.5, 4.0] range
# sd_weight_scaled <- sd_weight / 1000 # Scale standard deviation similarly

# # Step 4: Generate 1001 points using a truncated normal distribution
# random_birthweights <- rtruncnorm(
#   n = 1001,
#   a = 0.5,
#   b = 4.0,
#   mean = mean_weight_scaled,
#   sd = sd_weight_scaled
# )

# # Step 5: Compute mean, median, and percentiles
# birthweight_mean <- mean(random_birthweights)
# birthweight_median <- median(random_birthweights)
# birthweight_percentiles <- quantile(random_birthweights, probs = seq(0.1, 0.9, by = 0.1))

# # Step 6: Display results
# cat("Mean (Scaled):", round(birthweight_mean, 3), "\n")
# cat("Median (Scaled):", round(birthweight_median, 3), "\n")
# cat("Percentiles (Scaled):\n")
# print(round(birthweight_percentiles, 3))

# # Step 7: Plot the histogram of generated data with a truncated x-axis
# ggplot(data.frame(birthweight = random_birthweights), aes(x = birthweight)) +
#   geom_histogram(binwidth = 0.25, fill = "skyblue", color = "black", alpha = 0.7) +
#   stat_function(
#     fun = function(x) {
#       dtruncnorm(x, a = 0.5, b = 4.0, mean = mean_weight_scaled, sd = sd_weight_scaled) * 1001 * 0.25
#     },
#     color = "red",
#     size = 1
#   ) +
#   scale_x_continuous(
#     limits = c(0.5, 4.0), # Truncate x-axis to 0.5 to 4.0
#     breaks = seq(0.5, 4, by = 0.5) # Tick marks at increments of 0.5
#   ) +
#   labs(
#     title = "Randomly Generated Birthweights with Truncated Bell Curve",
#     x = "Birth Weight (in kg)",
#     y = "Frequency"
#   ) +
#   theme_minimal()


if (to_python && !to_generate_subset && to_generate_feather) {
  cat("\rReading final\n")
  flush.console()
  result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
}

if (to_python && to_generate_feather) {
  message("Renaming columns")
  # Convert relevant data types
  result[, patage := as.numeric(pat_age)]
  result[, patsex := as.character(pat_sex)]
  result[, birthweight := as.numeric(pat_bwt)]
  result[, discharge := as.integer(clin_discharge)]
  result[, dob := as.Date(pat_bdate)]
  result[, ageday := as.integer(pat_ageday)]
  result[, pdx := clin_pdx]

  # Handle ICD splitting for columns that are lists of character vectors
  split_codes_from_list <- function(dt, column, prefix, max_cols) {
    split_list <- dt[[column]] # Extract the list column
    # Pad each list to the specified max_cols with NAs if not enough elements
    # split_cols <- lapply(1:max_cols, function(i) sapply(split_list, function(x) if (length(x) >= i) x[[i]] else NA_character_))
    # Use mclapply for parallel processing
    split_cols <- parallel::mclapply(
      1:max_cols,
      function(i) sapply(split_list, function(x) if (length(x) >= i) x[[i]] else NA_character_),
      mc.cores = nthreads # Automatically use all available cores
    )

    split_dt <- as.data.table(split_cols)
    setnames(split_dt, paste0(prefix, 1:max_cols))
    return(split_dt)
  }

  # Apply the function to split clin_sdx and clin_proc
  message("Splitting clin_sdx")
  sdx_columns <- split_codes_from_list(result, "clin_sdx", "sdx", 12)
  message("Splitting clin_proc")
  proc_columns <- split_codes_from_list(result, "clin_proc", "proc", 20)

  # Combine the split columns back into the result
  message("cbind results")
  result <- cbind(result, sdx_columns, proc_columns)

  # Replace NA in non-date columns with "None"
  # non_date_columns <- c("patsex", "pdx", paste0("sdx", 1:12), paste0("proc", 1:20))
  # result[, (non_date_columns) := lapply(.SD, function(x) ifelse(is.na(x), "None", x)), .SDcols = non_date_columns]

  # Prepare the final data table for writing
  for_fwrite <- result[, c(
    "id_series", "date_adm", "date_dis", "time_adm", "time_dis", "patage", "dob", "patsex", "discharge", "pdx",
    paste0("sdx", 1:12), paste0("proc", 1:20), "birthweight", "ageday"
  ), with = FALSE]
  message("Formatting date_adm")
  for_fwrite[, date_adm := format(date_adm, "%Y-%m-%d %H:%M:%S")]
  message("Formatting date_dis")
  for_fwrite[, date_dis := format(date_dis, "%Y-%m-%d %H:%M:%S")]

  for_fwrite[, time_adm := NULL]
  for_fwrite[, time_dis := NULL]

  # Write the final table to a CSV file
  message("Writing to csv")
  fwrite(for_fwrite, here(checkpoint_7_path, paste0(checkpoint_7b_prefix, suffix, ".csv")))
  message("Creating summary table")
  # Create a summary table that shows the count of non-null values for each column
  summary_table <- for_fwrite[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = names(for_fwrite)]

  # Transpose the summary table to make it more readable
  summary_table <- transpose(summary_table)
  setnames(summary_table, "Non-Null Count")
  summary_table[, Column := names(for_fwrite)]

  # Reorder the summary table to show the columns
  setcolorder(summary_table, c("Column", "Non-Null Count"))

  # Print the summary table
  message("Printing summary table")
  print(summary_table)
  saveRDS(for_fwrite, here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
}


if (to_python) {
  if (!to_generate_py_fwrite && to_generate_feather) for_fwrite <- readRDS(here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
  if (to_generate_feather) write_feather(as.data.frame(for_fwrite), here(checkpoint_7_path, paste0("python_input_", year_to_load, suffix, ".feather")))

  # Prompt for manual confirmation if needed
  if (to_py_prompt) {
    response <- tolower(readline(prompt = "Have you run the Python grouper manually? (y/n): "))
    if (response != "y") {
      stop("Python Grouper not run yet. Script terminated. Continue on manually if necessary")
    }
    message("Continuing with the script...\n")
  } else {
    message("Python Grouper is assumed to have been run already. Continuing with the script...\n")
  }

  output_dt <- as.data.table(read_feather(here(checkpoint_8_path, paste0("python_output_", year_to_load, suffix, ".feather"))))
}


if (to_python) {
  # Rename columns to match required names if necessary
  setnames(output_dt,
    old = c("drg", "pdc", "pccl", "error_code", "warning_code"),
    new = c("py_drg", "py_pdc", "py_pccl", "py_err", "py_warn"), skip_absent = TRUE
  )

  # Select only the required columns
  required_columns <- c("id_series", "py_drg", "py_pdc", "py_pccl", "py_err", "py_warn")
  output_dt <- output_dt[, ..required_columns]

  # Adjust data types
  output_dt[, py_drg := as.character(py_drg)]
  output_dt[, py_pdc := as.character(py_pdc)]
  output_dt[, py_pccl := as.numeric(py_pccl)]

  # Convert 'py_err' and 'py_warn' to arrays (list of character vectors)
  array_columns <- c("py_err", "py_warn")

  process_error_warning_column <- function(col) {
    lapply(col, function(x) {
      # Flatten x to a character vector
      x <- unlist(x)
      x <- as.character(x)

      # If x is NULL or length zero after unlisting, return character(0)
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }

      # Remove any NA values from x
      x <- x[!is.na(x)]

      # Remove any "None", "NA", or empty strings from x
      x <- x[!(x %in% c("None", "NA", "NaN", ""))]

      # If x is now length zero after cleaning, return character(0)
      if (length(x) == 0) {
        return(character(0))
      }

      # Now split each element of x by comma and optional whitespace
      split_x <- unlist(strsplit(x, ",\\s*"))

      # Remove any empty strings, "NA", or "None" from split_x
      split_x <- split_x[!(split_x %in% c("", "NaN", "NA", "None")) & !is.na(split_x)]

      # Return character(0) if split_x is empty after cleaning
      if (length(split_x) == 0) {
        return(character(0))
      } else {
        return(split_x)
      }
    })
  }

  # Apply the processing function to the columns
  output_dt[, (array_columns) := mclapply(.SD, process_error_warning_column, mc.cores = nthreads), .SDcols = array_columns]

  # Now 'output_dt' is your final result
  # You can proceed to use 'output_dt' as needed

  # Replace <NA> values in 'py_drg' and 'py_pdc' with character(0)
  output_dt[, py_drg := ifelse(is.na(py_drg), "", py_drg)]
  output_dt[, py_pdc := ifelse(is.na(py_pdc), "", py_pdc)]
  # output_dt[, py_pccl := ifelse(is.nan(py_pccl), NA_real_, py_pccl)]

  # For example, print the first few rows
  # print(head(output_dt[id_series == 24465430]))
  print(head(output_dt, 100))
}


if (to_python) {
  if (to_debug) fwrite(output_dt, "test3.csv")
  # str(output_dt)
}


# Check for duplicates in id_series
if (to_python && any(duplicated(output_dt$id_series))) {
  stop("The 'id_series' column contains duplicates. Execution stopped.")
}


if (to_python && to_py_bq) {
  # Set the table name based on row count
  bq_table <- if (nrow(output_dt) == nrow(result)) {
    paste0("python_", year_to_load)
  } else {
    paste0("temp_python_", year_to_load)
  }

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
          "bq_schema_thai.json"
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
    num_chunks <- ceiling(nrow(output_dt) / chunk_size)

    for (i in seq_len(num_chunks)) {
      cat(paste("\rUploading chunk no.:", i))
      flush.console()
      chunk <- output_dt[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(output_dt)),
      ]

      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = chunk,
        write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
      )
      cat(paste("\rFinished uploading chunk no.:", i))
      flush.console()
    }
  }
}


if (to_thai) {
  if (!to_thai_all_years) {
    if (to_generate_thai_txt) {
      if (to_spc) {
        cat("\rReading stata\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "stata_subset_with_bdate", ".rds")))
      } else {
        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
      }
      # str(result)
      result[, caseid := as.character(seq_len(nrow(result)))]
      result_mapping <- result[, .(id_series, caseid)]
      cat("\rExporting for grouper\n")
      flush.console()
      # Define chunk size
      chunk_size <- 5000000
      num_chunks <- ceiling(nrow(result) / chunk_size)

      for (i in seq_len(num_chunks)) {
        # Define the file path and name for this part
        output_file <- here(
          checkpoint_4_path,
          paste0(
            checkpoint_4_prefix, year_to_load, suffix,
            "part_", i, "_of_", num_chunks, ".txt"
          )
        )

        # Extract the chunk
        start_row <- (i - 1) * chunk_size + 1
        end_row <- min(i * chunk_size, nrow(result))
        chunk <- result[start_row:end_row, ]

        # Export the chunk to a file
        export_for_grouper(chunk, output_file, i)
        message("Saved part ", i, " of ", num_chunks, " to ", output_file)

        # Upload the file to GCS
        message("Uploading part ", i, " of ", num_chunks, " to GCS")
        gcs_upload(
          file = output_file,
          bucket = gcs_bucket,
          name = paste0(gcs_pre_fpath, "/", basename(output_file)),
          predefinedAcl = "bucketLevel"
        )

        # Clean up memory
        rm(chunk)
        gc()
      }
    } else {
      message("Skipping thai txt generation")
    }
  } else if (to_thai_all_years) {
    if (to_generate_thai_txt) {
      for (year_to_load in c(2018:2023)) {
        year_to_load <<- year_to_load
        year_to_load <- year_to_load

        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))

        # str(result)
        result[, caseid := as.character(seq_len(nrow(result)))]
        result_mapping <- result[, .(id_series, caseid)]
        cat("\rExporting for grouper\n")
        flush.console()
        # Define chunk size
        chunk_size <- 5000000
        num_chunks <- ceiling(nrow(result) / chunk_size)

        for (i in seq_len(num_chunks)) {
          # Define the file path and name for this part
          output_file <- here(
            checkpoint_4_path,
            paste0(
              checkpoint_4_prefix, year_to_load, suffix,
              "part_", i, "_of_", num_chunks, ".txt"
            )
          )

          # Extract the chunk
          start_row <- (i - 1) * chunk_size + 1
          end_row <- min(i * chunk_size, nrow(result))
          chunk <- result[start_row:end_row, ]

          # Export the chunk to a file
          export_for_grouper(chunk, output_file, i)
          message("Saved part ", i, " of ", num_chunks, " to ", output_file)

          # Upload the file to GCS
          message("Uploading part ", i, " of ", num_chunks, " to GCS")
          gcs_upload(
            file = output_file,
            bucket = gcs_bucket,
            name = paste0(gcs_pre_fpath, "/", basename(output_file)),
            predefinedAcl = "bucketLevel"
          )

          # Clean up memory
          rm(chunk)
          gc()
        }
      }
    } else {
      message("Skipping thai txt generation")
    }
  }

  # Prompt for manual confirmation if needed
  if (to_thai_prompt && to_generate_thai_txt && !to_thai_all_years) {
    response <- tolower(readline(prompt = "Have you run the Thai grouper manually? (y/n): "))
    if (response != "y") {
      stop("Thai Grouper not run yet. Script terminated. Continue on manually if necessary")
    }
    message("Continuing with the script...\n")
  } else {
    message("Thai Grouper is assumed to have been run already. Continuing with the script...\n")
  }

  if (!to_thai_all_years) {
    cat("\rDownloading Grouper results\n")
    flush.console()
    # Define chunk size
    cat("\rReading final\n")
    flush.console()
    result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))

    # str(result)
    result[, caseid := as.character(seq_len(nrow(result)))]
    result_mapping <- result[, .(id_series, caseid)]
    # Define chunk size
    chunk_size <- 5000000
    num_chunks <- ceiling(nrow(result) / chunk_size)

    # Download each part and combine them into thai_result
    thai_result <- list()
    for (i in seq_len(num_chunks)) {
      # Define the remote file name and local path for this part
      remote_file <- paste0(
        gcs_post_fpath,
        "/",
        toupper(
          paste0(
            checkpoint_5_prefix, year_to_load, suffix,
            "part_", i, "_of_", num_chunks
          )
        ),
        "Res.TXT"
      )
      local_file <- here(
        checkpoint_5_path,
        paste0(
          toupper(
            paste0(
              checkpoint_5_prefix, year_to_load, suffix,
              "part_", i, "_of_", num_chunks
            )
          ),
          "Res.TXT"
        )
      )

      # Download the part from GCS
      message("Downloading part ", i, " of ", num_chunks, " from GCS")
      gcs_get_object(
        object_name = remote_file,
        bucket = gcs_bucket,
        saveToDisk = local_file,
        overwrite = TRUE
      )

      # Read the downloaded part and store it in the list
      part_data <- fread(local_file, colClasses = "character")
      thai_result[[i]] <- part_data

      # Clean up memory
      rm(part_data)
      gc()
    }

    # Combine all parts into a single data.table
    thai_result <- rbindlist(thai_result, use.names = FALSE, fill = FALSE)
    cat(paste("nrow thai_result:", nrow(thai_result), "\n"))
    cat(paste("nrow result_mapping:", nrow(result_mapping), "\n"))
    cat(paste("nrow result:", nrow(result), "\n"))
    # Final message
    message("All parts downloaded and combined successfully.\n")
    if (to_debug) print(head(thai_result))
    # Check for duplicates in id_series
    if (any(duplicated(thai_result$caseid))) {
      # Identify duplicates
      duplicate_ids <- thai_result$caseid[duplicated(thai_result$caseid)]

      # Extract rows with duplicate id_series
      duplicate_rows <- thai_result[caseid %in% duplicate_ids, ]

      # Print rows with duplicates
      cat("Rows with duplicate 'id_series':\n")
      print(duplicate_rows)

      # Stop execution
      stop("The 'id_series' column contains duplicates. Execution stopped.")
    }

    thai_result <- merge(
      thai_result,
      result_mapping, # Select only caseid and id_series from result_mapping
      by = "caseid", # Column to join on
      all.x = TRUE,
      all.y = FALSE,
    )
    cat(paste("nrow thai_result:", nrow(thai_result), "\n"))
    cat(paste("nrow result_mapping:", nrow(result_mapping), "\n"))
    cat(paste("nrow result:", nrow(result), "\n"))
    cat("\rRenaming columns\n")
    flush.console()
    thai_result[, row := caseid]
    thai_result[, caseid := id_series]
    thai_result[, id_series := NULL]
    thai_result[, thai_drg := drg]
    thai_result[, thai_rw := rw]
    thai_result[, thai_wtlos := wtlos]
    thai_result[, thai_ot := ot]
    thai_result[, thai_adjrw := adjrw]
    thai_result[, thai_err := err]
    thai_result[, thai_warn := warn]
    thai_result[, thai_los := los]
    thai_result[, drg := NULL]
    thai_result[, drgname := NULL]
    thai_result[, rw := NULL]
    thai_result[, wtlos := NULL]
    thai_result[, ot := NULL]
    thai_result[, adjrw := NULL]
    thai_result[, err := NULL]
    thai_result[, warn := NULL]
    thai_result[, los := NULL]

    # str(thai_result)
  }
}


if (to_thai && !to_thai_all_years) {
  print(nrow(thai_result))
  print(nrow(thai_result[thai_err == "6"]))
}


if (to_python && to_thai && !to_thai_all_years) {
  if (exists("output_dt")) {
    before_merge <- data.table::copy(output_dt)
    # str(before_merge)
    before_merge[, caseid := id_series]

    if (to_debug) print(head(before_merge))
    merged <- merge(before_merge, thai_result, by = "caseid", all.x = TRUE)
    if (to_debug) print(head(merged))
    diff_merged <- merged[!as.character(ifelse(is.na(py_drg), "NA", py_drg)) == as.character(thai_drg)]
    print(nrow(diff_merged))
    fwrite(diff_merged, here(checkpoint_9_path, paste0("checkpoint_9_grouper_differences_", year_to_load, suffix, ".csv")))
  }
}


if (to_python && to_thai && !to_thai_all_years) {
  if (exists("merged")) { # str(merged)
    if (to_debug) fwrite(merged, paste0(year_to_load, "test4.csv"))
  }
}


if (to_thai && !to_thai_all_years) {
  # Please run thai grouper first
  result_after_thai <- data.table::copy(thai_result)
  result_after_thai[, id_series := caseid]
  result_after_thai[, caseid := NULL]
  result_after_thai[, thai_drg := as.character(thai_drg)]
  result_after_thai[, thai_rw := as.numeric(thai_rw)]
  result_after_thai[, thai_wtlos := as.numeric(thai_wtlos)]
  result_after_thai[, thai_ot := as.integer(thai_ot)]
  result_after_thai[, thai_adjrw := as.numeric(thai_adjrw)]
  result_after_thai[, thai_err := as.integer(thai_err)]
  result_after_thai[, thai_warn := as.integer(thai_warn)]
  result_after_thai[, thai_los := as.integer(thai_los)]
  # Reorder the columns in the result data.table to match the schema
  setcolorder(result_after_thai, c(
    "row",
    "id_series",
    "thai_drg",
    "thai_rw",
    "thai_wtlos",
    "thai_ot",
    "thai_adjrw",
    "thai_err",
    "thai_warn",
    "thai_los"
  ))
}


if (to_thai && !to_thai_all_years) {
  # str(result_after_thai)
  print(result_after_thai[grepl("e", id_series)])
  # print(result_after_thai[grepl("e", id_pin)])
  # print(result_after_thai[grepl("e", id_hci)])
}


if (to_thai && !to_thai_all_years) {
  # str(result_after_thai)
}


if (to_thai && !to_thai_all_years) {
  # Check for duplicates in id_series
  if (any(duplicated(result_after_thai$id_series))) {
    # Identify duplicates
    duplicate_ids <- result_after_thai$id_series[duplicated(result_after_thai$id_series)]

    # Extract rows with duplicate id_series
    duplicate_rows <- result_after_thai[id_series %in% duplicate_ids, ]

    # Print rows with duplicates
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)

    # Stop execution
    stop("The 'id_series' column contains duplicates. Execution stopped.")
  }
}


print(nrow(result))
if (to_thai && !to_thai_all_years) print(nrow(result_after_thai))


if (to_thai && !to_thai_all_years) print(result_after_thai[is.na(thai_drg)])
if (to_thai && !to_thai_all_years) print(result_after_thai[is.na(id_series)])


if (to_thai && !to_thai_all_years && to_thai_bq) {
  result_after_thai[, row := NULL]
  saveRDS(result_after_thai, here(checkpoint_6_path, paste0(checkpoint_6_prefix, year_to_load, suffix, ".rds")))

  # message("You may now run drg-spc-v2.ipynb in the background")

  # Set the table name based on row count and sample status
  prefix <- if (!to_sample) "thai_" else "temp_thai_"
  bq_table <- paste0(prefix, year_to_load)

  # Only proceed with BigQuery upload if `to_spc` is FALSE
  if (!to_spc) {
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
            "bq_schema_thai.json"
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
      num_chunks <- ceiling(nrow(result_after_thai) / chunk_size)

      for (i in seq_len(num_chunks)) {
        cat(paste("\rUploading chunk no.:", i))
        flush.console()
        chunk <- result_after_thai[
          ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result_after_thai)),
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
  } else {
    message("Skipping BigQuery upload as to_spc is TRUE.")
  }
}


# paths <- list(
#   input_notebook = here::here("data-cleaning", "02-drg-grouping-v2.ipynb"),
#   output_rscript = here::here("data-cleaning", "debug", "drg-grouping")
# )

# system(paste(
#   "jupyter nbconvert --no-prompt --to script",
#   paths$input_notebook, "--output", paths$output_rscript
# ))

