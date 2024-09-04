# Delete all R objects and run garbage collection so we start with a clean slate
rm(list = ls())
gc()


# Load libraries and minor parameters
source(here::here("data-cleaning/r_scripts", "00_libraries-params.R"))


# Python packages to install
pkgs <- c("numpy", "pandas", "streamlit", "python_dateutil", "tabulate", "swifter", "rpy2")
# Install python packages for reticulate
for (pkg in pkgs) py_install(pkg)


# Prompt Options:
to_prompt <- FALSE # Whether to prompt for user inputs or not (if FALSE, default values in this cell will be used)
thai_prompt <- TRUE # Whether to prompt for thai grouper even if bypassing all other prompts

# IMPORTANT PARAMETERS:
full_claims_prefix <- "claims_extract_CLAIMS " # Include spaces if there are any
year_to_load <- "2018" # Which claims year to load # TODO: maybe add a script that loops through all claims?
gcs_email <- "271591364028-compute@developer.gserviceaccount.com" # Service Account to use
gcp_proj <- system("gcloud config get-value project", intern = TRUE) # get current GCP Project
gcs_bucket <- "phic-claims-checkpoints" # Name of GCS bucket
gcs_pre_fpath <- "pre-tdrg" # Name of folder path prefix in GCS bucket for thai grouper input
gcs_post_fpath <- "post-tdrg" # Name of folder path prefix in GCS bucket for thai grouper output
bq_dataset <- "phic" # bq dataset
bq_table <- "temp_claims_latest" # temp bq table, later renamed to claims_20XX1231 in Push to BQ section

# Input:
to_sample <- TRUE # Whether to sample each split_part by sample_size_divisor (useful when iterating through code runs in quick succession)
sample_size_divisor <- 25 # Sample size divisor: Formula for sample size is total_rows / split_parts / sample_size_divisor. Choose between 5, 25, 125, and 625

# Output:
to_write <- TRUE # Whether to write out checkpoint_1 files (everything up until converting for grouper export)
to_combine <- TRUE # Whether to combine checkpoint 1 files into one data.table
to_group <- TRUE # Whether to export for the batch grouper or not
to_gcs <- TRUE # Whether to push to GCS or nt (Thai Grouper Input/Output)
to_bq <- FALSE # Whether to push to BQ or not
to_drop_bq <- FALSE # Whether to drop the existing bq table and recreate it

# Manual Tweaks:
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b") # ICD codes to replace
manual_code_replacements <- c("O800", "O80", "O809") # ICD code replacements
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 13:14), # Start
  "ICCODED15", # note that ICDCODE15 is misspelled as ICCODED15 in all claims
  paste0("ICDCODE", 16:170), # Continuation
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)

# Flush files
to_flush_master <- FALSE # whether to flush aux-files, checkpoints, profvis, debug, cache, and samples
to_flush_partial <- FALSE # whether to flush partial files (raw files but split into split_parts parts)

# Control random behavior for reproducibility
global_seed <- seed <- 123 # Choose a number as seed
set.seed(seed) # Setting the seed reproducibility (Important for stuff like randomly choosing a pdx among multiple possible options)

# Machine Specifications
ram_size <- 32 # Input virtual or physical machine's RAM size here

# Print GCP project
cat(paste("GCP Project:", gcp_proj, "\n"))


# Debug Parameters:
to_debug <- FALSE # whether to print debug statements
to_profvis <- FALSE # Conduct runtime duration analysis via profvis or not
to_view_checks <- TRUE # Whether to view checks and print statements
to_view_checks_parallel <- FALSE # Whether to view intermediate per split_part/chunk checks and print statements (not consolidated) when parallelized
to_parallel <- TRUE # Whether to parallelize each split_parts split_part into availableCores() chunks. Cuts down processing time from 120min to 15min.
to_split_read <- FALSE # WARNING: TRUE uses a lot of memory!!
to_dec_mem_usage <- FALSE # Whether to run rm() and gc() at every possible step
tmp_nrow <- Inf # Per split_part/chunk end_nrow (leave at Inf)
diff_chars <- 0
split_parts <- 15 # How many (integer) parts to split the 12+m row claims file into # TODO: a value of 10 for claims year 2018 leads to quoted newline errors
end_nrow <- 10 # How many rows/entries to show in summary tables
max_bq_rows <- 15000 # Max rows to return for bq query
encode <- "unknown" # Choices: unknown, UTF-8, Latin-1
sep <- "," # Choices: "," or "\t"
is_unix <- if (.Platform$OS.type == "unix") TRUE else FALSE # Detect operating system architecture

ram_buffer <- 0.1 # How much of a RAM buffer to leave for the OS


# Folder Path Prefixes:
clean_prefix <- "data-cleaning"
data_prefix <- file.path(clean_prefix, "data")
claims_prefix <- file.path(data_prefix, "claims")
checkpoint_1_prefix <- "checkpoint_1_claims_"
checkpoint_2_prefix <- "checkpoint_2_claims_"
checkpoint_3_prefix <- "DRG_Grouped_"
checkpoint_4_prefix <- "checkpoint_4_thai_grouper_input_"
checkpoint_5_prefix <- toupper(paste0(gcs_pre_fpath, "_", checkpoint_4_prefix))
checkpoint_6_prefix <- "checkpoint_6_grouped_claims"
checkpoint_7a_prefix <- "python_input_1"
checkpoint_7b_prefix <- "python_input_2"

# Folder Paths:
chkpt_path <- file.path(data_prefix, "checkpoints")
checkpoint_1_path <- file.path(chkpt_path, "checkpoint_1_partial_clean_claims")
checkpoint_2_path <- file.path(chkpt_path, "checkpoint_2_master_clean_claims")
checkpoint_3_path <- file.path(chkpt_path, "checkpoint_3_thai_partial_input")
checkpoint_4_path <- file.path(chkpt_path, "checkpoint_4_thai_master_input")
checkpoint_5_path <- file.path(chkpt_path, "checkpoint_5_thai_output")
checkpoint_6_path <- file.path(chkpt_path, "checkpoint_6_thai_merged")
checkpoint_7_path <- file.path(chkpt_path, "checkpoint_7_py_input")
checkpoint_8_path <- file.path(chkpt_path, "checkpoint_8_py_output")
cache_path <- file.path(clean_prefix, "cache")
aux_path <- file.path(data_prefix, "aux-files")
raw_claims_path <- file.path(claims_prefix, "raw")
raw_claims_parts_path <- file.path(claims_prefix, "raw", "parts")
raw_claims_samples_path <- file.path(claims_prefix, "raw", "samples")
profvis_path <- file.path(data_prefix, "profvis")
debug_path <- file.path("data-cleaning", "debug")

# File Paths
profvis_fpath <- here("data-cleaning", "data", "profvis", "profvis.html")

# Create directories:
created_dirs <- c() # Initialize empty vector
# For all "_path" variables, create a directory with that path
# Excludes "_fpath" variables
for (path in mget(ls(pattern = "_path$"), envir = .GlobalEnv)) {
  full_path <- here(path)
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    created_dirs <- c(created_dirs, full_path)
  }
}

# Print directories created if any
if (length(created_dirs) == 0) {
  cat("All directories exist.\n")
} else {
  cat("The following directories were created:\n")
  cat(paste(created_dirs, collapse = ",\n"), "\n")
}

# Commonly Used File Paths:
full_claims_file <- here(
  raw_claims_path,
  paste0(full_claims_prefix, year_to_load, ".csv")
)


# Stop if forecasted memory usage is expected to crash the system
if (!split_parts == as.integer(split_parts) || split_parts <= 1) stop("ERROR: split_parts must be an integer greater than or equal to 2!")
if (ram_size <= 64 && split_parts <= 2) stop("Please set split_parts to at least 3 for 64 GB machines or it will likely crash")
if (ram_size <= 32 && split_parts <= 4) stop("Please set split_parts to at least 5 for 32 GB machines or it will likely crash")
if (ram_size <= 32 && to_split_read == TRUE) stop("Please set to_split_read to TRUE for 32 GB machines or it will likely crash")

# Function to prompt for input with default value
prompt_with_default <- function(prompt_text, default_value) {
  if (to_prompt) {
    user_input <- readline(prompt = paste0(prompt_text, " [Default: ", default_value, "]: "))
    if (user_input == "") {
      return(default_value)
    } else {
      return(user_input)
    }
  } else {
    message(
      paste0(
        "Using default value (",
        default_value, ") for ",
        deparse(substitute(default_value))
      )
    )
    return(default_value)
  }
}

# Set parameters based on prompts or defaults
full_claims_prefix <- prompt_with_default("Enter full_claims_prefix", full_claims_prefix)
full_claims_bq_prefix <- str_replace_all(full_claims_prefix, " ", "\\\\ ")
year_to_load <- prompt_with_default("Enter year_to_load", year_to_load)

# Prompt for whether to sample
to_sample <- as.logical(prompt_with_default("Sample data? (TRUE/FALSE)", to_sample))
sample_size_divisor <- as.integer(prompt_with_default("Enter sample_size_divisor", sample_size_divisor))

# Prompt for output options
to_write <- as.logical(prompt_with_default("Write output files? (TRUE/FALSE)", to_write))
to_combine <- as.logical(prompt_with_default("Combine files? (TRUE/FALSE)", to_combine))
to_group <- as.logical(prompt_with_default("Export for batch grouper? (TRUE/FALSE)", to_group))

# Flush options
to_flush_master <- as.logical(prompt_with_default("Flush master files? (TRUE/FALSE)", to_flush_master))
to_flush_partial <- as.logical(prompt_with_default("Flush partial files? (TRUE/FALSE)", to_flush_partial))

# RAM settings
ram_size <- as.numeric(prompt_with_default("Enter RAM size (GB)", ram_size))
# ram_buffer <- as.numeric(prompt_with_default("Enter RAM buffer (0.0-1.0)", ram_buffer))
ram_limit <- (1 - ram_buffer) * ram_size * (1024^3)

# Allowing each future_lapply session to use more memory
options(future.globals.maxSize = ram_limit)

# Compute the RAM limit for R processes, leaving the buffer for the OS
ram_limit_gb <- round((1 - ram_buffer) * ram_size, 0)

# Print the set RAM limit
cat(sprintf("Setting future.globals.maxSize to: %.1f GB", ram_limit / (1024^3)))


# Hide verbose outputs and warnings
options(verbose = FALSE) # Hide verbose output for script and library loading
options(warn = -1) # Hide warnings for script sourcing and library loading


scripts <- list( # List of scripts to source
  lib_params = "00_libraries-params.R",
  cleaning = "01_cleaning-functions.R",
  clinical = "02_clinical-functions.R",
  timing_debug = "03_timing-debug-functions.R",
  summary = "04_summary-functions.R",
  io = "05_io-functions.R"
)

# Loop to source above scripts
for (script in scripts) source(here("data-cleaning/r_scripts", script))


# TODO: figure out a way to return to default outputs
# since verbose = TRUE is way too verbose compared to default
options(warn = 1) # Reenable warnings; see above comments


# Load raw claims from GCS only if they don't exist on the VM yet
for (year in 2018:2021) {
  file_name <- paste0(full_claims_prefix, year, ".csv")
  bq_name <- paste0(full_claims_bq_prefix, year, ".csv")

  # Check if the file exists in the target directory
  file_path <- here(raw_claims_path, file_name)
  exists <- file.exists(file_path)

  # If the file does not exist, run the gsutil cp command
  if (!exists) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(paste0("cd .. && gsutil cp gs://phic-claims-raw/", bq_name, " ", raw_claims_path),
        intern = FALSE, ignore.stderr = FALSE
      )
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  } else {
    cat(paste("File", file_name, "already exists in the target directory. Skipping download.\n"))
  }
}


# Function to query BigQuery and optionally cache the results as a CSV
query_bq_to_dt <- function(query, file_path, cache, max_bq_rows) {
  if (file.exists(file_path)) {
    message(paste(basename(file_path), "already exists, loading from CSV"))
    dt <- fread(file_path)
  } else {
    message(paste("Querying BigQuery for", basename(file_path)))
    dt <- as.data.table(bq_table_download(
      bq_project_query(gcp_proj, query),
      n_max = max_bq_rows
    ))
    if (cache) {
      fwrite(dt, file_path)
    }
  }
  # return bq table as dt
  return(dt)
}


# 1. Query and load `grouper_v5.proc`
proc_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".grouper_v5.proc` LIMIT ", max_bq_rows
)
proc <- query_bq_to_dt(proc_query, here(aux_path, "proc.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)
proc[, CODE := as.character(CODE)]

# 2. Query and load `phic.acr_rvs_map`
rvs_icd9_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".phic.acr_rvs_map` LIMIT ", max_bq_rows
)
rvs_icd9 <- query_bq_to_dt(rvs_icd9_query, here(aux_path, "rvs_icd9cm.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)

# Convert rvs to character and handle icd9cm conversion carefully
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]

# Merge with proc to classify by DRGUSE
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)],
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)

# Remove DRGUSE and filter out NAs
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) &
  DRGUSE][!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

# 3. Query and load `phic.acr_procedure`
acr_rvs_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".phic.acr_procedure` LIMIT ", max_bq_rows
)
acr_rvs <- query_bq_to_dt(acr_rvs_query, here(aux_path, "acr_rvs.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)

# 4. Query and load `grouper_v5.i10`
i10_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".grouper_v5.i10` LIMIT ", max_bq_rows
)
tdrg_icd10 <- query_bq_to_dt(i10_query, here(aux_path, "i10.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)
setkey(tdrg_icd10, "CODE")

# Subset and assign to acc_pdx
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])

# 5. Query and load `icd.phl_icd10`
phl_icd10_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".icd.phl_icd10` LIMIT ", max_bq_rows
)
phl_icd10 <- query_bq_to_dt(phl_icd10_query, here(aux_path, "phl_icd10.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)

# Filter and process neoplasms
neoplasms_dt <- as.data.table(phl_icd10[
  grepl("/", icd10),
  .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])


# Load cached total rows file if available, saves ~10 seconds of runtime
total_rows_file <- here(cache_path, paste0("total_rows_", year_to_load, ".rds"))
if (file.exists(total_rows_file)) {
  total_rows <- readRDS(total_rows_file)
  cat(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- fread(file = full_claims_file, select = 1L, header = TRUE, colClasses = "character")[, .N]
  saveRDS(total_rows, file = total_rows_file)
  cat(paste("Total Rows via fread:", total_rows))
}

# Compute sample size when splitting and when not,
# only relevant when sampling
if (to_split) {
  sample_size <- ceiling(total_rows / split_parts / sample_size_divisor)
} else {
  sample_size <- ceiling(total_rows / sample_size_divisor)
}

# suffix appended to files to indicate if they are from sampled or full runs
suffix <- paste0(
  ifelse(to_sample, paste0("_sampled_", sample_size, "_"), "_full_")
)


# Define main clean data function, which does majority of the data cleaning on the claims file
clean_data <- function(dt) {
  # Convert source year to integer
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns and check if renaming was successful
  setnames(dt, old = old_colnames, new = new_colnames)
  rename_success <- all(new_colnames %in% colnames(dt))

  # Collapse and clean ICD and RVS columns
  dt <- collapse_and_clean_icd_rvs(dt)

  # Helper function to clean and compare clinical columns
  clean_clinical_column <- function(col_name) {
    dt[, (paste0(col_name, "_orig")) := dt[[col_name]]]
    dt[, (col_name) := clean_column(
      dt[[col_name]], na_like_strings, neoplasms_dt
    )]
    dt[, (paste0(col_name, "_orig")) := sapply(
      get(paste0(col_name, "_orig")), toString
    )]
    dt[, (col_name) := sapply(get(col_name), toString)]

    # Compare cleaning results
    dt[
      !is.na(get(paste0(col_name, "_orig"))) & get(col_name) !=
        get(paste0(col_name, "_orig")),
      .(
        old_code = get(paste0(col_name, "_orig")),
        new_code = get(col_name), count = .N
      ),
      by = .(get(paste0(col_name, "_orig")), get(col_name))
    ]
  }

  # Clean and compare clin_c1 and clin_c2 columns
  clin_c1_cleaning_comparison <- clean_clinical_column("clin_c1")
  clin_c2_cleaning_comparison <- clean_clinical_column("clin_c2")

  # Function to replace multiple patterns with corresponding replacements
  replace_multiple_patterns <- function(text, patterns, replacements) {
    # Ensure patterns and replacements are the same length
    if (length(patterns) != length(replacements)) {
      stop("Patterns and replacements must have the same length.")
    }

    # Perform replacements
    modified_text <- stri_replace_all_regex(
      text,
      pattern = patterns,
      replacement = replacements,
      vectorize_all = FALSE # Apply all replacements simultaneously
    )

    # return the text after modification
    return(modified_text)
  }

  # Apply multi-replacement function
  dt[, clin_icd := lapply(clin_icd,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, clin_c1 := lapply(clin_c1,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, clin_c2 := lapply(clin_c2,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]

  # Remove lumped ICD codes
  dt[, clin_c1 := remove_lumped_icd_codes(clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(clin_c2)]

  # Clean clinical columns
  clean_clin_col_res <- clean_clinical_columns(dt)
  dt <- clean_clin_col_res$dt

  # Replace empty strings with NA and remap patient data
  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  empty_strings_replaced_1 <- replace_result$return_replacement_summary

  remapping_results <- remap_patient_data(dt, to_view_checks)
  dt <- remapping_results$data

  return_summary_list <- list(
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_mapped = remapping_results$pat_type_mapped,
    pat_memcat_parent_mapped = remapping_results$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remapping_results$pat_memcat_child_mapped,
    clin_discharge_mapped = remapping_results$clin_discharge_mapped,
    claim_status_mapped = remapping_results$claim_status_mapped,
    pat_type_unmapped = remapping_results$pat_type_unmapped,
    memcat_parent_unmapped = remapping_results$memcat_parent_unmapped,
    memcat_child_unmapped = remapping_results$memcat_child_unmapped,
    discharge_unmapped = remapping_results$discharge_unmapped,
    claim_status_unmapped = remapping_results$claim_status_unmapped,
    discard_rvs_one = clean_clin_col_res$discard_rvs_one,
    discard_rvs_two = clean_clin_col_res$discard_rvs_two,
    empty_strings_replaced_1 = empty_strings_replaced_1
  )

  return(
    list(
      # data to return for further processing
      return_data = dt,
      # summary to return for checks and output
      return_summary = return_summary_list
    )
  )
}


# Define function to map RVS codes to ICD9 codes
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)

  return(
    list(
      # main return variable (a column) to save back to dt
      icd9_list = get_icd9_codes(clin_rvs, rvs_map_solo_env),
      # other return variables that are for checks and outputs
      rvs_map_list = rvs_maps$rvs_map_list,
      rvss = unique(unlist(clin_rvs)),
      mappable_rvs = intersect(unique(unlist(clin_rvs)), rvs_icd9$rvs),
      unmappable_rvs = setdiff(unique(unlist(clin_rvs)), rvs_icd9$rvs),
      multi_mapped_rvs = intersect(unique(unlist(clin_rvs)), names(rvs_maps$rvs_map_list)),
      without_drg = unique(rvs_icd9[!rvs %in% names(rvs_maps$rvs_map_list)]$rvs)
    )
  )
}


# Define function to apply icd10 mapping to the dt
implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10) {
  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  direct_match_codes <- find_direct_icd_matches(
    icds, thai_icd10_env
  )

  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env
  )
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count

  unmatched_icds <- setdiff(icds, names(icd_mapping))

  if (length(unmatched_icds) > 0) {
    unmatched_sources <- data.table(
      code = unmatched_icds, source = NA_character_, count = 0
    )
    for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[
        code %in% unlist(col_values),
        source := col_name
      ]
      unmatched_sources[
        code %in% unlist(col_values),
        count := count + table(unlist(col_values))[code]
      ]
    }
    unmatched_sources <- unmatched_sources[order(-count)]
  } else {
    unmatched_sources <- data.table()
  }

  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    tdrg_icd10 = unlist(icd_mapping)
  )
  if (to_debug) {
    fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  }
  icd10_env <- list2env(
    setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10)
  )

  mapped_columns <- apply_icd10_mapping_to_columns(
    clin_c1, clin_c2, clin_icd, icd10_env
  )
  return(
    list(
      # main return variables to be saved as columns in the dt
      clin_c1 = mapped_columns$clin_c1,
      clin_c2 = mapped_columns$clin_c2,
      clin_icd = mapped_columns$clin_icd,
      # return variables for checks and outputs
      icd10_map_dt = icd10_map,
      unique_icds = icds,
      direct_matches = direct_match_codes,
      unmatched = unmatched_icds,
      unmatched_sources = unmatched_sources
    )
  )
}


##################################################################################################################################
################################################### START OF PROCESS CHUNK #######################################################
##################################################################################################################################

# Define process_chunk (not to be confused with process_part) that processes each part in nthreads chunks
process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  if (to_view_checks) {
    # cat("\rViewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  clean_result <- clean_data(chunk)
  chunk <- clean_result$return_data

  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- implement_icd10_mapping(
    clin_c1, clin_c2, clin_icd, tdrg_icd10
  )
  chunk[, clin_c1 := icd10_mapping_result$clin_c1]
  chunk[, clin_c2 := icd10_mapping_result$clin_c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  res2 <- replace_empty_with_na(dt = chunk, to_view_checks)
  chunk <- res2$return_data
  empty_strings_replaced_2 <- res2$return_replacement_summary

  pdx_result <- apply_find_pdx(
    chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Ensure consistent lengths of clin_rvs and icd9_list
  clin_rvs_len <- lengths(chunk$clin_rvs)
  icd9_list_len <- lengths(chunk$icd9_list)

  max_len <- max(c(clin_rvs_len, icd9_list_len))
  chunk$clin_rvs <- lapply(chunk$clin_rvs, function(x) {
    length(x) <- max_len
    # return x with a new max length
    return(x)
  })
  chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
    length(x) <- max_len
    # return x with a new max length
    return(x)
  })

  chunk_summary <- modifyList(
    clean_result$return_summary,
    list(
      unique_icds = icd10_mapping_result$unique_icds,
      direct_matches = icd10_mapping_result$direct_matches,
      unmatched = icd10_mapping_result$unmatched,
      unmatched_sources = icd10_mapping_result$unmatched_sources,
      icd10_map_dt = icd10_mapping_result$icd10_map_dt,
      rvss = rvs_mapping_result$rvss,
      mappable_rvs = rvs_mapping_result$mappable_rvs,
      unmappable_rvs = rvs_mapping_result$unmappable_rvs,
      multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
      without_drg = rvs_mapping_result$without_drg
    )
  )

  if (to_dec_mem_usage) gc() # debug

  return(
    list(
      # chunk data to return
      return_chunk = chunk,
      # return summary for checks and outputs
      return_summary = chunk_summary
    )
  )
}

##################################################################################################################################
#################################################### END OF PROCESS CHUNK ########################################################
##################################################################################################################################


main_logic_func <- function() {
  # Start main execution logic
  ####################################################################################################################################
  ################################################## START OF SPLIT AND SAVE PART ####################################################
  ####################################################################################################################################

  full_header <<- fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

  # Define Split and Save function
  split_and_save <- function(split_and_save_part) {
    rows_per_part <- ceiling(total_rows / split_parts)
    chunk_file <- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", split_and_save_part),
      "_of_", split_parts, ".csv"
    ))
    if (!file.exists(chunk_file)) {
      start_row <- (split_and_save_part - 1) * rows_per_part + 1
      end_row <- min(split_and_save_part * rows_per_part, total_rows)
      chunk_dt <- fread(
        file = full_claims_file,
        skip = start_row,
        nrows = end_row - start_row + 1,
        na.strings = na_values,
        colClasses = "character",
        header = FALSE,
        encoding = encode,
        sep = sep
      )
      setnames(chunk_dt, colnames(full_header))
      if (to_debug) print(head(chunk_dt), 2) # debug
      fwrite(chunk_dt, chunk_file)
      if (to_dec_mem_usage) rm(chunk_dt)
      if (to_dec_mem_usage) gc()
      split_processing_times[[split_loop_part]] <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      ) # Save partial processing time to a list
      # Print status update and ETA
      print_status_update(split_loop_part, split_parts, split_processing_times, "split")
    }
  }

  start_time <<- Sys.time()
  for (split_loop_part in 1:split_parts) split_and_save(split_loop_part)

  ####################################################################################################################################
  ################################################## END OF SPLIT AND SAVE PART ######################################################
  ####################################################################################################################################

  if (to_parallel && !is_unix) plan(multisession, workers = nthreads) # Start the parallelization session or remain sequential

  for (loop_part in 1:split_parts) { # For each partial file (loop_part) of 1:N (split_parts) files
    # Process the partial file with or without parallelization

    ##################################################################################################################################
    ################################################### START OF PROCESS PART ########################################################
    ##################################################################################################################################

    start_time <- Sys.time()

    partial_claims_file <<- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".csv"
    ))
    ensure_partial_files_exist(loop_part) # Ensure partial exist

    if (to_sample) {
      sampled_claims_file <<- here(raw_claims_samples_path, paste0(
        "sampled_claims_", year_to_load, "_", sample_size,
        "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".csv"
      ))
      ensure_sample_files_exist(loop_part) # Ensure sample files exist
    }
    read_result <- read_appropriate_file(loop_part, to_sample) # Read the appropriate file
    read_in_dt <- read_result$read_result_dt
    read_in_replacement_summary <- read_result$read_result_replacement_summary

    ##################################################################################################################################
    ############################################ START OF PARALLELIZE AND SUMMARIZE DATA #############################################
    ##################################################################################################################################

    chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
    chunks <- split(read_in_dt, rep(1:nthreads, each = chunk_size, length.out = nrow(read_in_dt)))

    if (to_parallel && is_unix) {
      if (to_debug) message("Conducting mclapply")
      parallel_results <- mclapply(
        chunks, process_chunk,
        mc.cores = nthreads,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    } else if (to_parallel && !is_unix) {
      if (to_debug) message("Conducting future_lapply")
      parallel_results <- future_lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx,
        future.seed = global_seed
      )
    } else {
      if (to_debug) message("Conducting lapply")
      parallel_results <- lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    }

    parallel_summaries <- lapply(
      parallel_results,
      function(res) res$return_summary
    )
    rbound_dt <- rbindlist(lapply(
      parallel_results,
      function(res) res$return_chunk
    ))

    if (to_dec_mem_usage) rm(processed_chunks) # debug

    combined_chunk_summary <- combine_chunk_summaries(
      parallel_summaries, tmp_nrow
    )

    if (to_dec_mem_usage) rm(parallel_results) # debug

    acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
    for (code in acc_pdx) {
      assign(code, TRUE, envir = acc_pdx_env)
    }

    invalid_pdx_indices <- which(
      !is.na(rbound_dt$pdx) & rbound_dt$pdx != "" &
        !sapply(rbound_dt$pdx, function(x) exists(x, acc_pdx_env))
    )

    if (length(invalid_pdx_indices) > 0) {
      cat(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
      combined_chunk_summary$pdx_success <- FALSE
    } else {
      combined_chunk_summary$pdx_success <- TRUE
    }

    if (to_dec_mem_usage) gc() # debug

    ##################################################################################################################################
    ############################################## END OF PARALLELIZE AND SUMMARIZE DATA #############################################
    ##################################################################################################################################

    summarized_dt <- rbound_dt
    combined_parallel_summary <- combined_chunk_summary
    combined_parallel_summary$replacement_summary <- read_in_replacement_summary

    if (to_write) {
      fwrite(
        summarized_dt, here(checkpoint_1_path, paste0(
          checkpoint_1_prefix, year_to_load, suffix,
          "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".csv"
        ))
      )
    }
    if (to_combine) master_dt_list[[loop_part]] <- summarized_dt
    if (to_group) {
      # export_for_grouper(
      #   summarized_dt, year_to_load,
      #   here(checkpoint_3_path, paste0(
      #     checkpoint_3_prefix, year_to_load, suffix, "part_",
      #     sprintf("%02d", loop_part), "_of_", split_parts, ".txt"
      #   )), loop_part
      # )
    }

    ##################################################################################################################################
    ####################################################### END OF PROCESS PART ######################################################
    ##################################################################################################################################

    all_parts_summaries[[loop_part]] <- combined_parallel_summary
    # Save partial summaries to a list
    processing_times[[loop_part]] <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    ) # Save partial processing time to a list
    # Print status update and ETA
    print_status_update(loop_part, split_parts, processing_times, "clean")
    if (loop_part == 1) dim_dt <<- dim(summarized_dt)
    nrow_end[[loop_part]] <<- nrow(summarized_dt)

    if (to_dec_mem_usage) {
      rm(read_in_dt, rbound_dt, summarized_dt)
      gc()
    }
  }

  for (nrow_part in 1:split_parts) {
    if (nrow_start[[nrow_part]] != nrow_end[[nrow_part]]) {
      warning(
        "WARNING: Row Count Mismatch! Part ", nrow_part,
        " has ", nrow_start[[nrow_part]], " starting rows and ",
        nrow_end[[nrow_part]], " ending rows\n"
      )
      stop("ERROR: Row Count Mismatch")
    }
  }

  cat("\nRow Counts Match for All Parts\n") # only prints if above succeeds

  if (to_combine) {
    master_dt <<- rbindlist(master_dt_list)
    # if (to_group) master_grouper_input_dt <- rbindlist(master_grouper_input_list)
    # if (to_group) master_grouper_input_dt[, CASEID := 1:nrow(master_grouper_input_dt)]
    # master_dt[is.na(master_dt)] <- ""
    list_cols <- names(master_dt)[sapply(master_dt, is.list)]
    for (col in list_cols) {
      master_dt[, (col) := sapply(.SD[[col]], function(x) {
        # Remove "NA" from the list and collapse to a string
        cleaned <- paste(na.omit(x), collapse = "||")
        # If the cleaned string is empty, return NA_character_,
        # otherwise return the cleaned string
        if (cleaned == "") NA_character_ else cleaned
      }), .SDcols = col]
    }
    # print(head(master_dt))
    if (to_dec_mem_usage) {
      if (to_group) rm(master_dt_list, master_grouper_input_list) else rm(master_dt_list)
      gc()
    }
    if (to_write) {
      fwrite(master_dt, here(checkpoint_2_path, paste0(
        checkpoint_2_prefix, year_to_load, suffix, ".csv"
      )))
      # fwrite(master_grouper_input_dt,
      #   here(checkpoint_4_path, paste0(
      #     checkpoint_4_prefix,
      #     year_to_load, suffix, ".txt"
      #   )),
      #   sep = "|", col.names = TRUE, quote = FALSE # quotes don't work with thai grouper
      # )
    }
  }

  # Summaries are consolidated from 15 split_parts * 8 chunks = 120 sub outputs
  print_summary_tables( # Print final summaries
    combine_parts_summaries(all_parts_summaries, tmp_nrow),
    end_nrow
  )

  if (to_parallel && !is_unix) plan(sequential) # end parallelization

  # End main execution logic
  if (to_debug) {
    # if we need anything to return
    return(master_dt)
  } # debug
}


# Initialize Variables
all_parts_summaries <- master_dt_list <- list() # initialize lists
# if (to_group) master_grouper_input_list <- list()
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table() # initialize data.tables
# if (to_group) master_grouper_input_dt <- data.table() # initialize data.tables
nthreads <- parallelly::availableCores() # detect available threads
cat(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))

# Call the main function with or without profvis
if (to_profvis) {
  saveWidget(profvis({
    main_logic_func()
  }), profvis_fpath)
} else {
  main_logic_func()
}


# Please run thai grouper first
result <- fread(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, ".csv")), colClasses = "character")

# Convert data types to match BigQuery schema
result[, id_series := as.character(id_series)]
result[, id_pin := as.character(id_pin)]
result[, date_adm := as.Date(date_adm, format = "%m/%d/%Y")]
result[, time_adm := as.ITime(time_adm)]
result[, date_dis := as.Date(date_dis, format = "%m/%d/%Y")]
result[, time_dis := as.ITime(time_dis)]
result[, date_rec := as.Date(date_rec, format = "%m/%d/%Y")]
result[, date_ref := as.Date(date_ref, format = "%m/%d/%Y")]
result[, date_check := as.Date(date_check, format = "%m/%d/%Y")]
result[, id_hci := as.character(id_hci)]

# Convert character "0"/"1" to logical for Boolean fields
result[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
result[, clin_emergency := as.logical(as.integer(clin_emergency))]

result[, pat_type := as.character(pat_type)]
result[, clin_acc := as.character(clin_acc)]
result[, pat_rel := as.character(pat_rel)]
result[, pat_bdate := as.Date(pat_bdate, format = "%m/%d/%Y")]
result[, pat_age := as.numeric(pat_age)]
result[, pat_age_orig := as.numeric(pat_age)]
result[, pat_sex := as.character(pat_sex)]
result[, pat_bwt := as.numeric(pat_bwt)]
result[, pat_memcat_parent := as.character(pat_memcat_parent)]
result[, pat_memcat_child := as.character(pat_memcat_child)]
result[, clin_discharge := as.integer(clin_discharge)]

result[, claim_status := as.character(claim_status)]
result[, claim_payout := as.numeric(claim_payout)]
result[, claim_charge := as.numeric(claim_charge)]
result[, date_ext := as.Date(date_ext, format = "%m/%d/%Y")]
result[, id_year := as.integer(id_year)]

result[, clin_c1_orig := as.character(clin_c1_orig)]
result[, clin_c2_orig := as.character(clin_c2_orig)]

result[, pdx := as.character(pdx)]
result[, pdx_code := as.integer(pdx_code)]

result[, `:=`(
  clin_c1 = ifelse(clin_c1 == pdx, NA_character_, clin_c1),
  clin_c2 = ifelse(clin_c2 == pdx, NA_character_, clin_c2),
  clin_icd = mapply(function(pdx_var, sdx_var) sdx_var[sdx_var != pdx_var], pdx, clin_icd, SIMPLIFY = FALSE)
)]

fwrite(result, here(checkpoint_6_path, paste0(checkpoint_6_prefix, ".csv")))

# Step 1: Read in the data from the CSV file
# result_dt <- fread(here(checkpoint_6_path, paste0(checkpoint_6_prefix, ".csv")), colClasses = "character")

result_dt <- data.table::copy(result)

# # Convert columns to Date objects, ignoring NA values
date_columns <- names(result_dt)[grepl("date", names(result_dt))]
# result_dt[, pat_bdate_orig := pat_bdate]
# result_dt[, (date_columns) := lapply(.SD, as.Date, format = "%Y-%m-%d"), .SDcols = date_columns]

# Create a data.table for rows with negative `pat_age`
negative_age_dt <- result_dt[pat_age < 0, ]

# Create a data.table for rows with non-negative `pat_age`
positive_age_dt <- result_dt[pat_age >= 0, ]

# Process rows with non-negative `pat_age`
positive_age_dt[, pat_bdate := dmy(generate_dob(format(pat_bdate, "%Y-%m-%d"), pat_age, format(date_adm, "%Y-%m-%d")))]

# For negative `pat_age`, keep `pat_bdate` as it is
# negative_age_dt[, pat_bdate := pat_bdate_orig]

# Combine the processed data back together
result_dt <- rbind(positive_age_dt, negative_age_dt)

# Calculate 'pat_age' only for rows where 'pat_bdate' is not missing
result_dt[!is.na(pat_bdate), pat_age := floor(as.numeric(interval(pat_bdate, date_adm) / years(1)))]

# Function to split a list column by '||' and ensure a fixed number of columns
split_codes <- function(dt, column, prefix, max_cols) {
  # Apply strsplit to each element in the list column
  split_list <- lapply(dt[[column]], function(x) unlist(strsplit(x, "\\|\\|")))
  # Ensure that each list element has exactly max_cols elements
  split_cols <- lapply(1:max_cols, function(i) sapply(split_list, function(x) if (length(x) >= i) x[[i]] else NA_character_))
  # Convert to data.table
  split_dt <- as.data.table(split_cols)
  # Name the columns appropriately
  setnames(split_dt, paste0(prefix, 1:max_cols))
  return(split_dt)
}

# Apply the function to clin_icd and icd9_list
sdx_columns <- split_codes(result_dt, "clin_icd", "sdx", 12)
proc_columns <- split_codes(result_dt, "icd9_list", "proc", 20)

proc_columns[, (names(proc_columns)) := lapply(.SD, as.character)]
sdx_columns[, (names(sdx_columns)) := lapply(.SD, as.character)]

# Combine the split columns back into result_dt
result_dt <- cbind(result_dt, sdx_columns, proc_columns)

# Rename columns
setnames(result_dt,
  old = c("clin_discharge", "pat_bwt", "pat_age", "pat_sex"),
  new = c("discharge", "birthweight", "patage", "patsex")
)

# Replace NA with "None" in non-date columns
non_date_columns <- setdiff(names(result_dt), date_columns)
result_dt[, (non_date_columns) := lapply(.SD, function(x) ifelse(is.na(x), NA_character_, x)), .SDcols = non_date_columns]

# Columns you want to come first
priority_columns <- c(
  "id_series", "id_pin", "date_adm", "time_adm", "date_dis", "time_dis",
  "date_rec", "date_ref", "date_check", "id_hci", "id_hcp", "clin_outpatient",
  "clin_emergency", "pat_type", "clin_acc", "pat_rel", "pat_bdate", "patage",
  "patsex", "birthweight", "pat_memcat_parent", "pat_memcat_child", "discharge",
  "clin_c1", "clin_c2", "claim_status", "claim_payout", "claim_charge",
  "date_ext", "id_year", "clin_icd", "clin_rvs", "clin_c1_orig",
  "clin_c2_orig", "icd9_list", "pdx", "pdx_code"
)

# Remaining columns
remaining_columns <- c(
  "sdx1", "sdx2", "sdx3", "sdx4", "sdx5", "sdx6",
  "sdx7", "sdx8", "sdx9", "sdx10", "sdx11", "sdx12",
  "proc1", "proc2", "proc3", "proc4", "proc5", "proc6",
  "proc7", "proc8", "proc9", "proc10", "proc11", "proc12",
  "proc13", "proc14", "proc15", "proc16", "proc17", "proc18",
  "proc19", "proc20" # , "ageday" , "discharge", "birthweight"
)

# Combine the lists, ensuring no duplicates
desired_columns <- unique(c(priority_columns, remaining_columns))

# Reorder the data.table columns
setcolorder(result_dt, desired_columns)

# Add missing columns with "None" values if they are not already in the data.table
missing_columns <- setdiff(desired_columns, names(result_dt))
result_dt[, (missing_columns) := NA_character_]

# Initialize the 'ageday' column with NA
result_dt[, ageday := NA_real_]

# Calculate 'ageday' only for valid rows where pat_age < 1 and date_adm/pat_bdate are non-NA
result_dt[
  !is.na(patage) & patage < 1 & !is.na(date_adm) & !is.na(pat_bdate),
  ageday := as.numeric(difftime(date_adm, pat_bdate, units = "days"))
]

# Convert time_adm to numeric
result_dt[, time_adm := as.numeric(time_adm)]
result_dt[, time_dis := as.numeric(time_dis)] # Also do the same for time_dis if needed

# Convert time from seconds since midnight to "HH:MM:SS" format
result_dt[, time_adm := sprintf("%02d:%02d:%02d", time_adm %/% 3600, (time_adm %% 3600) %/% 60, time_adm %% 60)]
result_dt[, time_dis := sprintf("%02d:%02d:%02d", time_dis %/% 3600, (time_dis %% 3600) %/% 60, time_dis %% 60)]

# Convert ITime to character format "HH:MM:SS"
result_dt[, time_adm := format(as.ITime(time_adm), "%H:%M:%S")]
result_dt[, time_dis := format(as.ITime(time_dis), "%H:%M:%S")]

# Combine Date and Time and convert to POSIXct
result_dt[, date_adm := as.character(paste(date_adm, time_adm), format = "%Y-%m-%d %H:%M:%S")]
result_dt[, date_dis := as.character(paste(date_dis, time_dis), format = "%Y-%m-%d %H:%M:%S")]

# Replace NA values in 'ageday' with "None"
result_dt[, ageday := fifelse(is.na(ageday), NA, as.character(ageday))]

before_replacing_with_none <- data.table::copy(result_dt)

replace_result <- replace_empty_with_none(result_dt)

result_dt <- replace_result$return_data

# Write the final DataFrame to CSV
fwrite(result_dt, here(checkpoint_7_path, paste0(checkpoint_7b_prefix, ".csv")))

# for (col in date_columns) print(unique(result_dt[[col]]))

# pandas <- import("pandas")

# print(sapply(result_dt, class))

csv_path <- here(checkpoint_7_path, paste0(checkpoint_7b_prefix, ".csv"))

# Use reticulate to run the following Python code within the R environment
py_run_string(paste0("
import pandas as pd
import numpy as np
from io import StringIO
import sys

# Read the CSV with the specified dtype
pandas_df = pd.read_csv('", csv_path, "',

dtype = {
    'id_series': 'string',
    'id_pin': 'string',
    'date_adm': 'string',  # POSIXct/POSIXt in R
    'time_adm': 'string',  # character in R
    'date_dis': 'string',  # POSIXct/POSIXt in R
    'time_dis': 'string',  # character in R
    'date_rec': 'string',  # Date in R
    'date_ref': 'string',  # Date in R
    'date_check': 'string',  # Date in R
    'id_hci': 'string',
    'id_hcp': 'string',
    'clin_outpatient': 'string',
    'clin_emergency': 'string',
    'pat_type': 'string',
    'clin_acc': 'string',
    'pat_rel': 'string',
    'pat_bdate': 'string',  # Date in R
    'patage': 'float64',  # numeric in R
    'patsex': 'string',
    'birthweight': 'float64',  # character in R
    'pat_memcat_parent': 'string',
    'pat_memcat_child': 'string',
    'discharge': 'Int64',  # character in R
    'clin_c1': 'string',
    'clin_c2': 'string',
    'claim_status': 'string',
    'claim_payout': 'string',  # character in R
    'claim_charge': 'string',  # character in R
    'date_ext': 'string',  # Date in R
    'id_year': 'int64',  # integer in R
    'clin_icd': 'object',  # list in R
    'clin_rvs': 'string',
    'clin_c1_orig': 'string',
    'clin_c2_orig': 'string',
    'icd9_list': 'string',
    'pdx': 'string',
    'pdx_code': 'int64',  # integer in R
    'sdx1': 'string',
    'sdx2': 'string',
    'sdx3': 'string',
    'sdx4': 'string',
    'sdx5': 'string',
    'sdx6': 'string',
    'sdx7': 'string',
    'sdx8': 'string',
    'sdx9': 'string',
    'sdx10': 'string',
    'sdx11': 'string',
    'sdx12': 'string',
    'proc1': 'string',
    'proc2': 'string',
    'proc3': 'string',
    'proc4': 'string',
    'proc5': 'string',
    'proc6': 'string',
    'proc7': 'string',
    'proc8': 'string',
    'proc9': 'string',
    'proc10': 'string',
    'proc11': 'string',
    'proc12': 'string',
    'proc13': 'string',
    'proc14': 'string',
    'proc15': 'string',
    'proc16': 'string',
    'proc17': 'string',
    'proc18': 'string',
    'proc19': 'string',
    'proc20': 'string',
    'ageday': 'float64'
}
# , parse_dates = [
#     'date_adm',
#     'date_dis',
#     'date_rec',
#     'date_ref',
#     'date_check',
#     'pat_bdate',
#     'date_ext'
# ]
)

pandas_df = pandas_df.replace(pd.NA, None)
pandas_df = pandas_df.replace(np.nan, None)
pandas_df = pandas_df.replace('<NA>', None)

# Capture pandas_df.info() output
buffer = StringIO()
pandas_df.info(buf=buffer)
info_output = buffer.getvalue()

info_output
"))

# Print the captured output in R
cat(py$info_output)

# Step 6: Process each row of the DataFrame through `drg_seeker` and append results
py_run_file(here("data-cleaning", "py_scripts", "run_drg_seeker.py"))


# # Import pandas
# pandas <- import("pandas")

# # Step 1: Copy the master_dt to avoid modifying the original data
# python_input_dt <- fread(here(checkpoint_6_path, paste0(checkpoint_6_prefix, ".csv")))

# # Convert columns to Date objects, ignoring NA values
# date_columns <- names(python_input_dt)[grepl("date", names(python_input_dt))]

# python_input_dt[, pat_bdate_orig := as.Date(pat_bdate, format = "%m/%d/%Y")]
# python_input_dt[, (date_columns) := lapply(.SD, as.Date, format = "%m/%d/%Y"), .SDcols = date_columns]

# # Create a data.table for rows with negative `pat_age`
# negative_age_dt <- python_input_dt[pat_age < 0, ]

# # Create a data.table for rows with non-negative `pat_age`
# positive_age_dt <- python_input_dt[pat_age >= 0, ]

# # Process rows with non-negative `pat_age`
# positive_age_dt[, pat_bdate := dmy(generate_dob(format(pat_bdate_orig, "%Y-%m-%d"), pat_age, format(date_adm, "%Y-%m-%d")))]

# # For negative `pat_age`, keep `pat_bdate` as it is
# negative_age_dt[, pat_bdate := pat_bdate_orig]

# # Combine the processed data
# result_dt <- rbind(positive_age_dt, negative_age_dt)

# # Write the result to CSV
# csv_path <- here(checkpoint_7_path, paste0(checkpoint_7a_prefix, ".csv"))

# fwrite(result_dt, csv_path)

# # Use reticulate to run the following Python code within the R environment
# py_run_string(paste0("
# import pandas as pd

# # Create a dictionary with all columns set to 'string' type
# dtype_dict = {
#     'id_series': 'string',
#     'id_pin': 'string',
#     'date_adm': 'string',
#     'time_adm': 'string',
#     'date_dis': 'string',
#     'time_dis': 'string',
#     'date_rec': 'string',
#     'date_ref': 'string',
#     'date_check': 'string',
#     'id_hci': 'string',
#     'id_hcp': 'string',
#     'clin_outpatient': 'string',
#     'clin_emergency': 'string',
#     'pat_type': 'category',
#     'clin_acc': 'category',
#     'pat_rel': 'category',
#     'pat_bdate': 'string',
#     'pat_age': 'float64',
#     'pat_sex': 'category',
#     'pat_bwt': 'float64',
#     'pat_memcat_parent': 'category',
#     'pat_memcat_child': 'category',
#     'clin_discharge': 'Int64',
#     'clin_c1': 'string',
#     'clin_c2': 'string',
#     'claim_status': 'category',
#     'claim_payout': 'float64',
#     'claim_charge': 'float64',
#     'date_ext': 'string',
#     'id_year': 'string',
#     'clin_icd': 'string',
#     'clin_rvs': 'string',
#     'clin_c1_orig': 'string',
#     'clin_c2_orig': 'string',
#     'icd9_list': 'string',
#     'pdx': 'string',
#     'pdx_code': 'string',
#     'pat_bdate_orig': 'string'
# }

# # Read the CSV with the specified dtype
# pandas_df = pd.read_csv('", csv_path, "', dtype=dtype_dict)
# "))

# # Access the pandas DataFrame in R if needed
# pandas_df <- py$pandas_df

# # View the structure
# str(pandas_df)

# # Step 3: Convert `clin_icd` and `icd9_list` columns, replace NaN with "None"
# py_run_file(here("data-cleaning", "py_scripts", "format_data.py"))

# # Retrieve the processed DataFrame back to R
# subset_df <- py$output

# # str(subset_df)
# # Step 1: Ensure date columns are properly converted to Date objects
# subset_df$date_adm <- as.Date(subset_df$date_adm, format = "%Y-%m-%d")
# subset_df$pat_bdate <- as.Date(subset_df$pat_bdate, format = "%Y-%m-%d")

# # Step 2: Initialize the 'ageday' column with NA
# subset_df$ageday <- NA

# # Step 3: Calculate 'ageday' only for valid rows where pat_age == 0
# valid_rows <- subset_df$patage >= 0 & subset_df$patage %% 1 == 0

# subset_df$ageday[valid_rows & subset_df$patage == 0] <- as.numeric(
#   difftime(
#     subset_df$date_adm[valid_rows & subset_df$patage == 0],
#     subset_df$pat_bdate[valid_rows & subset_df$patage == 0],
#     units = "days"
#   )
# )

# subset_df$date_adm <- as.POSIXct(paste(subset_df$date_adm, subset_df$time_adm), format = "%Y-%m-%d %H:%M:%S")
# subset_df$date_dis <- as.POSIXct(paste(subset_df$date_dis, subset_df$time_dis), format = "%Y-%m-%d %H:%M:%S")

# # Step 4: Replace NA values in 'ageday' with "None"
# subset_df$ageday[is.na(subset_df$ageday)] <- "None"

# fwrite(as.data.table(subset_df), here(checkpoint_7_path, paste0(checkpoint_7b_prefix, ".csv")))
# # Step 5: Convert the DataFrame to a format suitable for Python processing
# py$pandas_df <- pandas$read_csv(here(checkpoint_7_path, paste0(checkpoint_7b_prefix, ".csv")))

# # Step 6: Process each row of the DataFrame through `drg_seeker` and append results
# py_run_file(here("data-cleaning", "py_scripts", "run_drg_seeker.py"))


# str(py$output)

# Convert data types to match BigQuery schema
result <- as.data.table(py$output)

# Define columns for each type conversion
character_columns <- c(
  "id_series", "id_pin", "id_hci", "pat_type", "clin_acc", "pat_rel",
  "pat_sex", "pat_memcat_parent", "pat_memcat_child", "claim_status",
  "pdx", "mdc", "pdc", "dc", "py_drg", "ageday", "error_code",
  "warning_code", "pat_bwt" # , "thai_drg"
)

date_columns <- c(
  "date_adm", "date_dis", "date_rec", "date_ref", "date_check",
  "pat_bdate", "date_ext"
)

integer_columns <- c(
  "clin_discharge", "pdx_code", "id_year" # , "ot", "err", "warn", "los"
)

numeric_columns <- c(
  "pat_age", "claim_payout", "claim_charge", "pccl"
  # , "rw", "wtlos", "adjrw"
)

logical_columns <- c("clin_outpatient", "clin_emergency")

time_columns <- c("time_adm", "time_dis")

# Apply conversions
result[, (character_columns) := lapply(.SD, as.character), .SDcols = character_columns]
result[, (date_columns) := lapply(.SD, as.Date), .SDcols = date_columns]
result[, (integer_columns) := lapply(.SD, as.integer), .SDcols = integer_columns]
result[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns]
result[, (logical_columns) := lapply(.SD, function(x) as.logical(as.integer(x))), .SDcols = logical_columns]
result[, (time_columns) := lapply(.SD, as.ITime), .SDcols = time_columns]

# Convert string columns to arrays
array_columns <- c("id_hcp", "clin_c1", "clin_c2", "clin_icd", "clin_rvs")
result[, (array_columns) := lapply(.SD, function(x) strsplit(x, "\\|\\|")), .SDcols = array_columns]

# Replace NULL (empty) arrays with an empty character vector
result[, (array_columns) := lapply(.SD, function(x) {
  lapply(
    x,
    function(y) if (length(y) == 0 || is.null(y) || all(is.na(y))) character(0) else y
  )
}), .SDcols = array_columns]


# rename columns for bq push, dropping the pre-renamed source columns, also drop mdc and dc
result[, clin_pdx := pdx]
result[, clin_sdx := clin_icd]
result[, pdx := NULL]
result[, clin_icd := NULL]
# result[, thai_rw := rw]
# result[, thai_wtlos := wtlos]
# result[, thai_ot := ot]
# result[, thai_adjrw := adjrw]
# result[, thai_err := err]
# result[, thai_warn := warn]
# result[, thai_los := los]
# result[, rw := NULL]
# result[, wtlos := NULL]
# result[, ot := NULL]
# result[, adjrw := NULL]
# result[, err := NULL]
# result[, warn := NULL]
# result[, los := NULL]
result[, py_pdc := pdc]
result[, py_pccl := pccl]
result[, py_warn := warning_code]
result[, py_err := error_code]
result[, pdc := NULL]
result[, pccl := NULL]
result[, warning_code := NULL]
result[, error_code := NULL]


result[, mdc := NULL]
result[, dc := NULL]


pre_pad_id_series_nrow <- result[, uniqueN(id_series)]
pre_pad_id_pin_nrow <- result[, uniqueN(id_pin)]
# pre_paid_thai_drg_nrow <- result[, uniqueN(thai_drg)]


# result[, thai_drg := str_pad(thai_drg, width = 5, side = "left", pad = "0")]
result[, id_series := str_pad(id_series, width = 13, side = "left", pad = "0")]
result[, id_pin := str_pad(id_pin, width = 20, side = "left", pad = "0")]


post_pad_id_series_nrow <- result[, uniqueN(id_series)]
post_pad_id_pin_nrow <- result[, uniqueN(id_pin)]
# post_paid_thai_drg_nrow <- result[, uniqueN(thai_drg)]

if (pre_pad_id_series_nrow != post_pad_id_series_nrow) stop("Error: id_series differs pre and post padding") else message("id_series nrow integrity valid")
if (pre_pad_id_pin_nrow != post_pad_id_pin_nrow) stop("Error: id_series differs pre and post padding") else message("id_pin nrow integrity valid")
# if (pre_paid_thai_drg_nrow != post_paid_thai_drg_nrow) stop("Error: thai_drg differs pre and post padding") else message("thai_drg nrow integrity valid")


# fwrite(result, "test.csv")


export_for_grouper(
  result,
  here(checkpoint_4_path, paste0(
    checkpoint_4_prefix, year_to_load, suffix, ".txt"
  ))
)

# Upload the file
gcs_auth(email = gcs_email)
gcs_upload(
  file = here(checkpoint_4_path, paste0(checkpoint_4_prefix, year_to_load, suffix, ".txt")),
  bucket = gcs_bucket,
  name = paste0(gcs_pre_fpath, "/", paste0(checkpoint_4_prefix, year_to_load, suffix, ".txt")),
  predefinedAcl = "bucketLevel"
)

if (thai_prompt || to_prompt) {
  response <- tolower(readline(prompt = "Have you run the Thai grouper manually? (y/n): "))
  if (response == "y") {
    message("Continuing with the script...\n")
    # Continue with the rest of the script
  } else {
    message("Stopping the script.\n")
    stop("Thai Grouper not run yet. Script terminated. Continue on manually if necessary")
  }
} else {
  message("Thai Grouper is assumed to have been run already. Continuing with the script...\n")
}

gcs_get_object(
  object_name = paste0(gcs_post_fpath, "/", toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT"),
  bucket = gcs_bucket,
  saveToDisk = here(checkpoint_5_path, paste0(toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT")),
  overwrite = TRUE
)


result[, caseid := 1:nrow(result)]
thai_result <- fread(here(checkpoint_5_path, paste0(toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT")), colClasses = "character")
thai_result[, caseid := as.integer(caseid)]
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
merged <- merge(result, thai_result, by = "caseid", all.x = TRUE)
diff_merged <- merged[!thai_drg == py_drg]
print(head(diff_merged))
fwrite(diff_merged, "test.csv")


# # Upload the file
# if (to_gcs) {
#   gcs_auth(email = gcs_email)
#   gcs_upload(
#     file = here(checkpoint_4_path, paste0(checkpoint_4_prefix, year_to_load, suffix, ".txt")),
#     bucket = gcs_bucket,
#     name = paste0(gcs_pre_fpath, "/", paste0(checkpoint_4_prefix, year_to_load, suffix, ".txt")),
#     predefinedAcl = "bucketLevel"
#   )
# }


# claims_fpath <- here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, ".csv"))
# grouper_fpath <- here(checkpoint_5_path, paste0(toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT"))
# merged_fpath <- here(checkpoint_6_path, paste0(checkpoint_6_prefix, ".csv"))

# if (thai_prompt || to_prompt || !file.exists(grouper_fpath)) {
#   response <- tolower(readline(prompt = "Have you run the Thai grouper manually? (y/n): "))
#   if (response == "y") {
#     message("Continuing with the script...\n")
#     # Continue with the rest of the script
#   } else {
#     message("Stopping the script.\n")
#     stop("Thai Grouper not run yet. Script terminated. Continue on manually if necessary")
#   }
# } else {
#   message("Thai Grouper is assumed to have been run already. Continuing with the script...\n")
# }


# if (to_gcs) {
#   gcs_get_object(
#     object_name = paste0(gcs_post_fpath, "/", toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT"),
#     bucket = gcs_bucket,
#     saveToDisk = here(checkpoint_5_path, paste0(toupper(paste0(checkpoint_5_prefix, year_to_load, suffix)), "Res.TXT")),
#     overwrite = TRUE
#   )
# }


# # Please run thai grouper first
# claims <- fread(claims_fpath, colClasses = "character")
# # claims[, caseid := 1:.N]
# # claims[, caseid := as.character(caseid)]
# # grouper <- fread(grouper_fpath, sep = "|", na.strings = "--", header = TRUE, colClasses = "character")
# # result <- merge(claims, grouper, by = "caseid", all.x = TRUE)

# # result[, caseid := as.integer(caseid)]

# # Convert data types to match BigQuery schema
# result[, id_series := as.character(id_series)]
# result[, id_pin := as.character(id_pin)]
# result[, date_adm := as.Date(date_adm, format = "%m/%d/%Y")]
# result[, time_adm := as.ITime(time_adm)]
# result[, date_dis := as.Date(date_dis, format = "%m/%d/%Y")]
# result[, time_dis := as.ITime(time_dis)]
# result[, date_rec := as.Date(date_rec, format = "%m/%d/%Y")]
# result[, date_ref := as.Date(date_ref, format = "%m/%d/%Y")]
# result[, date_check := as.Date(date_check, format = "%m/%d/%Y")]
# result[, id_hci := as.character(id_hci)]

# # Convert character "0"/"1" to logical for Boolean fields
# result[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
# result[, clin_emergency := as.logical(as.integer(clin_emergency))]

# result[, pat_type := as.character(pat_type)]
# result[, clin_acc := as.character(clin_acc)]
# result[, pat_rel := as.character(pat_rel)]
# result[, pat_bdate := as.Date(pat_bdate, format = "%m/%d/%Y")]
# result[, pat_age := as.numeric(pat_age)]
# result[, pat_sex := as.character(pat_sex)]
# result[, pat_bwt := as.numeric(pat_bwt)]
# result[, pat_memcat_parent := as.character(pat_memcat_parent)]
# result[, pat_memcat_child := as.character(pat_memcat_child)]
# result[, clin_discharge := as.integer(clin_discharge)]

# result[, claim_status := as.character(claim_status)]
# result[, claim_payout := as.numeric(claim_payout)]
# result[, claim_charge := as.numeric(claim_charge)]
# result[, date_ext := as.Date(date_ext, format = "%m/%d/%Y")]
# result[, id_year := as.integer(id_year)]

# result[, clin_c1_orig := as.character(clin_c1_orig)]
# result[, clin_c2_orig := as.character(clin_c2_orig)]

# result[, pdx := as.character(pdx)]
# result[, pdx_code := as.integer(pdx_code)]
# # result[, drgname := NULL]
# # result[, drg := as.character(drg)]
# # result[, rw := as.numeric(rw)]
# # result[, wtlos := as.numeric(wtlos)]
# # result[, ot := as.integer(ot)]
# # result[, adjrw := as.numeric(adjrw)]
# # result[, err := as.integer(err)]
# # result[, warn := as.integer(warn)]
# # result[, los := as.integer(los)]

# fwrite(result, merged_fpath) # Write to file for python grouper before strsplit


if (nrow(result) == total_rows) bq_table <- paste0("claims_", year_to_load, "1231")

# Check if the table should be dropped and replaced
if (to_drop_bq) {
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      # If the table does not exist, just continue
      if (grepl("Not found", e$message, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        # If it's a different error, re-throw the error
        stop(e)
      }
    }
  )
}

# Attempt to create the table
tryCatch(
  {
    bq_table_create(
      bq_table(gcp_proj, bq_dataset, bq_table),
      fields = fromJSON(here("data-cleaning/r_scripts", "bq_schema.json"), simplifyDataFrame = FALSE)
    )
    skip_bq_upload <<- FALSE
    message("Table created successfully.\n")
  },
  error = function(e) {
    # Check if the error message indicates that the table already exists
    if (grepl("already exists", e$message, ignore.case = TRUE)) {
      skip_bq_upload <<- TRUE
      message("Table already exists. Skipping creation and upload.")
    } else {
      # If it's a different error, re-throw the error
      stop(e)
    }
  }
)

# Upload to BQ only if table is empty
if (to_bq && !skip_bq_upload) {
  tryCatch(
    {
      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = result,
        write_disposition = "WRITE_EMPTY"
      )
      message("Data uploaded successfully with WRITE_EMPTY.\n")
    },
    error = function(e) {
      if (grepl("already exists", e$message, ignore.case = TRUE)) {
        # Handle the specific "already exists" error
        message("Upload skipped: table already exists and is not empty.")
      } else {
        # Handle all other errors
        message("Error during upload: ", e$message)
      }
    }
  )
}


# Print time estimates along with estimate for full claims file
print_time_estimates()


# in case we want to run this cell independently:
source(here::here("data-cleaning/r_scripts", "03_timing-debug-functions.R"))

# Consolidate all r_scripts scripts into debug.R; useful for debugging
concatenate_r_files(
  here::here("data-cleaning/r_scripts"),
  here::here("data-cleaning/debug/debug.R")
)

if (.Platform$OS.type == "unix") system("cd ~/drg-pipeline && jupyter nbconvert --no-prompt --to script data-cleaning/drg-cleaning.ipynb --output debug/drg-cleaning")


if (FALSE) {
  to_flush_master <- to_debug <- TRUE
  to_flush_partial <- FALSE
}

# Define the paths and corresponding conditions
paths <- list(
  to_flush_master = c(
    "data-cleaning/cache",
    "data-cleaning/data/profvis",
    "data-cleaning/data/aux-files",
    "data-cleaning/data/checkpoints",
    "data-cleaning/debug"
  ),
  to_flush_partial = c(
    "data-cleaning/data/claims/raw/parts",
    "data-cleaning/data/claims/raw/samples"
  )
)

# Iterate over the paths and conditions to delete them if the condition is true
for (condition in names(paths)) {
  if (get(condition)) {
    system(paste(
      "rm -r",
      paste(here::here(unlist(paths[[condition]])), collapse = " ")
    ))
  }
}

if (to_debug) {
  rm(list = ls())
  gc()
}

