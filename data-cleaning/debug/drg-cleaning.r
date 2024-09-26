system("git submodule update --init --recursive")
# system("git submodule foreach --recursive git fetch && git submodule foreach --recursive && git reset --hard origin/main")
# Sys.setenv(PYTHONPATH = here::here("data-cleaning", "grouper"))


# Delete all R objects and run garbage collection so we start with a clean slate
rm(list = ls())
gc()


# Load libraries and minor parameters
source(here::here("data-cleaning/r_scripts", "00_libraries-params.R"))


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
bq_table <- "temp_claims" # temp bq table, later renamed to claims_20XX1231 in Push to BQ section

# Input:
to_sample <- FALSE # Whether to sample each split_part by sample_size_divisor (useful when iterating through code runs in quick succession)
sample_size_divisor <- 25 # Sample size divisor: Formula for sample size is total_rows / split_parts / sample_size_divisor. Choose between 5, 25, 125, and 625

# Output:
to_write <- TRUE # Whether to write out checkpoint_1 files (everything up until converting for grouper export)
to_combine <- TRUE # Whether to combine checkpoint 1 files into one data.table
to_group <- TRUE # Whether to export for the batch grouper or not
to_gcs <- TRUE # Whether to push to GCS or nt (Thai Grouper Input/Output)
to_bq <- TRUE # Whether to push to BQ or not
to_drop_bq <- TRUE # Whether to drop the existing bq table and recreate it

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
ram_size <- 64 # Input virtual or physical machine's RAM size here

# Print GCP project
message(paste("GCP Project:", gcp_proj, "\n"))


# Debug Parameters:
to_debug <- FALSE # whether to print debug statements
to_profvis <- FALSE # Conduct runtime duration analysis via profvis or not
to_view_checks <- TRUE # Whether to view checks and print statements
to_view_checks_parallel <- FALSE # Whether to view intermediate per split_part/chunk checks and print statements (not consolidated) when parallelized
to_parallel <- TRUE # Whether to parallelize each split_parts split_part into availableCores() chunks. Cuts down processing time from 120min to 15min.
to_split_read <- FALSE # WARNING: TRUE uses a lot of memory!!
to_dec_mem_usage <- TRUE # Whether to run rm() and gc() at every possible step
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
checkpoint_9_path <- file.path(chkpt_path, "checkpoint_9_grouper_differences")
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
  message("All directories exist.\n")
} else {
  message("The following directories were created:\n")
  message(paste(paste(created_dirs, collapse = ",\n"), "\n"))
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
message(sprintf("Setting future.globals.maxSize to: %.1f GB", ram_limit / (1024^3)))


# Hide verbose outputs and warnings
options(verbose = FALSE) # Hide verbose output for script and library loading
options(warn = -1) # Hide warnings for script sourcing and library loading


scripts <- list( # List of scripts to source
  # lib_params = "00_libraries-params.R",
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
    message(paste("File", file_name, "already exists in the target directory. Skipping download.\n"))
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

# 6. Query and load `drg-pipeline.grouper_v5.i10vx`
i10vx_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".grouper_v5.i10vx` LIMIT ", max_bq_rows
)
i10vx <- query_bq_to_dt(i10vx_query, here(aux_path, "i10vx.csv"),
  cache = FALSE, max_bq_rows = max_bq_rows
)
setkey(i10vx, "code")
acc_icd <- unique(i10vx[, code])


str(acc_icd)


# Load cached total rows file if available, saves ~10 seconds of runtime
total_rows_file <- here(cache_path, paste0("total_rows_", year_to_load, ".rds"))
if (file.exists(total_rows_file)) {
  total_rows <- readRDS(total_rows_file)
  message(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- fread(file = full_claims_file, select = 1L, header = TRUE, colClasses = "character")[, .N]
  saveRDS(total_rows, file = total_rows_file)
  message(paste("Total Rows via fread:", total_rows))
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

  dt[, c1 := clin_c1]
  dt[, c2 := clin_c2]

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

  # Clean and compare c1 and c2 columns
  c1_cleaning_comparison <- clean_clinical_column("c1")
  c2_cleaning_comparison <- clean_clinical_column("c2")

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
  dt[, c1 := lapply(c1,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]
  dt[, c2 := lapply(c2,
    replace_multiple_patterns,
    patterns = manual_patterns_to_replace,
    replacements = manual_code_replacements
  )]

  # Remove lumped ICD codes
  dt[, c1 := remove_lumped_icd_codes(c1)]
  dt[, c2 := remove_lumped_icd_codes(c2)]
  # dt[, clin_rvs := remove_lumped_rvs_codes(clin_rvs)]

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
    ICD_replacements_1 = c1_cleaning_comparison,
    ICD_replacements_2 = c2_cleaning_comparison,
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
implement_icd10_mapping <- function(c1, c2, clin_icd, tdrg_icd10) {
  icds <- get_unique_icd_codes(c1, c2, clin_icd)

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
    for (col_name in c("c1", "c2", "clin_icd")) {
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
    c1, c2, clin_icd, icd10_env
  )
  return(
    list(
      # main return variables to be saved as columns in the dt
      c1 = mapped_columns$c1,
      c2 = mapped_columns$c2,
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
    # message("\rViewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  clean_result <- clean_data(chunk)
  chunk <- clean_result$return_data

  if (to_debug) fwrite(chunk, "test1.csv")
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  if (to_debug) fwrite(chunk, "test2.csv")

  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- implement_icd10_mapping(
    c1, c2, clin_icd, tdrg_icd10
  )
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  res2 <- replace_empty_with_na(dt = chunk, to_view_checks)
  chunk <- res2$return_data
  empty_strings_replaced_2 <- res2$return_replacement_summary

  # fwrite(chunk, "test2b.csv")

  # Function to remove all whitespace characters from character vectors
  remove_whitespace <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_) # Return NA for NULL or empty lists
    } else {
      return(gsub("\\s+", "", x)) # Remove all whitespace characters
    }
  }

  # Apply the function to the list columns 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  chunk[, c1 := as.character(c1)]
  chunk[, c2 := as.character(c2)]

  # Write the combined data.table to a single CSV file
  if (to_debug) fwrite(data.table(Column = colnames(chunk), Class = sapply(chunk, class)), "class.csv")

  pdx_result <- apply_find_pdx(
    chunk$c1, chunk$c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  if (to_debug) fwrite(chunk, "test2c.csv")

  # Function to remove clin_pdx from list columns
  remove_pdx_from_list <- function(pdx, lst) {
    if (!is.na(pdx)) {
      # Remove clin_pdx from the list and ensure the result is a character vector
      lst <- setdiff(lst, pdx)
    }
    return(lst)
  }

  # Apply the function to each row and ensure the result is a character vector within the list column
  chunk[, c1 := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], c1[[i]])))]
  chunk[, c2 := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], c2[[i]])))]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) as.character(remove_pdx_from_list(pdx[i], clin_icd[[i]])))]

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

  if (!file.exists(here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_parts),
    "_of_", split_parts, ".rds"
  )))) {
    full_file <<- fread(
      file = full_claims_file, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
  }

  split_and_save <- function(split_and_save_part) {
    rows_per_part <- ceiling(total_rows / split_parts)
    chunk_file <- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", split_and_save_part),
      "_of_", split_parts, ".rds"
    ))

    # Only process if the part does not already exist
    if (!file.exists(chunk_file)) {
      start_row <- (split_and_save_part - 1) * rows_per_part + 1
      end_row <- min(split_and_save_part * rows_per_part, total_rows)

      # Handle the first chunk with a header, skip header for subsequent chunks
      chunk_dt <- full_file[start_row:end_row]

      # Debug print
      if (to_debug) print(head(chunk_dt), 2)

      # Write the chunk to a CSV file
      saveRDS(chunk_dt, chunk_file, compress = FALSE)

      # Reduce memory usage if specified
      if (to_dec_mem_usage) rm(chunk_dt)
      if (to_dec_mem_usage) gc()

      # Save processing time for this part
      split_processing_times[[split_loop_part]] <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )

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
    # message(paste0("Partial file no. ", loop_part, " doesn't exist yet. Processing now."))
    partial_claims_file <<- here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))

    # ensure_partial_files_exist(loop_part) # Ensure partial exist

    if (to_sample) {
      sampled_claims_file <<- here(raw_claims_samples_path, paste0(
        "sampled_claims_", year_to_load, "_", sample_size,
        "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
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

    # if (to_dec_mem_usage) rm(processed_chunks) # debug

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
      message(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
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
      saveRDS(
        summarized_dt, here(checkpoint_1_path, paste0(
          checkpoint_1_prefix, year_to_load, suffix,
          "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
        )),
        compress = FALSE
      )
    }
    # if (to_combine) master_dt_list[[loop_part]] <- summarized_dt # TODO: DISABLING FOR NOW

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

  message("\nRow Counts Match for All Parts\n") # only prints if above succeeds

  if (to_combine) {
    for (read_part in 1:split_parts) {
      master_dt_list[[read_part]] <- readRDS(here(checkpoint_1_path, paste0(
        checkpoint_1_prefix, year_to_load, suffix,
        "part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
      )))
    }
    master_dt <<- rbindlist(master_dt_list)

    if (to_debug) print(head(master_dt))
    if (to_dec_mem_usage) {
      if (to_group) rm(master_dt_list) else rm(master_dt_list) # rm(master_grouper_input_list)
      gc()
    }
    if (to_write) {
      saveRDS(master_dt, here(checkpoint_2_path, paste0(
        checkpoint_2_prefix, year_to_load, suffix, ".rds"
      )), compress = FALSE)
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
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))

# Call the main function with or without profvis
if (to_profvis) {
  saveWidget(profvis({
    main_logic_func()
  }), profvis_fpath)
} else {
  main_logic_func()
}


if (exists("master_dt")) {
  result <- data.table::copy(master_dt)
  rm(master_dt)
  gc()
} else {
  result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, ".rds")))
  gc()
}

# Track invalid age corrections
invalid_age_before <- nrow(result[pat_age < -1 | pat_age > 124, .(id_series)])

# Use c1_orig and c2_orig as clin_c1 and clin_c2
result[, clin_c1 := c1_orig]
result[, clin_c2 := c2_orig]
result[, c("c1_orig", "c2_orig") := NULL]

# Use icd9_list as clin_proc
result[, clin_proc := icd9_list]
result[, icd9_list := NULL]

# Remove the primary diagnosis from the list of secondary diagnoses
result[, clin_icd := Map(function(pdx_var, sdx_var) sdx_var[sdx_var != pdx_var], pdx, clin_icd)]
result[, clin_sdx := clin_icd]
result[, clin_icd := NULL]

result[, c("c1", "c2", "clin_rvs") := NULL]
gc()
# Process date columns
date_cols <- c("date_adm", "date_dis", "date_rec", "date_ref", "date_check", "pat_bdate", "date_ext")
if (is_unix) {
  result[, (date_cols) := mclapply(.SD, function(x) as.Date(x, format = "%m/%d/%Y"), mc.cores = parallel::detectCores()), .SDcols = date_cols]
} else {
  result[, (date_cols) := lapply(.SD, function(x) as.Date(x, format = "%m/%d/%Y")), .SDcols = date_cols]
}

# Process time columns
time_cols <- c("time_adm", "time_dis")
standardize_time <- function(x) {
  x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
  as.ITime(x)
}
if (is_unix) {
  result[, (time_cols) := mclapply(.SD, standardize_time, mc.cores = parallel::detectCores()), .SDcols = time_cols]
} else {
  result[, (time_cols) := lapply(.SD, standardize_time), .SDcols = time_cols]
}

# Process logical columns
result[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
result[, clin_emergency := as.logical(as.integer(clin_emergency))]

# Process numeric columns
num_cols <- c("pat_age", "pat_bwt", "clin_discharge", "claim_payout", "claim_charge", "id_year", "pdx_code")
if (is_unix) {
  result[, (num_cols) := mclapply(.SD, as.numeric, mc.cores = parallel::detectCores()), .SDcols = num_cols]
} else {
  result[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
}

# Process integer columns
int_cols <- c("clin_discharge", "id_year", "pdx_code")
if (is_unix) {
  result[, (int_cols) := mclapply(.SD, as.integer, mc.cores = parallel::detectCores()), .SDcols = int_cols]
} else {
  result[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
}

# Process character columns
char_cols <- c("id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex", "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx")
if (is_unix) {
  result[, (char_cols) := mclapply(.SD, as.character, mc.cores = parallel::detectCores()), .SDcols = char_cols]
} else {
  result[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
}

# Age correction logic
invalid_ages_before_correction <- result[pat_age < 0 | pat_age > 124, .N]
invalid_age_ids_before <- result[pat_age < 0 | pat_age > 124, id_series]

result[pat_age < 0 & pat_age >= -1, pat_age := 0]
result[pat_age < -1 & is.na(pat_bdate), pat_age := NA_integer_]
result[pat_age > 124, pat_age := NA_integer_]
result[pat_age < -1 & !is.na(pat_bdate), pat_age := floor(as.numeric(interval(pat_bdate, date_adm) / years(1)))]

# Regenerate or correct DOB
invalid_bdate_before <- result[is.na(pat_bdate), .N]
invalid_bdate_ids_before <- result[is.na(pat_bdate), id_series]

result[!is.na(pat_age), pat_bdate := dmy(generate_dob(format(pat_bdate, "%Y-%m-%d"), pat_age, format(date_adm, "%Y-%m-%d")))]
result[!is.na(pat_bdate) & pat_bdate <= date_adm, pat_age := floor(as.numeric(interval(pat_bdate, date_adm) / years(1)))]
result[pat_age < -1 & !is.na(pat_bdate) & pat_bdate > date_adm, pat_age := NA_integer_]
result[pat_age > 124 | pat_age < 0, pat_age := NA_integer_]

# Save invalid age rows to CSV
invalid_age_path <- here("data-cleaning", "debug", "invalid_age.csv")
fwrite(data.table(id_series = invalid_age_ids_before), invalid_age_path)

# Save invalid birthdate rows to CSV
invalid_bdate_path <- here("data-cleaning", "debug", "invalid_bdate.csv")
fwrite(data.table(id_series = invalid_bdate_ids_before), invalid_bdate_path)

# Print messages for invalid ages corrected
invalid_ages_after_correction <- result[pat_age < 0 | pat_age > 124, .N]
message(
  "Number of invalid ages corrected: ", invalid_ages_before_correction - invalid_ages_after_correction,
  ". Invalid ages are those with a value less than 0 or greater than 124, which were reset to NA or corrected."
)

# Print messages for invalid birthdates corrected
invalid_bdate_after <- result[is.na(pat_bdate), .N]
message(
  "Number of invalid birthdates corrected: ", invalid_bdate_before - invalid_bdate_after,
  ". Invalid birthdates are missing values (NA), which were corrected based on age and admission dates."
)

result[, pat_ageday := NA_integer_]

# Process pat_ageday for patients younger than 1 year
invalid_ageday_before <- result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & is.na(pat_ageday), .N]
invalid_ageday_ids_before <- result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & is.na(pat_ageday), id_series]

result[
  !is.na(pat_age) & pat_age >= 0 & pat_age < 1 & !is.na(date_adm) & !is.na(pat_bdate),
  pat_ageday := as.integer(difftime(date_adm, pat_bdate, units = "days"))
]

if ("ageday" %in% colnames(result)) {
  result[, ageday := NULL]
}
gc()
# Print messages for invalid ageday corrections
invalid_ageday_after <- result[!is.na(pat_age) & pat_age >= 0 & pat_age < 1 & is.na(pat_ageday), .N]
message(
  "Number of agedays generated: ", invalid_ageday_before - invalid_ageday_after,
  ". Agedays generated are for where the patient is younger than 1 year, so the exact number of days was generated."
)

# Assuming acc_icd_env is an environment containing acc_icd codes
acc_icd_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_icd) {
  assign(code, TRUE, envir = acc_icd_env)
}

# Modify the data.table operation to use mget with the acc_icd_env
result[, clin_sdx := lapply(clin_sdx, function(row) {
  codes <- unlist(row)
  valid_codes <- codes[!is.na(mget(codes, envir = acc_icd_env, ifnotfound = NA))]
  if (length(valid_codes) > 0) {
    return(valid_codes)
  } else {
    return(NA_character_)
  }
})]

# Optionally unlist each element of clin_sdx
result[, clin_sdx := lapply(clin_sdx, unlist)]

na_replaced_result <- replace_empty_with_na(result)

result <- na_replaced_result$return_data

# Process character columns and convert to UTF-8
if (is_unix) {
  result[, (char_cols) := mclapply(.SD, function(col) iconv(col, from = "", to = "UTF-8"), mc.cores = parallel::detectCores()), .SDcols = char_cols]
} else {
  result[, (char_cols) := lapply(.SD, function(col) iconv(col, from = "", to = "UTF-8")), .SDcols = char_cols]
}

# Processing function for further data cleaning
process_data <- function(data) {
  # Process character columns
  char_cols <- names(data)[sapply(data, is.character)]
  if (is_unix) {
    data[, (char_cols) := mclapply(.SD, function(col) {
      col[col %in% c("None", "")] <- NA_character_
      return(col) # Return modified element
    }, mc.cores = parallel::detectCores()), .SDcols = char_cols]
  } else {
    data[, (char_cols) := lapply(.SD, function(col) {
      col[col %in% c("None", "")] <- NA_character_
      return(col) # Return modified element
    }), .SDcols = char_cols]
  }

  # Process numeric columns
  num_cols <- names(data)[sapply(data, is.numeric)]
  if (is_unix) {
    data[, (num_cols) := mclapply(.SD, function(col) {
      col[is.nan(col)] <- NA_real_
      return(col) # Return modified element
    }, mc.cores = parallel::detectCores()), .SDcols = num_cols]
  } else {
    data[, (num_cols) := lapply(.SD, function(col) {
      col[is.nan(col)] <- NA_real_
      return(col) # Return modified element
    }), .SDcols = num_cols]
  }

  # Process list columns, ensuring handling of character elements within lists
  list_cols <- names(data)[sapply(data, is.list)]
  if (is_unix) {
    data[, (list_cols) := mclapply(.SD, function(col) {
      lapply(col, function(x) {
        if (is.character(x)) x[x %in% c("None", "")] <- NA_character_ # Replace "None" and empty strings with NA
        return(x) # Return modified list element
      })
    }, mc.cores = parallel::detectCores()), .SDcols = list_cols]
  } else {
    data[, (list_cols) := lapply(.SD, function(col) {
      lapply(col, function(x) {
        if (is.character(x)) x[x %in% c("None", "")] <- NA_character_
        return(x) # Return modified list element
      })
    }), .SDcols = list_cols]
  }
  return(data)
}
result <- process_data(result)

# Convert string columns to arrays
array_columns <- c("id_hcp")
if (is_unix) {
  result[, (array_columns) := mclapply(.SD, function(x) {
    x <- strsplit(x, "\\|\\|")
    lapply(x, function(y) if (length(y) == 0L || all(is.na(y))) character(0) else y)
  }, mc.cores = parallel::detectCores()), .SDcols = array_columns]
} else {
  result[, (array_columns) := lapply(.SD, function(x) {
    x <- strsplit(x, "\\|\\|")
    lapply(x, function(y) if (length(y) == 0L || all(is.na(y))) character(0) else y)
  }), .SDcols = array_columns]
}

# Ensure 'clin_sdx', 'clin_proc', and 'id_hcp' are not NULL
list_columns <- c("clin_sdx", "clin_proc", "id_hcp")
if (is_unix) {
  result[, (list_columns) := mclapply(.SD, function(col) {
    lapply(col, function(x) if (is.null(x) || length(x) == 0L || all(is.na(x))) character(0) else x)
  }, mc.cores = parallel::detectCores()), .SDcols = list_columns]
} else {
  result[, (list_columns) := lapply(.SD, function(col) {
    lapply(col, function(x) if (is.null(x) || length(x) == 0L || all(is.na(x))) character(0) else x)
  }), .SDcols = list_columns]
}

setnames(result, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))

setcolorder(result, c(
  "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm", "time_adm", "date_dis", "time_dis",
  "date_rec", "date_ref", "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
  "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent", "pat_memcat_child", "claim_status", "claim_payout",
  "claim_charge", "clin_discharge", "clin_outpatient", "clin_emergency", "clin_acc", "clin_c1", "clin_c2",
  "clin_sdx", "clin_proc", "clin_pdx", "clin_pdx_source"
))

# Apply format_id function to each column in parallel or sequentially
columns_to_format <- c("id_series", "id_pin", "id_hci")

# Define the format_id function
format_id <- function(x) {
  x <- as.character(x)
  integer_x <- suppressWarnings(as.integer(x))
  x <- trimws(formatC(integer_x, format = "f", digits = 0))
  x[x == "NA" | is.na(integer_x)] <- NA_character_
  x
}

# Apply format_id to each column safely
if (is_unix) {
  # Make a copy of the columns to avoid directly accessing the data.table object in parallel
  formatted_cols <- mclapply(columns_to_format, function(col) {
    column_data <- result[[col]] # Extract column data outside the parallel loop
    return(format_id(column_data))
  }, mc.cores = parallel::detectCores())
} else {
  formatted_cols <- lapply(columns_to_format, function(col) {
    column_data <- result[[col]] # Extract column data
    return(format_id(column_data))
  })
}

# Assign the formatted results back to the respective columns
for (i in seq_along(columns_to_format)) {
  result[[columns_to_format[i]]] <- formatted_cols[[i]]
}

saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")), compress = FALSE)


# str(result_after_cleaning)
print(result[grepl("e", id_series)])
print(result[grepl("e", id_pin)])
print(result[grepl("e", id_hci)])


# str(result)
# fwrite(result, "test.csv")
# Search for rows where any element in clin_sdx is "A"
result[, if (any(sapply(clin_sdx, function(row) "A" %in% row))) print(.SD), by = 1:nrow(result)]


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
      if (grepl("Not found", e, ignore.case = TRUE)) {
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
      fields = fromJSON(here("data-cleaning/r_scripts", "bq_schema_cleaning.json"), simplifyDataFrame = FALSE)
    )
    skip_bq_upload <<- FALSE
    message("Table created successfully.\n")
  },
  error = function(e) {
    # Check if the error message indicates that the table already exists
    if (grepl("already exists", e, ignore.case = TRUE)) {
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
      if (grepl("already exists", e, ignore.case = TRUE)) {
        # Handle the specific "already exists" error
        message("Upload skipped: table already exists and is not empty.")
      } else {
        # Handle all other errors
        message("Error during upload: ", e)
      }
    }
  )
}


str(result)


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

