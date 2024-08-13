# %load_ext rpy2.ipython
gc()


source(here::here("data-cleaning/r_scripts", "00_libraries-params.R"))


# use_python("~/drg-pipeline/data-cleaning/data/python-venv/bin/python", required = TRUE)
# use_virtualenv("~/drg-pipeline/data-cleaning/data/python-venv", required = TRUE)
py_install("numpy")
py_install("pandas")
py_install("streamlit")
py_install("python_dateutil")
py_install("tabulate")
py_install("swifter")
py_install("rpy2")


# IMPORTANT PARAMETERS:
full_claims_prefix <- "claims_extract_CLAIMS "
full_claims_bq_prefix <- "claims_extract_CLAIMS\\ "
year_to_load <- "2018" # Which claims year to load # TODO: maybe add a script that loops through all claims?
split_parts <- 15 # How many (integer) parts to split the 12+m row claims file into # TODO: a value of 10 for claims year 2018 leads to quoted newline errors
end_nrow <- 10 # How many rows/entries to show in summary tables
is_unix <- if (.Platform$OS.type == "unix") TRUE else FALSE # Detect operating system architecture
gcp_proj <- if (is_unix) system("gcloud config get-value project", intern = TRUE) else NULL
max_bq_rows <- 15000 # Max rows to return for bq query
cat(paste("GCP Project:", gcp_proj, "\n"))
encode <- "unknown" # Choices: unknown, UTF-8, Latin-1
sep <- "," # Choices: "," or "\t"

# Input:
to_sample <- TRUE # Whether to sample each split_part by sample_size_divisor (useful when iterating through code runs in quick succession)
sample_size_divisor <- 125 # Sample size divisor: Formula for sample size is total_rows / split_parts / sample_size_divisor. Choose between 5, 25, 125, and 625

# File Path Prefixes:
clean_prefix <- "data-cleaning"
data_prefix <- file.path(clean_prefix, "data")
claims_prefix <- file.path(data_prefix, "claims")

# File Paths:
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
cleaned_claims_path <- file.path(claims_prefix, "cleaned")
raw_claims_path <- file.path(claims_prefix, "raw")
raw_claims_parts_path <- file.path(claims_prefix, "raw", "parts")
raw_claims_samples_path <- file.path(claims_prefix, "raw", "samples")
profvis_path <- file.path(data_prefix, "profvis")
profvis_fpath <- here("data-cleaning", "data", "profvis", "profvis.html")
debug_path <- file.path("data-cleaning", "debug")

# Create directories:
created_dirs <- c()
for (path in mget(ls(pattern = "_path$"), envir = .GlobalEnv)) {
  full_path <- here(path)
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    created_dirs <- c(created_dirs, full_path)
  }
}
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

# Manual Tweaks:
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b")
manual_code_replacements <- c("O800", "O80", "O809")

drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 13:14), # Start
  "ICCODED15", # note that ICDCODE15 is misspelled as ICCODED15 in all claims
  paste0("ICDCODE", 16:170), # Continuation
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)

# Output:
to_write <- TRUE # Whether to write out checkpoint_1 files (everything up until converting for grouper export)
to_combine <- TRUE # Whether to combine checkpoint 1 files into one data.table
to_group <- TRUE # Whether to export for the batch grouper or not

# Debug:
to_debug <- FALSE # whether to print debug statements
to_profvis <- TRUE # Conduct runtime duration analysis via profvis or not
to_view_checks <- TRUE # Whether to view checks and print statements
to_view_checks_parallel <- FALSE # Whether to view intermediate per split_part/chunk checks and print statements (not consolidated) when parallelized
to_parallel <- TRUE # Whether to parallelize each split_parts split_part into availableCores() chunks. Cuts down processing time from 120min to 15min.
to_split_read <- FALSE # WARNING: TRUE uses a lot of memory!!
to_dec_mem_usage <- FALSE # Whether to run rm() and gc() at every possible step
tmp_nrow <- Inf # Per split_part/chunk end_nrow (leave at Inf)
diff_chars <- 0

# Flush files
master_flush_all <- FALSE # Whether to flush all files

to_flush_cache_and_profvis <- FALSE # Whether to cache
to_flush_aux_files <- FALSE # Whether to delete aux files to pull from BQ again
to_flush_checkpoints <- FALSE # Whether to delete checkpoints to free up space
to_flush_cleaned_parts_and_samples <- FALSE # Whether to delete parts and samples to free up space (WARNING: TAKES A WHILE TO REGENERATE)
to_flush_raw <- FALSE # Whether to delete raw claims files (WARNING: PULLING FROM GCS TAKES A WHILE AND COSTS MONEY)
to_flush_debug <- FALSE # Whether to delete everything folder (debug)


global_seed <- seed <- 123
# Seed for reproducibility (Important for stuff like randomly choosing a pdx among multiple possible options)
set.seed(seed) # Setting the seed
global_seed <- seed # global_seed for future_lapply parts for parallelized operations

ram_size <- 32 # Input virtual or physical machine's RAM size here
ram_buffer <- 0.1 # How much of a buffer to leave for the OS
ram_limit <- (1 - ram_buffer) * (ram_size) * (1024^3) # Compute ram_limit in bytes
# Allowing each future_lapply session to use more memory
options(future.globals.maxSize = ram_limit)

# Compute the RAM limit for R processes, leaving the buffer for the OS
ram_limit_gb <- round((1 - ram_buffer) * ram_size, 0)
# Set the environment variable R_FUTURE_MAX_RAM in GB
# Sys.setenv(R_FUTURE_MAX_RAM = paste0(ram_limit_gb, "G"))

# Print the set RAM limit
# cat("Setting R_FUTURE_MAX_RAM to:", ram_limit_gb, "GB\n")
cat(sprintf("Setting future.globals.maxSize to: %.1f GB", ram_limit / (1024^3)))


# Automatically set all "to_flush" variables to FALSE if to_flush_all is FALSE
if (!is.null(master_flush_all) && master_flush_all) for (var in ls(pattern = "^to_flush")) assign(var, TRUE)
if (!is.null(master_flush_all) && !master_flush_all) for (var in ls(pattern = "^to_flush")) assign(var, FALSE)

# Stop if forecasted memory usage is expected to crash the system
if (!split_parts == as.integer(split_parts) || split_parts <= 1) stop("ERROR: split_parts must be an integer greater than or equal to 2!")
if (ram_size <= 64 && split_parts <= 2) stop("Please set split_parts to at least 3 for 64 GB machines or it will likely crash")
if (ram_size <= 32 && split_parts <= 4) stop("Please set split_parts to at least 5 for 32 GB machines or it will likely crash")
if (ram_size <= 32 && to_split_read == TRUE) stop("Please set to_split_read to TRUE for 32 GB machines or it will likely crash")


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
  return(dt)
}


# 1. Query and load `grouper_v5.proc`
proc_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".grouper_v5.proc` LIMIT ", max_bq_rows
)
proc <- query_bq_to_dt(proc_query, here(aux_path, "proc.csv"),
  cache = TRUE, max_bq_rows = max_bq_rows
)
proc[, CODE := as.character(CODE)]

# 2. Query and load `phic.acr_rvs_map`
rvs_icd9_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".phic.acr_rvs_map` LIMIT ", max_bq_rows
)
rvs_icd9 <- query_bq_to_dt(rvs_icd9_query, here(aux_path, "rvs_icd9cm.csv"),
  cache = TRUE, max_bq_rows = max_bq_rows
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
  cache = TRUE, max_bq_rows = max_bq_rows
)

# 4. Query and load `grouper_v5.i10`
i10_query <- paste0(
  "SELECT * FROM `", gcp_proj,
  ".grouper_v5.i10` LIMIT ", max_bq_rows
)
tdrg_icd10 <- query_bq_to_dt(i10_query, here(aux_path, "i10.csv"),
  cache = TRUE, max_bq_rows = max_bq_rows
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
  cache = TRUE, max_bq_rows = max_bq_rows
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
  total_rows <- fread(file = full_claims_file, select = 1L, header = TRUE)[, .N]
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

suffix <- paste0(
  ifelse(to_sample, paste0("_sampled_", sample_size, "_"), "_full_")
)


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

  return(list(return_data = dt, return_summary = list(
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
  )))
}


map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)

  return(list(
    icd9_list = get_icd9_codes(clin_rvs, rvs_map_solo_env),
    rvs_map_list = rvs_maps$rvs_map_list,
    rvss = unique(unlist(clin_rvs)),
    mappable_rvs = intersect(unique(unlist(clin_rvs)), rvs_icd9$rvs),
    unmappable_rvs = setdiff(unique(unlist(clin_rvs)), rvs_icd9$rvs),
    multi_mapped_rvs = intersect(
      unique(unlist(clin_rvs)),
      names(rvs_maps$rvs_map_list)
    ),
    without_drg = unique(rvs_icd9[!rvs %in% names(rvs_maps$rvs_map_list)]$rvs)
  ))
}


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

  return(list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    direct_matches = direct_match_codes,
    unmatched = unmatched_icds,
    unmatched_sources = unmatched_sources
  ))
}


# Define a codeblock to avoid repeating it twice when
# to_profvis is TRUE and again if FALSE
# Makes it easier to maintain as well, since we only need
# to modify one section instead of two
unified_block <- function() {
  # Start main execution logic
  # split_and_save_parts() # Read, split, and save partial files

  ####################################################################################################################################
  ################################################## START OF SPLIT AND SAVE PART ####################################################
  ####################################################################################################################################

  full_header <<- fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

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
      fwrite(chunk_dt, chunk_file, quote = TRUE)
      if (to_dec_mem_usage) rm(chunk_dt)
      if (to_dec_mem_usage) gc()
    }
  }
  lapply(1:split_parts, split_and_save)

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

    ##################################################################################################################################
    ##################################################### START OF PROCESS CHUNK #####################################################
    ##################################################################################################################################

    process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
      if (to_view_checks) {
        # cat("\rViewing checks")
      } else {
        sink(tempfile())
        on.exit(sink(), add = TRUE)
      }

      clean_result <- clean_data(chunk)
      chunk <- clean_result$return_data
      chunk_summary <- clean_result$return_summary

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
      chunk_summary$empty_strings_replaced_2 <- res2$return_replacement_summary

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
        return(x)
      })
      chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
        length(x) <- max_len
        return(x)
      })

      chunk_summary$unique_icds <- icd10_mapping_result$unique_icds
      chunk_summary$direct_matches <- icd10_mapping_result$direct_matches
      chunk_summary$unmatched <- icd10_mapping_result$unmatched
      chunk_summary$unmatched_sources <- icd10_mapping_result$unmatched_sources
      chunk_summary$icd10_map_dt <- icd10_mapping_result$icd10_map_dt
      chunk_summary$rvss <- rvs_mapping_result$rvss
      chunk_summary$mappable_rvs <- rvs_mapping_result$mappable_rvs
      chunk_summary$unmappable_rvs <- rvs_mapping_result$unmappable_rvs
      chunk_summary$multi_mapped_rvs <- rvs_mapping_result$multi_mapped_rvs
      chunk_summary$without_drg <- rvs_mapping_result$without_drg

      if (to_dec_mem_usage) gc() # debug
      return(
        list(
          return_chunk = chunk,
          return_summary = chunk_summary
        )
      )
    }

    ###################################################################################################################################
    ##################################################### END OF PROCESS CHUNK ########################################################
    ###################################################################################################################################

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
          "checkpoint_1_claims_", year_to_load, suffix,
          "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".csv"
        ))
      )
    }
    if (to_combine) master_dt_list[[loop_part]] <- summarized_dt
    if (to_group) {
      export_for_grouper(
        summarized_dt, year_to_load,
        here(checkpoint_3_path, paste0(
          "DRG_Grouped", "_", year_to_load, suffix, "part_",
          sprintf("%02d", loop_part), "_of_", split_parts, ".txt"
        )), loop_part
      )
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
    print_status_update(loop_part, split_parts, processing_times)
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
    master_grouper_input_dt <- rbindlist(master_grouper_input_list)
    master_grouper_input_dt[, CASEID := 1:nrow(master_grouper_input_dt)]
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
      rm(master_dt_list, master_grouper_input_list)
      gc()
    }
    if (to_write) {
      fwrite(master_dt, here(checkpoint_2_path, paste0(
        "checkpoint_2_claims_", year_to_load, suffix, ".csv"
      )))
      fwrite(master_grouper_input_dt,
        here(checkpoint_4_path, paste0(
          "checkpoint_4_thai_grouper_input_",
          year_to_load, suffix, ".txt"
        )),
        sep = "|", col.names = TRUE
      )
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
    return(master_dt)
  } # debug
}


all_parts_summaries <- list() # initialize list for summaries
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
nthreads <- parallelly::availableCores() # detect available threads
cat(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
master_dt <- data.table()
master_dt_list <- list()
master_grouper_input_dt <- data.table()
master_grouper_input_list <- list()

# Call the main function with or without profvis
if (to_profvis) {
  saveWidget(profvis({
    unified_block()
  }), profvis_fpath)
} else {
  unified_block()
}


# Please run thai grouper via drg-merge-and-upload.ipynb
claims_fpath <- here(checkpoint_2_path, "checkpoint_2_claims_2018_sampled_6282_.csv")
grouper_fpath <- here(checkpoint_5_path, "CHECKPOINT_4_THAI_GROUPER_INPUT_2018_SAMPLED_6282_Res.TXT")
merged_fpath <- here(checkpoint_6_path, "checkpoint_6_grouped_claims.csv")

claims <- fread(claims_fpath, colClasses = "character")
claims[, caseid := 1:.N]
grouper <- fread(grouper_fpath, sep = "|", na.strings = "--", header = TRUE)
result <- merge(claims, grouper, by = "caseid", all.x = TRUE)
result[, drgname := NULL]

# Convert data types to match BigQuery schema
result[, caseid := as.integer(caseid)]
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
result[, drg := as.character(drg)]
result[, rw := as.numeric(rw)]
result[, wtlos := as.numeric(wtlos)]
result[, ot := as.integer(ot)]
result[, adjrw := as.numeric(adjrw)]
result[, err := as.integer(err)]
result[, warn := as.integer(warn)]
result[, los := as.integer(los)]

fwrite(result, merged_fpath) # Write to file for python grouper before strsplit

# result[, id_hcp := strsplit(id_hcp, "\\|\\|")]
# result[, clin_c1 := strsplit(clin_c1, "\\|\\|")]
# result[, clin_c2 := strsplit(clin_c2, "\\|\\|")]
# result[, clin_icd := strsplit(clin_icd, "\\|\\|")]
# result[, clin_rvs := strsplit(clin_rvs, "\\|\\|")]
# result[, icd9_list := strsplit(icd9_list, "\\|\\|")]


pandas <- import("pandas")

# Step 1:
# Copy the master_dt to avoid modifying the original data
python_input_dt <- fread(here(checkpoint_6_path, "checkpoint_6_grouped_claims.csv"))

# Convert columns to Date objects, ignoring NA values
python_input_dt[, pat_bdate_orig := as.Date(pat_bdate, format = "%m/%d/%Y")]
python_input_dt[, pat_bdate := as.Date(pat_bdate, format = "%m/%d/%Y")]
python_input_dt[, date_adm := as.Date(date_adm, format = "%m/%d/%Y")]
python_input_dt[, date_dis := as.Date(date_dis, format = "%m/%d/%Y")]
python_input_dt[, date_rec := as.Date(date_rec, format = "%m/%d/%Y")]
python_input_dt[, date_ref := as.Date(date_ref, format = "%m/%d/%Y")]
python_input_dt[, date_ext := as.Date(date_ext, format = "%m/%d/%Y")]
python_input_dt[, date_check := as.Date(date_check, format = "%m/%d/%Y")]

# Create a data.table for rows with negative `pat_age`
negative_age_dt <- python_input_dt[pat_age < 0, ]

# Create a data.table for rows with non-negative `pat_age`
positive_age_dt <- python_input_dt[pat_age >= 0, ]

# Process rows with non-negative `pat_age`
positive_age_dt[, pat_bdate := dmy(generate_dob(format(pat_bdate_orig, "%m/%d/%Y"), pat_age, format(date_adm, "%m/%d/%Y")))]

# For negative `pat_age`, keep `pat_bdate` as it is
negative_age_dt[, pat_bdate := pat_bdate_orig]

# Combine the processed data
result_dt <- rbindlist(list(positive_age_dt, negative_age_dt))

fwrite(result_dt, here(checkpoint_7_path, "python_input.csv"))

# Convert the `data.table` (result_dt) to a pandas DataFrame
pandas_df <- pandas$read_csv(here(checkpoint_7_path, "python_input.csv"))
# pandas_df <- pandas$DataFrame(as.data.frame(result_dt)) # Doesn't work for some reason
# print(head(pandas_df))
py$pandas_df <- pandas_df

# Step 3: Convert `clin_icd` and `icd9_list` columns, replace NaN with "None"
py_run_string("
import pandas as pd
import numpy as np

# Create sdx and proc columns from clin_icd and icd9_list
def split_codes(df, column, prefix, max_cols):
    split_cols = df[column].str.split(r'\\|\\|', expand=True).iloc[:, :max_cols]
    split_cols.columns = [f'{prefix}{i+1}' for i in range(split_cols.shape[1])]
    split_cols = split_cols.reindex(columns=[f'{prefix}{i+1}' for i in range(max_cols)], fill_value=np.nan)
    return split_cols

# Fixed columns to keep
fixed_columns = [
    'pat_age', 'pat_sex', 'date_adm', 'date_dis',
    'pdx', 'clin_discharge', 'pat_bwt',
    'pat_bdate', 'pat_bdate_orig',  # Add pat_bdate and pat_bdate_orig
    # Add the new columns below
    'id_series', 'id_pin', 'time_adm', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci',
    'id_hcp', 'clin_outpatient', 'clin_emergency',
    'pat_type', 'clin_acc', 'pat_rel', 'pat_memcat_parent',
    'pat_memcat_child', 'clin_c1', 'clin_c2', 'claim_status',
    'claim_payout', 'claim_charge', 'date_ext', 'id_year',
    'clin_icd', 'clin_rvs', 'clin_c1_orig', 'clin_c2_orig',
    'icd9_list', 'pdx_code', 'drg', 'rw', 'wtlos', 'ot',
    'adjrw', 'err', 'warn', 'los'
]

# Assuming `pandas_df` is already defined in the environment
# Create sdx and proc columns
sdx_columns = split_codes(pandas_df, 'clin_icd', 'sdx', 12)
proc_columns = split_codes(pandas_df, 'icd9_list', 'proc', 20)

# Combine all columns into the final DataFrame
subset_df = pd.concat([pandas_df[fixed_columns], sdx_columns, proc_columns], axis=1)

# Rename 'clin_discharge' to 'discharge'
subset_df.rename(columns={'clin_discharge': 'discharge',
                          'pat_bwt': 'birthweight',
                          'pat_age': 'patage',
                          'pat_sex': 'patsex'}, inplace=True)

# Ensure 'None' is a category in all Categorical columns
for col in subset_df.select_dtypes(include=['category']).columns:
    subset_df[col] = subset_df[col].cat.add_categories(['None'])

# Replace NaN and NA with 'None'
subset_df = subset_df.fillna('None')

# Reorder columns to match desired output
# Columns you want to come first
priority_columns = [
    'id_series', 'id_pin', 'date_adm', 'time_adm', 'date_dis', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci', 'id_hcp', 'clin_outpatient',
    'clin_emergency', 'pat_type', 'clin_acc', 'pat_rel', 'pat_bdate', 'pat_age',
    'pat_sex', 'pat_bwt', 'pat_memcat_parent', 'pat_memcat_child',
    'clin_discharge', 'clin_c1', 'clin_c2', 'claim_status', 'claim_payout',
    'claim_charge', 'date_ext', 'id_year', 'clin_icd', 'clin_rvs',
    'clin_c1_orig', 'clin_c2_orig', 'icd9_list', 'pdx', 'pdx_code', 'drg',
    'rw', 'wtlos', 'ot', 'adjrw', 'err', 'warn', 'los'
]

# Remaining columns to follow the priority columns
remaining_columns = [
    'patage', 'patsex', 'date_adm', 'date_dis', 'pdx',
    'sdx1', 'sdx2', 'sdx3', 'sdx4', 'sdx5', 'sdx6',
    'sdx7', 'sdx8', 'sdx9', 'sdx10', 'sdx11', 'sdx12',
    'proc1', 'proc2', 'proc3', 'proc4', 'proc5', 'proc6',
    'proc7', 'proc8', 'proc9', 'proc10', 'proc11', 'proc12',
    'proc13', 'proc14', 'proc15', 'proc16', 'proc17', 'proc18',
    'proc19', 'proc20', 'discharge', 'birthweight', 'ageday'
]

# Combine the lists, ensuring no duplicates
desired_columns = priority_columns + [col for col in remaining_columns if col not in priority_columns]

# Add missing columns with None values if they are not already in the DataFrame
for col in desired_columns:
    if col not in subset_df.columns:
        subset_df[col] = 'None'

# Store the result in output
output = subset_df.rename(columns = {'drg':'thai_drg'})
")

# Retrieve the processed DataFrame back to R
subset_df <- py$output

# Debugging: Print a few rows to check date formats
# print(head(subset_df))

# Step 1: Ensure date columns are properly converted to Date objects
subset_df$date_adm <- as.Date(subset_df$date_adm, format = "%Y-%m-%d")
subset_df$pat_bdate <- as.Date(subset_df$pat_bdate, format = "%Y-%m-%d")

# Step 2: Initialize the 'ageday' column with NA
subset_df$ageday <- NA

# Step 3: Calculate 'ageday' only for valid rows where pat_age == 0
valid_rows <- subset_df$patage >= 0 & subset_df$patage %% 1 == 0

subset_df$ageday[valid_rows & subset_df$patage == 0] <- as.numeric(
  difftime(
    subset_df$date_adm[valid_rows & subset_df$patage == 0],
    subset_df$pat_bdate[valid_rows & subset_df$patage == 0],
    units = "days"
  )
)

# Step 4: Replace NA values in 'ageday' with "None"
subset_df$ageday[is.na(subset_df$ageday)] <- "None"

fwrite(as.data.table(subset_df), here(checkpoint_7_path, "python_input_2.csv"))
# Step 5: Convert the DataFrame to a format suitable for Python processing
py$pandas_df <- pandas$read_csv(here(checkpoint_7_path, "python_input_2.csv"))

# Step 6: Process each row of the DataFrame through `drg_seeker` and append results
py_run_string("
import pandas as pd
import numpy as np
import swifter
from grouper import seeker

# Initialize the necessary libraries
libs = seeker.Libraries()

# Apply the drg_seeker function directly to each row using swifter
pandas_df[['mdc', 'pdc', 'dc', 'pccl', 'drg']] = pandas_df.swifter.apply(
    lambda row: pd.Series(seeker.drg_seeker(row.to_dict(), libs)),
    axis=1
)

# Store the result in output to be retrieved by R
output = pandas_df.rename(columns={'drg': 'py_drg'})

# Define the desired column order
desired_columns = [
    'id_series', 'id_pin', 'date_adm', 'time_adm', 'date_dis', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci', 'id_hcp', 'clin_outpatient',
    'clin_emergency', 'pat_type', 'clin_acc', 'pat_rel', 'pat_bdate', 'patage',
    'patsex', 'birthweight', 'pat_memcat_parent', 'pat_memcat_child',
    'discharge', 'clin_c1', 'clin_c2', 'claim_status', 'claim_payout',
    'claim_charge', 'date_ext', 'id_year', 'clin_icd', 'icd9_list', 'pdx',
    'pdx_code', 'thai_drg', 'rw', 'wtlos', 'ot', 'adjrw', 'err', 'warn',
    'los', 'mdc', 'pdc', 'dc', 'pccl', 'py_drg'
]

# Reorder the DataFrame and drop any columns not in the desired list
output = output[desired_columns]

# Define the renaming mapping
rename_mapping = {
    'patage': 'pat_age',
    'patsex': 'pat_sex',
    'birthweight': 'pat_bwt',
    'discharge': 'clin_discharge',
    'icd9_list': 'clin_rvs'
}

# Rename the columns
output = output.rename(columns = rename_mapping)
")


result <- as.data.table(py$output)
# Convert data types to match BigQuery schema
# result[, caseid := as.integer(caseid)]
result[, id_series := as.character(id_series)]
result[, id_pin := as.character(id_pin)]
result[, date_adm := as.Date(date_adm)]
result[, time_adm := as.ITime(time_adm)]
result[, date_dis := as.Date(date_dis)]
result[, time_dis := as.ITime(time_dis)]
result[, date_rec := as.Date(date_rec)]
result[, date_ref := as.Date(date_ref)]
result[, date_check := as.Date(date_check)]
result[, id_hci := as.character(id_hci)]

# Convert character "0"/"1" to logical for Boolean fields
result[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
result[, clin_emergency := as.logical(as.integer(clin_emergency))]

result[, pat_type := as.character(pat_type)]
result[, clin_acc := as.character(clin_acc)]
result[, pat_rel := as.character(pat_rel)]
result[, pat_bdate := as.Date(pat_bdate)]
result[, pat_age := as.numeric(pat_age)]
result[, pat_sex := as.character(pat_sex)]
result[, pat_bwt := as.numeric(pat_bwt)]
result[, pat_memcat_parent := as.character(pat_memcat_parent)]
result[, pat_memcat_child := as.character(pat_memcat_child)]
result[, clin_discharge := as.integer(clin_discharge)]

result[, claim_status := as.character(claim_status)]
result[, claim_payout := as.numeric(claim_payout)]
result[, claim_charge := as.numeric(claim_charge)]
result[, date_ext := as.Date(date_ext)]
result[, id_year := as.integer(id_year)]

result[, pdx := as.character(pdx)]
result[, pdx_code := as.integer(pdx_code)]
result[, thai_drg := as.character(thai_drg)]
result[, rw := as.numeric(rw)]
result[, wtlos := as.numeric(wtlos)]
result[, ot := as.integer(ot)]
result[, adjrw := as.numeric(adjrw)]
result[, err := as.integer(err)]
result[, warn := as.integer(warn)]
result[, los := as.integer(los)]
result[, mdc := as.character(mdc)]
result[, pdc := as.character(pdc)]
result[, dc := as.character(dc)]
result[, pccl := as.numeric(pccl)]
result[, py_drg := as.character(py_drg)]

result[, id_hcp := strsplit(id_hcp, "\\|\\|")]
result[, clin_c1 := strsplit(clin_c1, "\\|\\|")]
result[, clin_c2 := strsplit(clin_c2, "\\|\\|")]
result[, clin_icd := strsplit(clin_icd, "\\|\\|")]
result[, clin_rvs := strsplit(clin_rvs, "\\|\\|")]

# Replace NULL (empty) arrays with an empty character vector, which is acceptable to BigQuery
replace_null_with_empty_array <- function(x) {
  lapply(x, function(y) if (length(y) == 0 || is.null(y) || all(is.na(y))) character(0) else y)
}

result[, id_hcp := replace_null_with_empty_array(id_hcp)]
result[, clin_c1 := replace_null_with_empty_array(clin_c1)]
result[, clin_c2 := replace_null_with_empty_array(clin_c2)]
result[, clin_icd := replace_null_with_empty_array(clin_icd)]
result[, clin_rvs := replace_null_with_empty_array(clin_rvs)]


project_id <- gcp_proj
dataset_id <- "phic"
table_id <- "temp_claims_latest"

# Define the schema using bq_field
table_schema <- list(
  bq_field("id_series", "STRING", mode = "NULLABLE"),
  bq_field("id_pin", "STRING", mode = "NULLABLE"),
  bq_field("date_adm", "DATE", mode = "NULLABLE"),
  bq_field("time_adm", "TIME", mode = "NULLABLE"),
  bq_field("date_dis", "DATE", mode = "NULLABLE"),
  bq_field("time_dis", "TIME", mode = "NULLABLE"),
  bq_field("date_rec", "DATE", mode = "NULLABLE"),
  bq_field("date_ref", "DATE", mode = "NULLABLE"),
  bq_field("date_check", "DATE", mode = "NULLABLE"),
  bq_field("id_hci", "STRING", mode = "NULLABLE"),
  bq_field("id_hcp", "STRING", mode = "REPEATED"), # Array of strings
  bq_field("clin_outpatient", "BOOL", mode = "NULLABLE"),
  bq_field("clin_emergency", "BOOL", mode = "NULLABLE"),
  bq_field("pat_type", "STRING", mode = "NULLABLE"),
  bq_field("clin_acc", "STRING", mode = "NULLABLE"),
  bq_field("pat_rel", "STRING", mode = "NULLABLE"),
  bq_field("pat_bdate", "DATE", mode = "NULLABLE"),
  bq_field("pat_age", "FLOAT64", mode = "NULLABLE"),
  bq_field("pat_sex", "STRING", mode = "NULLABLE"),
  bq_field("pat_bwt", "FLOAT64", mode = "NULLABLE"),
  bq_field("pat_memcat_parent", "STRING", mode = "NULLABLE"),
  bq_field("pat_memcat_child", "STRING", mode = "NULLABLE"),
  bq_field("clin_discharge", "INT64", mode = "NULLABLE"),
  bq_field("clin_c1", "STRING", mode = "REPEATED"), # Array of strings
  bq_field("clin_c2", "STRING", mode = "REPEATED"), # Array of strings
  bq_field("claim_status", "STRING", mode = "NULLABLE"),
  bq_field("claim_payout", "FLOAT64", mode = "NULLABLE"),
  bq_field("claim_charge", "FLOAT64", mode = "NULLABLE"),
  bq_field("date_ext", "DATE", mode = "NULLABLE"),
  bq_field("id_year", "INT64", mode = "NULLABLE"),
  bq_field("clin_icd", "STRING", mode = "REPEATED"), # Array of strings
  bq_field("clin_rvs", "STRING", mode = "REPEATED"), # Array of strings
  bq_field("pdx", "STRING", mode = "NULLABLE"),
  bq_field("pdx_code", "INT64", mode = "NULLABLE"),
  bq_field("thai_drg", "STRING", mode = "NULLABLE"),
  bq_field("rw", "FLOAT64", mode = "NULLABLE"),
  bq_field("wtlos", "FLOAT64", mode = "NULLABLE"),
  bq_field("ot", "INT64", mode = "NULLABLE"),
  bq_field("adjrw", "FLOAT64", mode = "NULLABLE"),
  bq_field("err", "INT64", mode = "NULLABLE"),
  bq_field("warn", "INT64", mode = "NULLABLE"),
  bq_field("los", "INT64", mode = "NULLABLE"),
  bq_field("mdc", "STRING", mode = "NULLABLE"),
  bq_field("pdc", "STRING", mode = "NULLABLE"),
  bq_field("dc", "STRING", mode = "NULLABLE"),
  bq_field("pccl", "FLOAT64", mode = "NULLABLE"),
  bq_field("py_drg", "STRING", mode = "NULLABLE")
)

tryCatch(
  {
    # Attempt to create the table
    bq_table_create(
      bq_table(project_id, dataset_id, table_id),
      fields = table_schema
    )
    cat("Table created successfully.\n")
  },
  error = function(e) {
    # Check if the error message indicates that the table already exists
    if (grepl("already exists", e$message, ignore.case = TRUE)) {
      cat("Table already exists. Skipping creation.\n")
    } else {
      # If it's a different error, re-throw the error
      stop(e)
    }
  }
)

bq_table_upload(
  bq_table(project_id, dataset_id, table_id),
  values = result,
  write_disposition = "WRITE_TRUNCATE" # Options: WRITE_TRUNCATE, WRITE_APPEND, WRITE_EMPTY
)


print_time_estimates() # Print time estimates along with estimate for full claims file


# in case we want to run this cell independently:
source(here::here("data-cleaning/r_scripts", "03_timing-debug-functions.R"))

# Consolidate all r_scripts scripts into debug.R; useful for debugging
concatenate_r_files(
  here::here("data-cleaning/r_scripts"),
  here::here("data-cleaning/debug/debug.R")
)

if (.Platform$OS.type == "unix") system("cd ~/drg-pipeline && jupyter nbconvert --no-prompt --to script data-cleaning/drg-cleaning.ipynb --output debug/drg-cleaning")
library(rmarkdown)
input <- "~/drg-pipeline/data-cleaning/drg-cleaning.ipynb"
convert_ipynb(input, output = xfun::with_ext(input, "Rmd"))


# Define the paths and corresponding conditions
paths <- list(
  to_flush_cleaned_parts_and_samples = c(
    "data-cleaning/data/claims/raw/cleaned",
    "data-cleaning/data/claims/raw/parts",
    "data-cleaning/data/claims/raw/samples"
  ),
  to_flush_cache_and_profvis = c(
    "data-cleaning/cache",
    "data-cleaning/data/profvis"
  ),
  to_flush_aux_files = "data-cleaning/data/aux-files",
  to_flush_checkpoints = "data-cleaning/data/checkpoints",
  to_flush_raw = "data-cleaning/data/claims/raw",
  to_flush_debug = "data-cleaning/debug"
)

# Iterate over the paths and conditions
for (condition in names(paths)) {
  if (get(condition)) {
    system(paste(
      "rm -r",
      paste(here(unlist(paths[[condition]])), collapse = " ")
    ))
  }
}

if (to_debug) {
  rm(list = ls())
  gc()
}

