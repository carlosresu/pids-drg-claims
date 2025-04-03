## Start total execution timer
tictoc::tic("Time spent (total)               ")

# detect available threads
nthreads <- parallelly::availableCores()
nthreads <- if (nthreads >= 16) nthreads - thread_offset else nthreads

dir.create(dirname(here::here("data-cleaning/debug/cache/year_to_load.txt")),
  recursive = TRUE, showWarnings = FALSE
)
if (!file.exists(here::here("data-cleaning/debug/cache/year_to_load.txt"))) {
  writeLines("2018", here::here("data-cleaning/debug/cache/year_to_load.txt"))
}
if (!exists("year_to_load")) {
  year_to_load <- data.table::fread(here::here("data-cleaning", "debug", "cache", "year_to_load.txt"),
    header = FALSE, colClasses = "character"
  )[[1]]
}
file_type <- if (year_to_load %in% c(2022:2023)) ".tsv" else ".csv"
separator <- if (file_type == ".tsv") "\t" else ","
# Whether to prompt for thai grouper even if bypassing all other prompts
thai_prompt <- TRUE
to_prompt <- FALSE

split_parts <- if (nthreads > 16) 30 else 15
# Sample size divisor: Formula for sample size is
# (total_rows ÷ split_parts) ÷ sample_size_divisor.
# Choose between 5, 25, 125, and 625
if (exists("sample_size_divisor")) {
  sample_size_divisor <- sample_size_divisor
} else {
  sample_size_divisor <- 5
}

# Columns to drop
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 21:170) # Continuation
)
drop_cols_manual <- c(
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)

# other parameters for manual adjustments
# ICD codes to replace
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b")
# ICD code replacements
manual_code_replacements <- c("O800", "O80", "O809")

# Control random behavior for reproducibility
# (Choose and set a number as seed)
# (Important for stuff like randomly choosing a
# clin_pdx among multiple possible options)
global_seed <- seed <- 123
set.seed(global_seed)

# Get current machine's nodename
current_node <- Sys.info()["nodename"]

# Define expected nodename for GCE instance
gce_node <- "pids-drg-data.us-central1-a.c.pids-drg-data.internal"

if (current_node == gce_node) {
  # If on the expected GCE VM, use the default service account
  gcs_email <- "10962838043-compute@developer.gserviceaccount.com"
  googleAuthR::gar_auth(email = gcs_email)
} else {
  # Not on GCE — use local service account file
  key_dir <- here::here("keys")
  key_file <- file.path(key_dir, "pids-drg-data-25ad1e4c7298.json")

  # Ensure the directory exists
  if (!dir.exists(key_dir)) {
    dir.create(key_dir, recursive = TRUE, showWarnings = FALSE)
    message(paste0("📁 Created key directory at: ", normalizePath(key_dir)))
  }

  # Prompt user to manually copy the JSON key file if it doesn't exist
  if (!file.exists(key_file)) {
    message("❌ Service account key not found.")
    message("👉 Please copy the key file named `pids-drg-data-25ad1e4c7298.json` to the following folder:\n")
    message(paste0("   ", normalizePath(key_dir)))
    message("\n⚠️ Ensure this key is stored securely and is only accessible to authorized users.\n")

    # Wait for user to confirm before continuing
    repeat {
      confirm <- readline("⏸️ Once you have copied the JSON key file to the above folder, enter 'y' to continue: ")
      if (tolower(confirm) == "y" && file.exists(key_file)) break
      if (tolower(confirm) == "y") {
        message("❌ File not found. Please ensure the file exists before continuing.")
      }
    }
  }

  # Authenticate using the key file
  googleAuthR::gar_auth_service(json_file = key_file)
  googleCloudStorageR::gcs_auth(json_file = key_file)
  message("✅ Authentication successful using service account key.")
}

# get current GCP Project
gcp_proj <- system("gcloud config get-value project", intern = TRUE)
# Name of GCS bucket
gcs_bucket <- "pids-drg-data"
# Name of folder path prefix in GCS bucket for thai grouper input
gcs_pre_fpath <- "data/phic/thai/pre"
# Name of folder path prefix in GCS bucket for thai grouper output
gcs_post_fpath <- "data/phic/thai/post"
# TODO: Add description here
gcs_spc_fpath <- "spc"
# bq dataset
bq_dataset <- "phic_claims"
# temp bq table, later renamed to claims_20XX1231 in Push to BQ section
bq_table <- paste0("temp_claims_", year_to_load)


# Folder Path Prefixes:
# Include spaces if there are any
full_claims_prefix <- "claims_extract_CLAIMS "
full_claims_bq_prefix <- stringr::str_replace_all(
  full_claims_prefix, " ", "\\\\ "
)
clean_prefix <- "data-cleaning"
data_prefix <- file.path(clean_prefix, "data")
claims_prefix <- file.path(data_prefix, "claims")
chkpt_1_prefix <- "chkpt_1_claims_"
chkpt_2_prefix <- "chkpt_2_claims_"
chkpt_3_prefix <- "DRG_Grouped_"
chkpt_4_prefix <- "chkpt_4_thai_grouper_input_"
chkpt_5_prefix <- chkpt_4_prefix
chkpt_6_prefix <- "chkpt_6_grouped_claims_"
chkpt_7a_prefix <- "python_input_1"
chkpt_7b_prefix <- "python_input_2"
chkpt_10_prefix <- "stata"
chkpt_11_prefix <- "bwt"
chkpt_12_prefix <- "map"

# Folder Paths:
filtered_path <- file.path(data_prefix, "filtered-claims")
filtered_chkpt_1_path <- file.path(filtered_path, "chkpt_1_partial")
filtered_chkpt_2_path <- file.path(filtered_path, "chkpt_2_master")
chkpt_path <- file.path(data_prefix, "chkpts")
chkpt_1_path <- file.path(chkpt_path, "chkpt_1_partial_clean_claims")
chkpt_2_path <- file.path(chkpt_path, "chkpt_2_master_clean_claims")
chkpt_3_path <- file.path(chkpt_path, "chkpt_3_thai_partial_input")
chkpt_4_path <- file.path(chkpt_path, "chkpt_4_thai_master_input")
chkpt_5_path <- file.path(chkpt_path, "chkpt_5_thai_master_output")
chkpt_6_path <- file.path(chkpt_path, "chkpt_6_thai_merged")
chkpt_7_path <- file.path(chkpt_path, "chkpt_7_py_input")
chkpt_8_path <- file.path(chkpt_path, "chkpt_8_py_output")
chkpt_9_path <- file.path(chkpt_path, "chkpt_9_grouper_differences")
chkpt_10_path <- file.path(chkpt_path, "chkpt_10_stata")
chkpt_11_path <- file.path(chkpt_path, "chkpt_11_bwt")
chkpt_12_path <- file.path(chkpt_path, "chkpt_12_mapping")
cache_path <- file.path(clean_prefix, "debug", "cache")
mapping_path <- file.path(cache_path, "mapping")
total_rows_path <- file.path(cache_path, "total_rows")
py_pkgs_path <- file.path(cache_path, "py_pkgs")
aux_path <- file.path(data_prefix, "aux-files")
raw_claims_path <- file.path(data_prefix, "raw-claims")
raw_claims_parts_path <- file.path(data_prefix, "partial-claims")
raw_claims_samples_path <- file.path(data_prefix, "sampled-claims")
raw_claims_md5_path <- file.path(data_prefix, "md5")
profvis_path <- file.path(data_prefix, "profvis")
debug_path <- file.path(clean_prefix, "debug")

# File Paths
profvis_fpath <- here::here("data-cleaning", "data", "profvis", "profvis.html")

# Create directories:
created_dirs <- c() # Initialize empty vector
# For all "_path" variables, create a directory with that path
# Create directories for variables ending in "_path"
# that are directories, not files
for (path in mget(ls(pattern = "_path$"), envir = .GlobalEnv)) {
  full_path <- here::here(path)
  # Check if the path is a directory and does not end
  # with a specific file extension
  if (!dir.exists(full_path) && !grepl("\\.rds$|\\.csv$|\\.tsv$", full_path)) {
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
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
full_claims_file <- here::here(
  raw_claims_path,
  paste0(full_claims_prefix, year_to_load, file_type)
)
# Use the file_type variable here

ram_limit <- (1 - 0.10) * 64 * (1024^3)

# Allowing each future_lapply session to use more memory
options(future.globals.maxSize = ram_limit)

total_rows_file <- here::here(
  cache_path, "total_rows",
  paste0("total_rows_", year_to_load, ".rds")
)

# Load cached total rows file if available, saves ~10 seconds of runtime
if (file.exists(total_rows_file)) {
  total_rows <- readRDS(total_rows_file)
  message(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- data.table::fread(
    file = full_claims_file,
    select = 1L,
    header = TRUE,
    colClasses = "character"
  )[, .N]
  saveRDS(total_rows, file = total_rows_file)
  message(paste("Total Rows via fread:", total_rows))
}

sample_size <- ceiling(total_rows / split_parts / sample_size_divisor)

suffix <- paste0(ifelse(to_sample, paste0(
  "_sampled_",
  sample_size_divisor, "_"
), "_full_"))

## NA-like strings
na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", '"', "'", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

# Mapping of column names (including clin_icd and clin_rvs)
column_mappings <- list(
  # Admission and discharge information
  "ADMISSION_YEAR" = "id_year",
  "ADMISSION_DATE" = "date_adm",
  "DATE_ADM" = "date_adm",
  "ADMISSION_TIME" = "time_adm",
  "TIME_ADM" = "time_adm",
  "DISCHARGE_DATE" = "date_dis",
  "DATE_DIS" = "date_dis",
  "DISCHARGE_TIME" = "time_dis",
  "TIME_DIS" = "time_dis",

  # Claim and patient identifiers
  "CLAIM_SERIES_ID" = "id_series",
  "PSEUDO_CLAIMSERIES" = "id_series",
  "PIN" = "id_pin",
  "PSEUDO_MEM_PIN" = "id_pin",

  # Date information
  "RECEIVE_DATE" = "date_rec",
  "DATE_REC" = "date_rec",
  "REFILE_DATE" = "date_ref",
  "DATE_REF" = "date_ref",
  "CHECK_DATE" = "date_check",
  "CHKDT" = "date_check",
  "EXTRACTION_DATE" = "date_ext",

  # Health care provider and institution
  "HCI_PMCC_NO" = "id_hci",
  "HCP_NO_LIST" = "id_hcp",

  # Patient information
  "PATIENT_TYPE" = "pat_type",
  "PATIENT_RELATIONSHIP" = "pat_rel",
  "DEP_REL" = "pat_rel",
  "PATIENT_SEX" = "pat_sex",
  "PATSEX" = "pat_sex",
  "PATIENT_AGE" = "pat_age",
  "PATAGE" = "pat_age",
  "PAT_BDAY" = "pat_bdate",
  "PAT_BWT_KG" = "pat_bwt",
  "MEMCAT_PARENT_DESC" = "pat_memcat_parent",
  "MEMCAT_CHILD_DESC" = "pat_memcat_child",

  # Clinical information
  "IS_ADMISSION_OPD" = "clin_outpatient",
  "IS_EMERGENCY_CASE" = "clin_emergency",
  "OUT_PATIENT" = "clin_outpatient",
  "EMERGENCY" = "clin_emergency",
  "ROOM_TYPE" = "clin_acc",
  "PATIENT_DISPOSITION" = "clin_discharge",
  "DISPOSITION" = "clin_discharge",
  "PRIMARY_ILLNESS" = "clin_c1",
  "SECONDARY_ILLNESS" = "clin_c2",
  # We'll dynamically handle clin_icd and clin_rvs
  "ICDCODES_ITEM7" = "clin_icd1",
  "RVSCODES_ITEM7" = "clin_rvs1",

  # Claim status and amounts
  "CLAIM_STATUS" = "claim_status",
  "CLAIMS_STATUS" = "claim_status",
  "CLAIM_PAID_AMOUNT" = "claim_payout",
  "CLAIMS_PAID_AMT" = "claim_payout",
  "CLAIM_AMOUNT_ACTUAL" = "claim_charge",
  "ACR_AMOUNT_ACTUAL" = "claim_charge"
)

for (i in 1:20) {
  column_to_map <- if (i == 15) "ICCODED15" else paste0("ICDCODE", i)
  column_mappings[[column_to_map]] <- paste0("clin_icd", i)
}
for (i in 1:20) {
  column_mappings[[paste0("RVSCODE", i)]] <- paste0("clin_rvs", i)
}

expected_types <- list(
  "character" = c(
    # Identifiers, date, and time strings (dates are kept as char)
    "id_series", "id_pin", "id_hci", "id_hcp",
    "date_adm", "time_adm", "date_dis", "time_dis",
    "date_rec", "date_ref", "date_check", "date_ext",
    "pat_bdate",
    # Patient and clinical text fields
    "pat_type", "pat_rel", "pat_sex", "pat_memcat_parent", "pat_memcat_child",
    "claim_status", "clin_pdx", "clin_c1", "clin_c2",
    "c1", "c2", "clin_sdx", "clin_proc", "clin_rvs",
    # Dynamically generated ICD and RVS columns
    paste0("clin_icd", 1:20), paste0("clin_rvs", 1:20),
    # Other clinical information
    "clin_acc"
  ),
  "integer" = c(
    "id_year", "clin_pdx_source", "pat_ageday"
  ),
  "numeric" = c(
    "pat_age", "pat_bwt", "claim_payout", "claim_charge"
  ),
  "factor" = c(
    "pat_type", "pat_rel", "pat_memcat_parent", "pat_memcat_child",
    "claim_status", "clin_discharge"
  ),
  "logical" = c(
    "clin_outpatient", "clin_emergency"
  )
)



## COVID codes (for exclusion later)
covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4",
  "C191P1", "C191P2", "C191P3", "C191P4",
  "C19PP1", "C19PP2", "C19PP3", "C19PP4",
  "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID", "C19AT1", "C19HI"
)

## Convert the COVID codes into a regular expression pattern (without word boundaries)
covid_rvs_pattern <- paste(covid_rvs, collapse = "|")

# Initialize known values for each type of remapping
# Define known values for each column
known_values <- list(
  pat_type = c("MEMBER", "MM", "DEPENDENT", "DD"),
  claim_status = c("DENIED", "IN-PROCESS", "PAID", "RTH", "APRV4PAYMENT"),
  pat_memcat_parent = c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR"),
  pat_memcat_child = c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER", "FORMAL ECONOMY",
    "DIRECT CONTRIBUTOR", "PROFESSIONAL PRACTITIONER"
  ),
  clin_discharge = c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE",
    "ABSCONDED", "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED",
    "I", "R", "H", "A", "T", "E"
  )
)

# Define the fcase logic for remapping the membership categories
col_remap_master <- quote(fcase(
  dt[[column_name]] %in% c("MEMBER", "MM"), "M",
  dt[[column_name]] %in% c("DEPENDENT", "DD"), "D",
  dt[[column_name]] == "DENIED", "D",
  dt[[column_name]] == "IN-PROCESS", "I",
  dt[[column_name]] %in% c("PAID", "APRV4PAYMENT"), "G",
  dt[[column_name]] == "RTH", "R",
  dt[[column_name]] == "DIRECT CONTRIBUTOR", "D", # DIRECT
  dt[[column_name]] == "INDIRECT CONTRIBUTOR", "I", # INDIRECT
  dt[[column_name]] %in% c(
    "EMPLOYED PRIVATE", "EMPLOYED GOVERNMENT", "HOUSEHOLD HELP/KASAMBAHAY",
    "FAMILY DRIVER", "FORMAL ECONOMY"
  ), "1", # Formal
  dt[[column_name]] %in% c(
    "SELF-EARNING INDIVIDUAL", "SELF EARNING INDIVIDUAL", "INFORMAL ECONOMY",
    "MIGRANT WORKER", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "PROFESSIONAL PRACTITIONER"
  ), "2", # Informal
  dt[[column_name]] == "LIFETIME MEMBER", "3", # Lifetime
  dt[[column_name]] == "INDIGENT", "4", # Indigent
  dt[[column_name]] == "SPONSORED", "5", # Sponsored
  dt[[column_name]] == "SENIOR CITIZEN", "6", # Senior Citizen
  dt[[column_name]] %in% c("IMPROVED", "RECOVERED", "I", "R"), "1",
  dt[[column_name]] %in% c("HOME/DISCHARGED AGAINST MEDICAL ADVICE", "H"), "2",
  dt[[column_name]] %in% c("ABSCONDED", "A"), "3",
  dt[[column_name]] %in% c("TRANSFERRED/REFERRED", "T"), "4",
  dt[[column_name]] %in% c("EXPIRED", "E"), "9",
  dt[[column_name]] == "UNDEFINED", NA_character_
))

bq_cols <- c(
  "id_series",
  "id_pin",
  "id_hci",
  "id_hcp",
  "date_adm",
  "date_dis",
  "date_rec",
  "date_ref",
  "date_check",
  "pat_type",
  "pat_rel",
  "pat_age",
  "pat_ageday",
  "pat_sex",
  "pat_bwt",
  "pat_memcat_parent",
  "pat_memcat_child",
  "claim_status",
  "claim_payout",
  "claim_charge",
  "is_covid",
  "clin_discharge",
  "clin_outpatient",
  "clin_emergency",
  "clin_acc",
  "clin_c1_orig",
  "clin_c2_orig",
  "clin_c1_cleaned",
  "clin_c2_cleaned",
  "clin_icd",
  "clin_sdx",
  "clin_rvs",
  "clin_proc",
  "clin_pdx",
  "clin_pdx_source"
)

# Initialize Variables
# initialize lists
all_parts_summaries <- master_dt_list <- icd_mapping_list <- list()
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <-
  nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table::data.table() # initialize data.tables
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
