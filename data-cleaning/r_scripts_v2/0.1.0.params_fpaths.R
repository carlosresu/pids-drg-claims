## Start total execution timer
tictoc::tic("Time spent (total)               ")

# detect available threads
nthreads <- parallelly::availableCores()
nthreads <- if (nthreads >= 16) nthreads - thread_offset else nthreads

# # Whether to sample each split_part by sample_size_divisor
# # (useful when iterating through code runs in quick succession)
# to_sample <- FALSE
# # TODO: Add description here
# to_write <- TRUE
# # TODO: Add description here
# to_flush <- FALSE
# # TODO: Add description here
# to_parallel <- as.logical(Sys.getenv("TO_PARALLEL", "TRUE"))
# # TODO: Add description here
# to_debug <- FALSE
# verbose_output <- if (to_debug) TRUE else FALSE


dir.create(dirname(here::here("data-cleaning/cache/year_to_load.txt")), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(here::here("data-cleaning/cache/year_to_load.txt"))) writeLines("2018", here::here("data-cleaning/cache/year_to_load.txt"))
if (!exists("year_to_load")) year_to_load <- data.table::fread(here::here("data-cleaning", "cache", "year_to_load.txt"), header = FALSE, colClasses = "character")[[1]]
file_type <- if (year_to_load %in% c(2022:2023)) ".tsv" else ".csv"
separator <- if (file_type == ".tsv") "\t" else ","
to_read <- FALSE # TODO: Deprecated, used to be whether to forcibly read the whole file again instead of using the split parts created even if available
to_split <- TRUE # TODO: Deprecated, only used when to_sample is TRUE # Whether to split into split_parts parts (i.e. to fit in 32gb RAM).
thai_prompt <- TRUE # Whether to prompt for thai grouper even if bypassing all other prompts
to_prompt <- FALSE

split_parts <- 15
# Sample size divisor: Formula for sample size is
# (total_rows ÷ split_parts) ÷ sample_size_divisor.
# Choose between 5, 25, 125, and 625
if (exists("sample_size_divisor")) sample_size_divisor <- sample_size_divisor else sample_size_divisor <- 5

# Columns to drop
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 21:170) # Continuation
)
drop_cols_manual <- c(
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)

# other parameters for manual adjustments
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b") # ICD codes to replace
manual_code_replacements <- c("O800", "O80", "O809") # ICD code replacements

# Control random behavior for reproducibility
# Choose a number as seed
global_seed <- seed <- 123
# Setting the seed reproducibility
# (Important for stuff like randomly choosing a pdx among
# multiple possible options)
set.seed(seed)

if (Sys.info()["nodename"] == "ubuntu2404vm") {
  # Code to execute if the condition is TRUE
  googleAuthR::gar_auth_service("~/.config/gcloud/drg-pipeline-e80a2b3a9229.json")
} else {
  gcs_email <- "271591364028-compute@developer.gserviceaccount.com"
  googleAuthR::gar_auth(email = gcs_email)
}
# get current GCP Project
gcp_proj <- system("gcloud config get-value project", intern = TRUE)
# Name of GCS bucket
gcs_bucket <- "phic-claims-checkpoints"
# Name of folder path prefix in GCS bucket for thai grouper input
gcs_pre_fpath <- "pre-tdrg"
# Name of folder path prefix in GCS bucket for thai grouper output
gcs_post_fpath <- "post-tdrg"
# TODO: Add description here
gcs_spc_fpath <- "spc"
# bq dataset
bq_dataset <- "phic_claims"
# temp bq table, later renamed to claims_20XX1231 in Push to BQ section
bq_table <- paste0("temp_claims_", year_to_load)


# Folder Path Prefixes:
# Include spaces if there are any
full_claims_prefix <- "claims_extract_CLAIMS "
full_claims_bq_prefix <- stringr::str_replace_all(full_claims_prefix, " ", "\\\\ ")
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
checkpoint_10_prefix <- "stata"

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
checkpoint_10_path <- file.path(chkpt_path, "checkpoint_10_stata")
cache_path <- file.path(clean_prefix, "cache")
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
# Create directories for variables ending in "_path" that are directories, not files
for (path in mget(ls(pattern = "_path$"), envir = .GlobalEnv)) {
  full_path <- here::here(path)
  # Check if the path is a directory and does not end with a specific file extension
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
  paste0(full_claims_prefix, year_to_load, file_type) # Use the file_type variable here
)

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

suffix <- paste0(
  ifelse(to_sample, paste0("_sampled_", sample_size_divisor, "_"), "_full_")
)

## NA-like strings
na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
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
  "ICDCODES_ITEM7" = "clin_icd1", # We'll dynamically handle clin_icd and clin_rvs
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
  # Character columns (identifiers and date/time information)
  "character" = c(
    "CLAIM_SERIES_ID",
    "PSEUDO_CLAIMSERIES",
    "PIN",
    "PSEUDO_MEM_PIN",
    "HCI_PMCC_NO",
    "HCP_NO_LIST",
    "ADMISSION_DATE",
    "DATE_ADM",
    "ADMISSION_TIME",
    "TIME_ADM",
    "DISCHARGE_DATE",
    "DATE_DIS",
    "DISCHARGE_TIME",
    "TIME_DIS",
    "RECEIVE_DATE",
    "DATE_REC",
    "REFILE_DATE",
    "DATE_REF",
    "CHECK_DATE",
    "CHKDT",
    "EXTRACTION_DATE",
    "PRIMARY_ILLNESS",
    "SECONDARY_ILLNESS",
    "PAT_BDAY",
    "ICDCODES_ITEM7",
    "RVSCODES_ITEM7",
    paste0("ICDCODE", 1:14),
    "ICCODED15",
    paste0("ICDCODE", 16:20),
    paste0("RVSCODE", 1:20)
  ),

  # Integer columns (year, clinical, and patient data)
  "integer" = c(
    "ADMISSION_YEAR",
    "SRC_YR",
    "IS_ADMISSION_OPD",
    "IS_EMERGENCY_CASE",
    "OUT_PATIENT",
    "EMERGENCY",
    "PATIENT_AGE",
    "PATAGE"
  ),

  # Factor columns (categorical patient and claim information)
  "factor" = c(
    "PATIENT_TYPE",
    "PATIENT_RELATIONSHIP",
    "DEP_REL",
    "PATIENT_SEX",
    "PATSEX",
    "MEMCAT_PARENT_DESC",
    "MEMCAT_CHILD_DESC",
    "CLAIM_STATUS",
    "CLAIMS_STATUS",
    "PATIENT_DISPOSITION",
    "DISPOSITION",
    "ROOM_TYPE"
  ),

  # Numeric columns (claim amounts and patient weight)
  "numeric" = c(
    "CLAIM_PAID_AMOUNT",
    "CLAIMS_PAID_AMT",
    "CLAIM_AMOUNT_ACTUAL",
    "ACR_AMOUNT_ACTUAL",
    "PAT_BWT_KG"
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
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD", "SELF EARNING INDIVIDUAL",
    "FAMILY DRIVER", "FORMAL ECONOMY", "DIRECT CONTRIBUTOR", "PROFESSIONAL PRACTITIONER"
  ),
  clin_discharge = c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE", "ABSCONDED",
    "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED", "I", "R", "H", "A", "T", "E"
  )
)

# Define the fcase logic for remapping the membership categories
remapped_column <- quote(fcase(
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

# Initialize Variables
all_parts_summaries <- master_dt_list <- combined_chunk_summary <- pdx_success_list <- replacement_summary_list <- icd_mapping_list <- list() # initialize lists
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table::data.table() # initialize data.tables
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
