library(data.table)
library(here)

message("==== Loading Data Mapping ====")

# Enable caching and printing options for data mapping
to_use_cache <- TRUE # If TRUE, load/save .rds files to avoid repeated BigQuery queries
to_print_mapping_data <- FALSE # If TRUE, print mapping tables to console for debugging

# Helper function to load data from cache or query from BigQuery if not cached
load_or_query <- function(query, var_name, year_to_load = NULL, overwrite_cache = FALSE) {
  # Construct file path for cached .rds version
  rds_path <- here(
    "data-cleaning/debug/cache/mapping", eclaims_batch,
    paste0(
      ifelse(var_name == "hci" & !is.null(year_to_load), paste0("hci_", year_to_load),
        ifelse(var_name == "claims" & !is.null(year_to_load), paste0("claims_", year_to_load),
          var_name
        )
      ), ".rds"
    )
  )

  # Load from .rds file if cache exists and overwriting is not requested
  if (!overwrite_cache && to_use_cache && file.exists(rds_path)) {
    message(paste("Loading from cache:", rds_path))
    return(readRDS(rds_path))
  }

  # Otherwise, query BigQuery
  message(paste("Querying:", var_name))
  dt <- query_bq_to_dt(query)

  # Save result to cache if enabled
  if (to_use_cache) saveRDS(dt, rds_path)
  return(dt)
}

# Utility to print full data.table if debugging is enabled
print_all <- function(dt, title) {
  if (to_print_mapping_data) {
    cat("\n---", title, "---\n")
    print(dt, nrow = Inf)
  }
}

# Define and execute query for procedure codes used in DRG grouping
proc_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.proc")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure CODE column is character

# Load RVS to ICD-9-CM mapping
rvs_icd9_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_rvs_crosswalk.icd9_rvs_mapping")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")

# Clean and standardize RVS/ICD9 mapping
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100) # Multiply by 100 as per expected format
)]

# Merge with DRG-use metadata and flag entries used in DRG
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)], by.x = "icd9cm", by.y = "CODE", all.x = TRUE)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

# Load ACR RVS codes
acr_rvs_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.procedure")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")

# Load ICD-10 codes used for DRG grouping
i10_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.i10")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set key for fast lookup

# Extract accepted primary diagnosis (PDX) codes
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])

# Create environment from a vector for fast existence lookup
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}
acc_pdx_env <- create_env_from_vector(acc_pdx)

# Load full Philippine ICD-10 code table
phl_icd10_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_icd.phl_icd10")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")

# Extract actual neoplasm codes (with slashes)
neoplasms_dt_actual <- as.data.table(phl_icd10[grepl("/", icd10), .(icd10)])
neoplasms_dt_actual[, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))]

# Load additional ICD-10 variation mappings
i10vx_query <- paste0("SELECT * FROM ", gcp_proj, ".tdrg_libraries.i10vx")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code")

# Extract full set of ICD codes
acc_icd <- unique(i10vx[, code])

# Load z-benefit codes and clean formatting
zben_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.zben")
zben <- load_or_query(zben_query, "zben")
setkey(zben, "code")
zben[, code := gsub("[^A-Za-z0-9]", "", code)] # Strip all non-alphanumeric characters
zben <- zben$code

# Load and clean ACR codes
acr_query <- paste0("SELECT * FROM ", gcp_proj, ".phic_acr.acr")
acr <- load_or_query(acr_query, "acr")
setkey(acr, "code")
acr[, code := gsub("[^A-Za-z0-9/\\\\]", "", code)] # Keep only alphanumerics, slashes, and backslashes
acr <- acr[nchar(gsub("[^A-Za-z]", "", code)) <= 1] # Filter codes with at most 1 letter
acr <- acr$code

# Query HCI data. TODO: Uncomment this once hci is available for 2024 and 2025
# hci_query <- if (to_filter) {
#   paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_full")
# } else {
#   paste0("SELECT * FROM ", gcp_proj, ".phic_hci.hci_", year_to_load)
# }
# hci <- load_or_query(hci_query, "hci", year_to_load)

# Define global variables for code groups and their corresponding regex patterns

# Get unique neoplasm ICD codes (with slashes)
neoplasm_codes <- unique(neoplasms_dt_actual$icd10)

# Get unique COVID-related codes from covid_rvs
covid_codes <- unique(covid_rvs)

# Get unique RVS procedure codes from ACR
rvs_codes <- unique(acr_rvs$rvs)

# Build regex patterns for neoplasm, COVID, and RVS code matching
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")

# Get cleaned Philippine ICD-10 codes (excluding neoplasm-formatted codes)
phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))

# Load full ICD-10 code list used in DRG mapping
icd_codes <- unique(tdrg_icd10$CODE)

# Create fast lookup environments for various reference sets

# All DRG-use procedures
proc_env <- create_env_from_vector(proc$CODE)
# RVS codes in ICD9-RVS crosswalk
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
# ICD-9-CM codes in the crosswalk
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)
# RVS codes from ACR
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)
# Accepted primary diagnosis codes
acc_pdx_env <- create_env_from_vector(acc_pdx)
# Full PHIC ICD-10 list
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)
# Accepted ICD-10 variation codes
acc_icd_env <- create_env_from_vector(i10vx$code)

# TODO: Uncomment once HCI data is available for relevant years
# hci_env <- create_env_from_vector(hci$id_hci)

# Specialized environments for filtering specific code classes
# Neoplasm ICD codes
neoplasm_env <- create_env_from_vector(neoplasm_codes)
# COVID codes
covid_env <- create_env_from_vector(covid_codes)
# RVS codes
rvs_codes_env <- create_env_from_vector(rvs_codes)
# Cleaned PH ICDs
phil_icds_env <- create_env_from_vector(phil_icds)
# DRG ICD-10 code list
icd_codes_env <- create_env_from_vector(icd_codes)

# Unioned code sets for composite lookups
covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)

covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)

covid_rvs_neoplasm_zben_codes <- unique(
  c(covid_codes, rvs_codes, neoplasm_codes, zben)
)
covid_rvs_neoplasm_zben_env <- create_env_from_vector(
  covid_rvs_neoplasm_zben_codes
)

# Combined pattern used for regex-based extraction
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)

# # Define claim queries. TODO: Uncomment this once hci is available for 2024 and 2025
# if (to_filter) {
#   claims_query <- paste0("
#   SELECT
#       c.id_series
#   FROM `pids-drg-data.phic_eclaims.eclaims_", year_to_load, "` c
#   JOIN `pids-drg-data.phic_hfac.hfac_2023` h
#       ON c.id_hci = h.id_hci
#   WHERE
#       c.claim_status = 'G'
#       AND c.is_covid = FALSE
#       AND c.clin_outpatient = FALSE
#       AND h.inst_level IN ('INF', 'L1', 'L2', 'L3');")

#   claims <- load_or_query(claims_query, "claims", year_to_load)

#   hci_filter <- hci[inst_level %chin% c("INF", "L1", "L2", "L3"), id_hci]
# }

# Prepare custom codes for matching and dictionary augmentation

# Combine COVID, RVS, neoplasm, ZBEN, and ACR codes into one set
custom_codes <- c(covid_rvs_neoplasm_codes, zben, acr)

# Separate out custom RVS codes that start with digits (e.g., "12345")
custom_rvs_codes <- custom_codes[grepl("^[0-9]", custom_codes)]

# Keep only non-digit-prefixed codes for use in alphabetic ICD matching
custom_codes <- custom_codes[!grepl("^[0-9]", custom_codes)]

# Deduplicate non-digit-prefixed custom codes
custom_codes <- unique(custom_codes)

# Sort custom codes by descending length so longer matches
# take priority in regex
custom_codes_sorted <- unique(custom_codes[order(-nchar(custom_codes))])

# Build an ICD dictionary grouped by first character

# Step 1: Split ICD codes into a list grouped by first letter
# (e.g., "J", "K", etc.)
icd_dict <- split_icd_by_prefix(icd_codes)

# Step 2: Extend the dictionary by merging in the custom alphabetic codes
icd_dict <- extend_icd_dict_with_custom_codes(icd_dict, custom_codes_sorted)

# Step 3: Sort codes within each group by descending length
# (for longest match first)
icd_dict <- sort_icd_dict_by_length(icd_dict)

# Flatten the full dictionary into a single vector of all valid codes
all_codes <- unlist(icd_dict, use.names = FALSE)

# Print confirmation message
message("00d-load-mapping.r successfully executed.")
