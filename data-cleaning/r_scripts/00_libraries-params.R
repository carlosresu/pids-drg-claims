## Required packages
required_packages <- c(
  "data.table",
  "here",
  "tictoc",
  "stringr",
  "stringi",
  "lubridate",
  "docstring",
  "profvis",
  "hash",
  "future",
  "future.apply",
  "knitr",
  "htmlwidgets",
  "parallelly",
  "stringdist",
  "progress",
  "parallel",
  "reticulate",
  "bigrquery",
  "jsonlite",
  "googleCloudStorageR"
)
## Install required packages
lapply(required_packages, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
})

# Load required packages
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

to_read <- FALSE # TODO: Deprecated, used to be whether to forcibly read the whole file again instead of using the split parts created even if available
to_split <- TRUE # TODO: Deprecated, only used when to_sample is TRUE # Whether to split into split_parts parts (i.e. to fit in 32gb RAM).

## Start total execution timer
tic("Time spent (total)               ")

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
  "SRC_YR" = "id_year",
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

# Add dynamically generated clin_icd1 through clin_icd12 and clin_rvs1 through clin_rvs20
for (i in 1:12) {
  column_mappings[[paste0("ICDCODE", i)]] <- paste0("clin_icd", i)
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
    paste0("ICDCODE", 1:12),
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
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

## Convert the COVID codes into a regular expression pattern (without word boundaries)
covid_rvs_pattern <- paste(covid_rvs, collapse = "|")

# Initialize known values for each type of remapping
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

# Define the big fcase that handles remapping for all columns
remapped_column <- fcase(
  column_name == "pat_type" & dt[[column_name]] %in% c("MEMBER", "MM"), "M",
  column_name == "pat_type" & dt[[column_name]] %in% c("DEPENDENT", "DD"), "D",
  column_name == "claim_status" & dt[[column_name]] == "DENIED", "D",
  column_name == "claim_status" & dt[[column_name]] == "IN-PROCESS", "I",
  column_name == "claim_status" & dt[[column_name]] %in% c("PAID", "APRV4PAYMENT"), "G",
  column_name == "claim_status" & dt[[column_name]] == "RTH", "R",
  column_name == "pat_memcat_parent" & dt[[column_name]] == "DIRECT CONTRIBUTOR", "D",
  column_name == "pat_memcat_parent" & dt[[column_name]] == "INDIRECT CONTRIBUTOR", "I",
  column_name == "pat_memcat_child" & dt[[column_name]] == "EMPLOYED PRIVATE", "FORMAL",
  column_name == "pat_memcat_child" & dt[[column_name]] == "SELF-EARNING INDIVIDUAL", "INFORMAL",
  column_name == "pat_memcat_child" & dt[[column_name]] == "SENIOR CITIZEN", "SENIOR",
  column_name == "pat_memcat_child" & dt[[column_name]] == "INDIGENT", "INDIGENT",
  column_name == "pat_memcat_child" & dt[[column_name]] == "LIFETIME MEMBER", "LIFETIME",
  column_name == "pat_memcat_child" & dt[[column_name]] == "SPONSORED", "SPONSORED",
  column_name == "pat_memcat_child" & dt[[column_name]] %in% c(
    "MIGRANT WORKER", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD"
  ), "OFW",
  column_name == "pat_memcat_child" & dt[[column_name]] %in% c(
    "EMPLOYED GOVERNMENT", "HOUSEHOLD HELP/KASAMBAHAY",
    "FAMILY DRIVER", "FORMAL ECONOMY", "DIRECT CONTRIBUTOR"
  ), "FORMAL",
  column_name == "pat_memcat_child" & dt[[column_name]] == "PROFESSIONAL PRACTITIONER", "INFORMAL",
  column_name == "clin_discharge" & dt[[column_name]] %in% c("IMPROVED", "RECOVERED", "I", "R"), 1L,
  column_name == "clin_discharge" & dt[[column_name]] %in% c("HOME/DISCHARGED AGAINST MEDICAL ADVICE", "H"), 2L,
  column_name == "clin_discharge" & dt[[column_name]] %in% c("ABSCONDED", "A"), 3L,
  column_name == "clin_discharge" & dt[[column_name]] %in% c("TRANSFERRED/REFERRED", "T"), 4L,
  column_name == "clin_discharge" & dt[[column_name]] %in% c("EXPIRED", "E"), 9L,
  column_name == "clin_discharge" & dt[[column_name]] == "UNDEFINED", NA_integer_
)