## Install and load required packages
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

lapply(required_packages, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
})

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

## Column formats
integer_cols <- c("OUT_PATIENT", "EMERGENCY")

factor_cols <- c(
  "PATIENT_TYPE", "ROOM_TYPE", "DEP_REL", "PATSEX", "MEMCAT_PARENT_DESC",
  "MEMCAT_CHILD_DESC", "DISPOSITION", "CLAIMS_STATUS"
)
numeric_cols <- c(
  "PATAGE", "PAT_BWT_KG", "CLAIMS_PAID_AMT",
  "ACR_AMOUNT_ACTUAL"
)

character_cols <- c(
  "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "HCI_PMCC_NO", "HCP_NO_LIST",
  "PRIMARY_ILLNESS", "SECONDARY_ILLNESS", paste0("ICDCODE", c(1:12)),
  paste0("RVSCODE", 1:20), "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT",
  "PAT_BDAY", "EXTRACTION_DATE"
)

## Define column classes
col_classes <- c(
  rep("character", length(character_cols)),
  rep("integer", length(integer_cols)),
  rep("factor", length(factor_cols)),
  rep("numeric", length(numeric_cols))
)

names(col_classes) <- c(
  character_cols, integer_cols,
  factor_cols, numeric_cols
)

## COVID codes (for exclusion later)
covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

## Mapping of column names
old_colnames <- c(
  "SRC_YR", "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT", "EXTRACTION_DATE",
  "HCI_PMCC_NO", "HCP_NO_LIST", "PATIENT_TYPE", "DEP_REL", "PATSEX", "PATAGE",
  "PAT_BDAY", "PAT_BWT_KG", "MEMCAT_PARENT_DESC", "MEMCAT_CHILD_DESC",
  "OUT_PATIENT", "EMERGENCY", "ROOM_TYPE",
  "DISPOSITION", "PRIMARY_ILLNESS", "SECONDARY_ILLNESS",
  paste0("ICDCODE", 1:12), paste0("RVSCODE", 1:20),
  "CLAIMS_STATUS", "ACR_AMOUNT_ACTUAL", "CLAIMS_PAID_AMT"
)

new_colnames <- c(
  "id_year", "id_series", "id_pin", "date_adm", "time_adm",
  "date_dis", "time_dis", "date_rec", "date_ref", "date_check", "date_ext",
  "id_hci", "id_hcp", "pat_type", "pat_rel", "pat_sex", "pat_age",
  "pat_bdate", "pat_bwt", "pat_memcat_parent", "pat_memcat_child",
  "clin_outpatient", "clin_emergency", "clin_acc",
  "clin_discharge", "clin_c1", "clin_c2", paste0("clin_icd", 1:12),
  paste0("clin_rvs", 1:20), "claim_status", "claim_charge", "claim_payout"
)
