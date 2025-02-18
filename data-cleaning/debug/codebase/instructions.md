# Codebase Context

## Helper Scripts

### 0.1.0.params_fpaths.R
```r
tictoc::tic("Time spent (total)               ")
nthreads <- parallelly::availableCores()
nthreads <- if (nthreads >= 16) nthreads - thread_offset else nthreads
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
if (exists("sample_size_divisor")) sample_size_divisor <- sample_size_divisor else sample_size_divisor <- 5
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 21:170) # Continuation
)
drop_cols_manual <- c(
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b") # ICD codes to replace
manual_code_replacements <- c("O800", "O80", "O809") # ICD code replacements
global_seed <- seed <- 123
set.seed(seed)
if (Sys.info()["nodename"] == "ubuntu2404vm") {
  service_account_json <- "~/.config/gcloud/drg-pipeline-e80a2b3a9229.json"
  googleAuthR::gar_auth_service(json_file = service_account_json)
  googleCloudStorageR::gcs_auth(json_file = service_account_json)
} else {
  gcs_email <- "271591364028-compute@developer.gserviceaccount.com"
  googleAuthR::gar_auth(email = gcs_email)
}
gcp_proj <- system("gcloud config get-value project", intern = TRUE)
gcs_bucket <- "phic-claims-checkpoints"
gcs_pre_fpath <- "pre-tdrg"
gcs_post_fpath <- "post-tdrg"
gcs_spc_fpath <- "spc"
bq_dataset <- "phic_claims"
bq_table <- paste0("temp_claims_", year_to_load)
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
profvis_fpath <- here::here("data-cleaning", "data", "profvis", "profvis.html")
created_dirs <- c() # Initialize empty vector
for (path in mget(ls(pattern = "_path$"), envir = .GlobalEnv)) {
  full_path <- here::here(path)
  if (!dir.exists(full_path) && !grepl("\\.rds$|\\.csv$|\\.tsv$", full_path)) {
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
    created_dirs <- c(created_dirs, full_path)
  }
}
if (length(created_dirs) == 0) {
  message("All directories exist.\n")
} else {
  message("The following directories were created:\n")
  message(paste(paste(created_dirs, collapse = ",\n"), "\n"))
}
full_claims_file <- here::here(
  raw_claims_path,
  paste0(full_claims_prefix, year_to_load, file_type) # Use the file_type variable here
)
ram_limit <- (1 - 0.10) * 64 * (1024^3)
options(future.globals.maxSize = ram_limit)
total_rows_file <- here::here(
  cache_path, "total_rows",
  paste0("total_rows_", year_to_load, ".rds")
)
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
na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)
column_mappings <- list(
  "ADMISSION_YEAR" = "id_year",
  "ADMISSION_DATE" = "date_adm",
  "DATE_ADM" = "date_adm",
  "ADMISSION_TIME" = "time_adm",
  "TIME_ADM" = "time_adm",
  "DISCHARGE_DATE" = "date_dis",
  "DATE_DIS" = "date_dis",
  "DISCHARGE_TIME" = "time_dis",
  "TIME_DIS" = "time_dis",
  "CLAIM_SERIES_ID" = "id_series",
  "PSEUDO_CLAIMSERIES" = "id_series",
  "PIN" = "id_pin",
  "PSEUDO_MEM_PIN" = "id_pin",
  "RECEIVE_DATE" = "date_rec",
  "DATE_REC" = "date_rec",
  "REFILE_DATE" = "date_ref",
  "DATE_REF" = "date_ref",
  "CHECK_DATE" = "date_check",
  "CHKDT" = "date_check",
  "EXTRACTION_DATE" = "date_ext",
  "HCI_PMCC_NO" = "id_hci",
  "HCP_NO_LIST" = "id_hcp",
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
  "numeric" = c(
    "CLAIM_PAID_AMOUNT",
    "CLAIMS_PAID_AMT",
    "CLAIM_AMOUNT_ACTUAL",
    "ACR_AMOUNT_ACTUAL",
    "PAT_BWT_KG"
  )
)
covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4",
  "C191P1", "C191P2", "C191P3", "C191P4",
  "C19PP1", "C19PP2", "C19PP3", "C19PP4",
  "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID", "C19AT1", "C19HI"
)
covid_rvs_pattern <- paste(covid_rvs, collapse = "|")
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
all_parts_summaries <- master_dt_list <- combined_chunk_summary <- pdx_success_list <- replacement_summary_list <- icd_mapping_list <- list() # initialize lists
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table::data.table() # initialize data.tables
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
```

### 0.2.0.process_helper_functions.R
```r
manual_replacement <- function(text) {
  stri_replace_all_regex(text, manual_patterns_to_replace, manual_code_replacements, vectorize_all = FALSE)
}
remove_periods_and_whitespaces <- function(x) {
  x <- sapply(x, function(elem) iconv(elem, from = "latin1", to = "UTF-8"), USE.NAMES = FALSE)
  x <- gsub("[.\\s]", "", x)
  return(x)
}
split_to_vector <- function(column) {
  lapply(column, function(long_string) {
    result <- character(0)
    if (is.na(long_string)) {
      return(result)
    }
    covid_matches <- gregexpr(covid_pattern, long_string, perl = TRUE)[[1]]
    if (!is.na(covid_matches[1]) && covid_matches[1] != -1) {
      covid_codes <- regmatches(long_string, list(covid_matches))[[1]]
      result <- c(result, covid_codes)
      long_string <- gsub(covid_pattern, "", long_string, perl = TRUE)
    }
    neoplasm_matches <- gregexpr(neoplasm_pattern, long_string, perl = TRUE)[[1]]
    if (!is.na(neoplasm_matches[1]) && neoplasm_matches[1] != -1) {
      neoplasm_codes <- regmatches(long_string, list(neoplasm_matches))[[1]]
      result <- c(result, neoplasm_codes)
      long_string <- gsub(neoplasm_pattern, "", long_string, perl = TRUE)
    }
    rvs_patterns <- c(
      "[A-Za-z]{3}[0-9]{2}", # Three letters followed by one or two digits
      "[A-Za-z]{2}[0-9]{3}", # Two letters followed by two or three digits
      "[A-Za-z][0-9]{4}", # A letter followed by four or five digits
      "[0-9]{5}" # Five consecutive numbers
    )
    for (pattern in rvs_patterns) {
      rvs_matches <- gregexpr(pattern, long_string, perl = TRUE)[[1]]
      if (!is.na(rvs_matches[1]) && rvs_matches[1] != -1) {
        rvs_codes <- regmatches(long_string, list(rvs_matches))[[1]]
        result <- c(result, rvs_codes)
        long_string <- gsub(pattern, "", long_string, perl = TRUE)
      }
    }
    if (!is.na(long_string) && nchar(long_string) > 0) {
      result <- c(result, long_string)
    }
    final_result <- unlist(lapply(result, function(element) {
      strsplit(element, "\\|\\|", perl = TRUE)[[1]]
    }))
    return(final_result[final_result != ""]) # Remove any empty strings
  })
}
remove_lumped_icd_codes <- function(column) {
  result <- lapply(column, function(vec) {
    processed <- unlist(lapply(vec, function(element) {
      if ((is.na(element) || element == "") # && (!is.null(mget(element, envir = neoplasm_env, ifnotfound = NA)[[1]]) || !is.null(mget(element, envir = covid_env, ifnotfound = NA)[[1]]))
      ) {
        return(character(0)) # Keep intact if it's a valid neoplasm or COVID code
      } else {
        return(unlist(strsplit(element, "(?<=\\d)(?=[A-Z][0-9]{2,})", perl = TRUE)))
      }
    }))
    return(processed[processed != ""])
  })
  return(result)
}
remove_lumped_rvs_codes <- function(column) {
  split_rvs_codes_helper <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # If the input code is NA, empty, or NULL, return NA
    }
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 5 != 0) {
      message(paste0("Total length of concatenated RVS codes is not a multiple of 5 characters: ", code_clean))
      return(NA_character_)
    } else {
      modified_code <- gsub("(.{5})", "\\1||", code_clean)
      modified_code <- gsub("\\|\\|$", "", modified_code)
      return(modified_code)
    }
  }
  modified_column <- sapply(as.character(column), split_rvs_codes_helper, USE.NAMES = FALSE)
  return(modified_column) # Return the modified column with split RVS codes
}
collapse_to_string <- function(vec) {
  vec <- vec[vec != "" & !is.na(vec)]
  if (length(vec) > 0) {
    paste(vec, collapse = "||")
  } else {
    NA_character_
  }
}
replace_na_or_empty <- function(dt, replace_with, to_view_checks = TRUE, additional_columns = NULL) {
  if (!replace_with %chin% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }
  cols <- if (replace_with == "NA_character_") {
    names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col) || is.list(col))]
  } else {
    names(dt)[sapply(dt, is.list)]
  }
  cols <- unique(c(cols, additional_columns))
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    String_NA_Replaced = integer(),
    Actual_NA_Replaced = integer()
  )
  replacement_value <- if (replace_with == "NA_character_") NA_character_ else character(0)
  label_na_replaced <- if (replace_with == "NA_character_") "String_NA_Replaced" else "Actual_NA_Replaced"
  label_char0_replaced <- if (replace_with == "NA_character_") "Actual_NA_Replaced" else "String_NA_Replaced"
  for (col_name in cols) {
    col <- dt[[col_name]]
    empty_count <- 0
    string_na_count <- 0
    actual_na_count <- 0
    if (is.list(col)) {
      if (to_view_checks) {
        empty_count <- sum(sapply(col, function(x) identical(x, "")))
        string_na_count <- sum(sapply(col, function(x) identical(x, "NA")))
        actual_na_count <- sum(sapply(col, function(x) all(is.na(x)) || (is.list(x) && length(x) == 0)))
      }
      dt[, (col_name) := lapply(get(col_name), function(x) {
        if (all(is.na(x)) || identical(x, "") || identical(x, "NA")) character(0) else x
      })]
    } else {
      if (to_view_checks) {
        empty_count <- sum(col == "", na.rm = TRUE)
        string_na_count <- sum(col == "NA", na.rm = TRUE)
        actual_na_count <- sum(col == "character(0)", na.rm = TRUE)
      }
      dt[
        get(col_name) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
        (col_name) := NA_character_
      ]
      if (is.factor(col)) {
        set(dt, j = col_name, value = factor(dt[[col_name]], levels = c(levels(col), NA)))
      }
    }
    summary_row <- data.table(
      Column = col_name,
      Empty_Replaced = empty_count,
      String_NA_Replaced = ifelse(replace_with == "NA_character_", string_na_count, NA_integer_),
      Actual_NA_Replaced = ifelse(replace_with == "character(0)", actual_na_count, NA_integer_)
    )
    replacement_summary <- rbind(replacement_summary, summary_row, fill = TRUE)
  }
  replacement_summary <- replacement_summary[
    Empty_Replaced > 0 | get(label_na_replaced) > 0 | get(label_char0_replaced) > 0
  ]
  return(list(
    return_data = dt,
    return_replacement_summary = replacement_summary
  ))
}
clean_column <- function(col) {
  column_to_clean <- as.character(col)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d/\\\\]+", "")
  cleaned_col <- stri_replace_all_regex(
    cleaned_col,
    "(?<=[A-Z])/|/(?=[A-Z])|(?<=[A-Z])\\\\|\\\\(?=[A-Z])",
    "",
    opts_regex = stri_opts_regex(case_insensitive = TRUE)
  )
  cleaned_col[cleaned_col %chin% na_like_strings] <- NA_character_
  covid_rvs_pattern <- paste0(covid_rvs, collapse = "|")
  is_covid <- stri_detect_regex(cleaned_col, covid_rvs_pattern)
  is_covid[is.na(is_covid)] <- FALSE # Handle NAs
  neopl <- setNames(neoplasms_dt_actual$icd10, gsub("/", "", neoplasms_dt_actual$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]
  return(list(
    cleaned_col = cleaned_col,
    is_covid = is_covid
  ))
}
remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_) # Return NA for NULL or empty lists
  } else {
    return(gsub("\\s+", "", x)) # Remove all whitespace characters
  }
}
flatten_and_clean <- function(input) {
  input <- unlist(input, recursive = TRUE)
  if (length(input) == 0 || all(is.null(input)) || all(is.na(input))) {
    return(character(0)) # Return empty character vector if all NULL/NA
  } else {
    return(input) # Already a flat character vector
  }
}
```

### 0.3.0.summary_helper_functions.R
```r
print_status_update <- function(status_part, split_parts, processing_times, phase) {
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time
  convert_and_format_time <- function(seconds) {
    period <- lubridate::seconds_to_period(round(seconds))
    h <- lubridate::hour(period)
    m <- lubridate::minute(period)
    s <- lubridate::second(period)
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))
    return(trimws(paste(time_components, collapse = " ")))
  }
  elapsed_str <- convert_and_format_time(elapsed_time)
  remaining_str <- convert_and_format_time(estimated_remaining_time)
  print_update <- function(message) {
    cat(sprintf(message, status_part, split_parts, elapsed_str, remaining_str))
    utils::flush.console()
  }
  if (phase == "split") {
    if (avg_time_per_part >= 4 || status_part %% 5 == 0) {
      print_update("\rFinished splitting %d of %d parts in %s (ETA %s)       ")
    }
  } else if (phase == "clean") {
    if (avg_time_per_part >= 4 || status_part %% 5 == 0) {
      print_update("\rFinished cleaning %d of %d parts in %s (ETA %s)       ")
    }
  }
}
safe_access <- function(s, field) {
  if (is.list(s) && field %in% names(s)) s[[field]] else NULL
}
safe_unlist <- function(x) if (length(x) > 0) unlist(x, recursive = TRUE) else character(0)
extract_modified_matches <- function(summaries) {
  all_modified_matches <- list()
  all_modified_match <- list()
  for (summary in summaries) {
    result <- safe_access(summary, "modified_matches")
    if (is.list(result) && all(c("modified_matches", "modified_match") %in% names(result))) {
      all_modified_matches <- append(all_modified_matches, result$modified_matches)
      all_modified_match <- append(all_modified_match, result$modified_match)
    }
  }
  data.table::data.table(
    modified_matches = safe_unlist(all_modified_matches),
    modified_match = safe_unlist(all_modified_match)
  )[, .(count = .N), by = .(modified_matches, modified_match)]
}
safe_extract <- function(summary, field) {
  tryCatch(summary[[field]], error = function(e) NULL)
}
combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- data.table::rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table::data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}
combine_replacement_tables <- function(rboundlist, replace_with) {
  if (!replace_with %in% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }
  if (replace_with == "NA_character_") {
    summary_cols <- c("Empty_Replaced", "NA_Replaced", "Character0_Replaced")
  } else {
    summary_cols <- c("Empty_Replaced", "String_NA_Replaced", "Actual_NA_Replaced")
  }
  if (nrow(rboundlist) == 0) {
    return(data.table::data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_or_String_Replaced = integer(),
      Character0_or_Actual_Replaced = integer()
    ))
  }
  for (col in summary_cols) {
    if (!col %in% names(rboundlist)) rboundlist[, (col) := 0]
  }
  combined <- rboundlist[, .(
    Empty_Replaced = sum(get(summary_cols[1]), na.rm = TRUE),
    Replaced_1 = sum(get(summary_cols[2]), na.rm = TRUE),
    Replaced_2 = sum(get(summary_cols[3]), na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -Replaced_1, -Replaced_2)]
  if (replace_with == "NA_character_") {
    data.table::setnames(combined, old = c("Replaced_1", "Replaced_2"), new = c("NA_Replaced", "Character0_Replaced"))
    return(combined[, .(Column, Empty_Replaced, NA_Replaced, Character0_Replaced)])
  } else {
    data.table::setnames(combined, old = c("Replaced_1", "Replaced_2"), new = c("String_NA_Replaced", "Actual_NA_Replaced"))
    return(combined[, .(Column, Empty_Replaced, String_NA_Replaced, Actual_NA_Replaced)])
  }
}
final_combine_replace_tables <- function(rboundlist, replace_with, samplesizedivisor = sample_size_divisor, splitparts = split_parts, totalrows = total_rows) {
  if (!replace_with %in% c("NA_character_", "character(0)")) {
    stop("Invalid replace_with argument. Use either 'NA_character_' or 'character(0)'.")
  }
  if (replace_with == "NA_character_") {
    summary_cols <- c("Empty_Replaced", "NA_Replaced", "Character0_Replaced")
  } else {
    summary_cols <- c("Empty_Replaced", "String_NA_Replaced", "Actual_NA_Replaced")
  }
  samplesize <- ceiling(totalrows / samplesizedivisor)
  total_elements <- samplesize * splitparts
  if (nrow(rboundlist) == 0) {
    return(data.table::data.table(
      Column = character(),
      Empty_Replaced_Percentage = character(),
      NA_or_String_Replaced_Percentage = character(),
      Character0_or_Actual_Replaced_Percentage = character()
    ))
  }
  for (col in summary_cols) {
    if (!col %in% names(rboundlist)) rboundlist[, (col) := 0]
  }
  combined_replace <- rboundlist[, .(
    Total_Empty_Replaced = sum(get(summary_cols[1]), na.rm = TRUE),
    Total_Replaced_1 = sum(get(summary_cols[2]), na.rm = TRUE),
    Total_Replaced_2 = sum(get(summary_cols[3]), na.rm = TRUE)
  ), by = Column]
  combined_replace[, `:=`(
    Empty_Replaced_Percentage = pmin((Total_Empty_Replaced / total_elements) * 100, 100),
    Replaced_1_Percentage = pmin((Total_Replaced_1 / total_elements) * 100, 100),
    Replaced_2_Percentage = pmin((Total_Replaced_2 / total_elements) * 100, 100)
  )]
  combined_replace[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    Replaced_1_Percentage = sprintf("%.2f%%", Replaced_1_Percentage),
    Replaced_2_Percentage = sprintf("%.2f%%", Replaced_2_Percentage)
  )]
  if (replace_with == "NA_character_") {
    data.table::setnames(combined_replace, old = c("Replaced_1_Percentage", "Replaced_2_Percentage"), new = c("NA_Replaced_Percentage", "Character0_Replaced_Percentage"))
    return(combined_replace[, .(Column, Empty_Replaced_Percentage, NA_Replaced_Percentage, Character0_Replaced_Percentage)])
  } else {
    data.table::setnames(combined_replace, old = c("Replaced_1_Percentage", "Replaced_2_Percentage"), new = c("String_NA_Replaced_Percentage", "Actual_NA_Replaced_Percentage"))
    return(combined_replace[, .(Column, Empty_Replaced_Percentage, String_NA_Replaced_Percentage, Actual_NA_Replaced_Percentage)])
  }
}
```

### 0.5.0.grouping_functions.R
```r
export_for_grouper <- function(dt, output_txt_file, chunk_number) {
  output_dt_thai <- data.table()
  output_dt_thai[, CASEID := dt$caseid]
  output_dt_thai[, DOB := as.character(format(as.Date(dt$pat_bdate), "%d/%m/%Y"))]
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt_thai[, DateAdm := format(as.Date(dt$date_adm), "%d/%m/%Y")]
  output_dt_thai[, TimeAdm := format(as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M")]
  output_dt_thai[, DateDsc := format(as.Date(dt$date_dis), "%d/%m/%Y")]
  output_dt_thai[, TimeDsc := format(as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M")]
  output_dt_thai[, DischT := dt$clin_discharge]
  output_dt_thai[, AdmWt := dt$pat_bwt]
  output_dt_thai[, PDx := dt$clin_pdx]
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12 # Ensure there are 12 elements
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt_thai[, (icd_cols) := icd_codes]
  proc_codes_list <- lapply(dt$clin_proc, function(proc_str) {
    codes <- unlist(proc_str)
    length(codes) <- 20 # Ensure there are 20 elements
    codes
  })
  proc_codes <- as.data.table(do.call(rbind, proc_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt_thai[, (proc_cols) := proc_codes]
  output_dt_thai[is.na(output_dt_thai)] <- "--"
  str(output_dt_thai)
  fwrite(output_dt_thai, output_txt_file, sep = "|", col.names = TRUE)
  if (to_debug) {
    return(NULL)
  }
}
```

### 1.0.query_bq_to_dt.R
```r
query_bq_to_dt <- function(query, max_bq_rows = Inf) {
  tryCatch(
    dt <- as.data.table(
      bq_table_download(bq_project_query(gcp_proj, query), n_max = max_bq_rows)
    ),
    error = function(e) {
      stop(paste("Error querying BigQuery:", e$message))
    }
  )
  return(dt)
}
```

### 2.0.split_and_save_part.R
```r
split_and_save_part <- function(split_loop_part) {
  rows_per_part <- ceiling(total_rows / split_parts)
  chunk_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_loop_part),
    "_of_", split_parts, ".rds"
  ))
  if (!file.exists(chunk_file)) {
    start_row <- (split_loop_part - 1) * rows_per_part + 1
    end_row <- min(split_loop_part * rows_per_part, total_rows)
    chunk_dt <- full_file[start_row:end_row]
    saveRDS(chunk_dt, chunk_file, compress = TRUE)
    rm(chunk_dt)
    invisible(gc())
  }
}
```

### 3.0.create_sample_files.R
```r
create_sample_files <- function(sample_part, sampled_claims_file, seed = global_seed) {
  partial_file_for_sampling <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".rds"
  ))
  dt <- readRDS(partial_file_for_sampling)
  set.seed(seed)
  dt <- dt[sample(.N, min(sample_size, .N))]
  saveRDS(dt, sampled_claims_file, compress = TRUE)
}
```

### 4.0.read_appropriate_file.R
```r
read_appropriate_file <- function(read_part, to_sample_argument = to_sample) {
  chunk_file <- if (to_sample_argument) {
    here(raw_claims_samples_path, paste0(
      "sampled_claims_", year_to_load, "_", sample_size_divisor,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))
  }
  dt <- readRDS(chunk_file)
  available_columns <<- colnames(dt)
  if (any(drop_cols %in% available_columns)) {
    dt <- dt[, (drop_cols) := NULL]
  }
  if (any(drop_cols_manual %in% available_columns)) {
    dt <- dt[, (drop_cols_manual) := NULL]
  }
  replace_result <- replace_na_or_empty(dt = dt, replace_with = "NA_character_")
  dt <- replace_result$return_data
  replacement_summary <- replace_result$return_replacement_summary
  col_classes <- sapply(available_columns, function(col) {
    if (col %in% unlist(expected_types["character"])) {
      return("character")
    }
    if (col %in% unlist(expected_types["integer"])) {
      return("integer")
    }
    if (col %in% unlist(expected_types["factor"])) {
      return("factor")
    }
    if (col %in% unlist(expected_types["numeric"])) {
      return("numeric")
    }
  })
  for (col in names(col_classes)) {
    original_values <- dt[[col]]
    dt[[col]] <- switch(col_classes[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = {
        levels <- unique(dt[[col]])
        as.factor(dt[[col]])
      },
      "integer" = {
        suppressWarnings(as.integer(dt[[col]]))
      },
      "numeric" = {
        suppressWarnings(as.numeric(dt[[col]]))
      },
      dt[[col]] # Default case: no conversion if unrecognized type
    )
    coerced_to_na <- which(is.na(dt[[col]]) & !is.na(original_values))
    if (length(coerced_to_na) > 0) {
      cat(sprintf(
        "Column '%s' coerced %d values to NA.
          First few original values: %s\n",
        col, length(coerced_to_na),
        paste(original_values[coerced_to_na][1:5],
          collapse = ", "
        )
      ))
    }
  }
  nrow_start[[read_part]] <<- nrow(dt)
  return(
    list(
      read_result_dt = dt,
      read_result_replacement_summary = replacement_summary
    )
  )
}
```

### 5.0.process_chunk.R
```r
process_chunk <- function(chunk,
                          yr_to_load = year_to_load,
                          col_maps = column_mappings,
                          known_vals = known_values,
                          remap_cols = remapped_column,
                          avail_cols = available_columns) {
  setnames(chunk,
    old = avail_cols[avail_cols %in% names(col_maps)],
    new = sapply(
      avail_cols[avail_cols %in% names(col_maps)],
      function(col) col_maps[[col]]
    )
  )
  renamesuccess <- all(sapply(
    avail_cols[avail_cols %in% names(col_maps)],
    function(col) col_maps[[col]]
  ) %in% colnames(chunk))
  if (!"id_year" %in% colnames(chunk)) {
    chunk[, id_year := as.integer(yr_to_load)]
  }
  chunk[, `:=`(
    id_series = trimws(id_series),
    id_pin = trimws(id_pin)
  )]
  chunk[, `:=`(
    c1 = clin_c1,
    c2 = clin_c2
  )]
  chunk[, (c("time_adm", "time_dis")) := lapply(.SD, function(col) {
    ifelse(
      grepl("AM|PM", col),
      format(as.POSIXct(sub("\\.\\d+ ", " ", col), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
      col
    )
  }), .SDcols = c("time_adm", "time_dis")]
  cols_to_extract <- grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)
  col_list <- collapse_and_clean_icd_rvs_cols(
    lapply(cols_to_extract[grepl("^clin_icd", cols_to_extract)], function(col) chunk[[col]]),
    lapply(cols_to_extract[grepl("^clin_rvs", cols_to_extract)], function(col) chunk[[col]])
  )
  chunk[, `:=`(clin_icd = col_list$clin_icd, clin_rvs = col_list$clin_rvs)]
  chunk[, (grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)) := NULL]
  c1_result <- clean_column(chunk$c1)
  chunk[, `:=`(c1_orig = c1, c1 = c1_result$cleaned_col)]
  c1_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c1_orig, toString),
    new_code = sapply(chunk$c1, toString)
  )[
    remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]
  c2_result <- clean_column(chunk$c2)
  chunk[, `:=`(c2_orig = c2, c2 = c2_result$cleaned_col)]
  c2_cleaning_comparison <- data.table(
    old_code = sapply(chunk$c2_orig, toString),
    new_code = sapply(chunk$c2, toString)
  )[
    remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]
  is_covid_c1 <- c1_result$is_covid
  is_covid_c2 <- c2_result$is_covid
  chunk[, is_covid := (is_covid_c1 | is_covid_c2)]
  chunk[, c1 := lapply(c1, function(text) {
    replaced_text <- manual_replacement(text)
    collapsed_text <- collapse_to_string(replaced_text)
    split_result <- split_to_vector(collapsed_text)
    cleaned_result <- remove_lumped_icd_codes(split_result)
    return(flatten_and_clean(cleaned_result))
  })]
  chunk[, c2 := lapply(c2, function(text) {
    replaced_text <- manual_replacement(text)
    collapsed_text <- collapse_to_string(replaced_text)
    split_result <- split_to_vector(collapsed_text)
    cleaned_result <- remove_lumped_icd_codes(split_result)
    return(flatten_and_clean(cleaned_result))
  })]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    clin_icd_list <- c(manual_replacement(clin_icd[[i]]), c1[[i]], c2[[i]])
    return(flatten_and_clean(clin_icd_list))
  })]
  replace_result <- replace_na_or_empty(dt = chunk, replace_with = "NA_character_")
  chunk <- replace_result$return_data
  empty_replaced_with_na_1 <- replace_result$return_replacement_summary
  replace_empty_result_1 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
  chunk <- replace_empty_result_1$return_data
  NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary
  c1_results <- append_copy_and_remove_icd_rvs(chunk$c1, chunk$clin_rvs, chunk$clin_icd)
  chunk[, clin_rvs := c1_results$clin_rvs]
  chunk[, c1 := c1_results$col]
  chunk[, clin_icd := c1_results$clin_icd]
  c1_discarded_rvs <- c1_results$discarded_rvs
  c2_results <- append_copy_and_remove_icd_rvs(chunk$c2, chunk$clin_rvs, chunk$clin_icd)
  chunk[, clin_rvs := c2_results$clin_rvs]
  chunk[, c2 := c2_results$col]
  chunk[, clin_icd := c2_results$clin_icd]
  c2_discarded_rvs <- c2_results$discarded_rvs
  replace_result <- replace_na_or_empty(dt = chunk, replace_with = "NA_character_")
  chunk <- replace_result$return_data
  empty_replaced_with_na_1 <- replace_result$return_replacement_summary
  replace_empty_result_1 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
  chunk <- replace_empty_result_1$return_data
  NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary
  remap_res <- remap_patient_data(
    pat_type = chunk$pat_type,
    pat_memcat_parent = chunk$pat_memcat_parent,
    pat_memcat_child = chunk$pat_memcat_child,
    clin_discharge = chunk$clin_discharge,
    claim_status = chunk$claim_status,
    known_values = known_vals,
    remapped_column = remap_cols
  )
  chunk[, pat_type := remap_res$remapped$pat_type]
  chunk[, pat_memcat_parent := remap_res$remapped$pat_memcat_parent]
  chunk[, pat_memcat_child := remap_res$remapped$pat_memcat_child]
  chunk[, clin_discharge := remap_res$remapped$clin_discharge]
  chunk[, claim_status := remap_res$remapped$claim_status]
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]
  modified_c1 <- lapply(chunk$c1, function(x) if (is.null(x) || all(is.na(x))) character(0) else x)
  modified_c2 <- lapply(chunk$c2, function(x) if (is.null(x) || all(is.na(x))) character(0) else x)
  chunk[, c1 := modified_c1]
  chunk[, c2 := modified_c2]
  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd
  icd10_mapping_result <- map_icd10(c1, c2, clin_icd)
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]
  replace_empty_result_2 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
  chunk <- replace_empty_result_2$return_data
  NA_replaced_with_empty_2 <- replace_empty_result_2$return_replacement_summary
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]
  pdx_result <- find_pdx(chunk$c1, chunk$c2, chunk$clin_icd)
  chunk[, pdx := pdx_result$pdx]
  chunk[, pdx_code := pdx_result$pdx_code]
  chunk[, c1 := lapply(seq_len(.N), function(i) {
    lst <- c1[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]
  chunk[, c2 := lapply(seq_len(.N), function(i) {
    lst <- c2[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    lst <- clin_icd[[i]]
    pdx_val <- pdx[i]
    if (!is.na(pdx_val)) {
      lst <- setdiff(lst, pdx_val)
    }
    as.character(lst)
  })]
  chunk[, clin_c1 := c1_orig]
  chunk[, clin_c2 := c2_orig]
  chunk[, c("c1_orig", "c2_orig") := NULL]
  chunk[, clin_proc := icd9_list]
  chunk[, icd9_list := NULL]
  chunk[, clin_icd := Map(function(pdx_var, sdx_var) {
    sdx_var[sdx_var != pdx_var]
  }, pdx, clin_icd)]
  chunk[, clin_sdx := clin_icd]
  chunk[, clin_icd := NULL]
  if (!"pat_bdate" %in% colnames(chunk)) {
    chunk[, pat_bdate := NA_Date_]
  }
  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )
  chunk[, (date_cols) := lapply(
    .SD,
    function(x) {
      converted_dates <- as.Date(x, format = "%m/%d/%Y")
      converted_dates[converted_dates < as.Date("1900-01-01")] <- NA_Date_
      return(converted_dates)
    }
  ), .SDcols = date_cols]
  time_cols <- c("time_adm", "time_dis")
  chunk[, (time_cols) := lapply(
    .SD,
    function(x) {
      x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
      as.character(x)
    }
  ), .SDcols = time_cols]
  chunk[, date_adm := as.POSIXct(
    paste(date_adm, time_adm),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]
  chunk[, date_dis := as.POSIXct(
    paste(date_dis, time_dis),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]
  chunk[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
  chunk[, clin_emergency := as.logical(as.integer(clin_emergency))]
  if (!"pat_bwt" %in% colnames(chunk)) {
    chunk[, pat_bwt := NA_real_]
  }
  num_cols <- c(
    "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
    "claim_charge", "id_year", "pdx_code"
  )
  chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  int_cols <- c("clin_discharge", "id_year", "pdx_code")
  chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  char_cols <- c(
    "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
    "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx"
  )
  chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  char_cols <- names(chunk)[sapply(chunk, is.character)]
  chunk[, pat_ageday := NA_integer_]
  chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
    valid_codes <- codes[codes %chin% acc_icd_set]
    if (length(valid_codes) > 0) {
      return(valid_codes)
    } else {
      return(NA_character_)
    }
  })]
  chunk[, clin_sdx := lapply(clin_sdx, function(x) if (is.null(x)) character(0) else unlist(x))]
  replace_empty_result_3 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)", additional_columns = c("c1", "c2", "pdx"))
  chunk <- replace_empty_result_3$return_data
  NA_replaced_with_empty_3 <- replace_empty_result_3$return_replacement_summary
  chunk[, (char_cols) := lapply(.SD, function(col) iconv(col, from = "", to = "UTF-8")), .SDcols = char_cols]
  chunk[, (char_cols) := lapply(.SD, function(col) {
    col[col %chin% c("None", "")] <- NA_character_
    return(col)
  }), .SDcols = char_cols]
  num_cols <- names(chunk)[sapply(chunk, is.numeric)]
  chunk[, (num_cols) := lapply(.SD, function(col) {
    col[is.nan(col)] <- NA_real_
    return(col)
  }), .SDcols = num_cols]
  array_columns <- c("id_hcp")
  split_pattern <- "\\s*,\\s*|\\|\\||\\|"
  chunk[, (array_columns) := lapply(.SD, function(x) {
    x <- strsplit(x, split_pattern)
    lapply(x, function(y) {
      if (length(y) == 0L || all(is.na(y))) {
        character(0)
      } else {
        y
      }
    })
  }), .SDcols = array_columns]
  list_columns <- c("id_hcp")
  chunk[, (list_columns) := lapply(.SD, function(col) {
    lapply(col, function(x) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) {
        character(0)
      } else {
        x
      }
    })
  }), .SDcols = list_columns]
  setnames(chunk, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))
  setcolorder(chunk, c(
    "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
    "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
    "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
    "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent",
    "pat_memcat_child", "claim_status", "claim_payout",
    "claim_charge", "is_covid", "clin_discharge", "clin_outpatient", "clin_emergency",
    "clin_acc", "clin_c1", "c1", "clin_c2", "c2", "clin_sdx", "clin_proc",
    "clin_rvs", "clin_pdx", "clin_pdx_source"
  ))
  chunk[, clin_discharge := as.integer(clin_discharge)]
  chunk[, clin_sdx := lapply(clin_sdx, function(x) head(x, 12))]
  chunk[, clin_proc := lapply(clin_proc, function(x) head(x, 20))]
  chunk[!is.na(pat_bdate) & !is.na(date_adm), pat_age := floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)]
  chunk[!is.na(pat_bdate) & !is.na(date_adm) & !is.na(pat_age) & pat_bdate > as.Date(date_adm), pat_bdate := NA_Date_]
  chunk[grepl("99432", c1) & !is.na(pat_age) & pat_age < 0 & pat_age >= -1, pat_age := 0]
  chunk[!is.na(pat_age) & pat_age > 0 & pat_age <= 124, pat_age := floor(pat_age)]
  chunk[!is.na(pat_age) & (pat_age < 0 | pat_age > 124), pat_age := NA_integer_]
  chunk_summary <- list(
    rename_success = renamesuccess,
    ICD_replacements_1 = c1_cleaning_comparison,
    ICD_replacements_2 = c2_cleaning_comparison,
    pat_type_mapped = remap_res$pat_type_mapped,
    pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
    clin_discharge_mapped = remap_res$clin_discharge_mapped,
    claim_status_mapped = remap_res$claim_status_mapped,
    pat_type_unmapped = remap_res$pat_type_unmapped,
    memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
    memcat_child_unmapped = remap_res$memcat_child_unmapped,
    discharge_unmapped = remap_res$discharge_unmapped,
    claim_status_unmapped = remap_res$claim_status_unmapped,
    discard_rvs_one = c1_discarded_rvs,
    discard_rvs_two = c2_discarded_rvs,
    empty_replaced_with_na_1 = empty_replaced_with_na_1,
    NA_replaced_with_empty_1 = NA_replaced_with_empty_1,
    NA_replaced_with_empty_2 = NA_replaced_with_empty_2,
    NA_replaced_with_empty_3 = NA_replaced_with_empty_3,
    unique_icds = icd10_mapping_result$unique_icds,
    direct_matches = icd10_mapping_result$direct_matches,
    unmatched_codes = icd10_mapping_result$unmatched_codes,
    unmatched_sources = icd10_mapping_result$unmatched_sources,
    icd10_map_dt = icd10_mapping_result$icd10_map_dt,
    modified_matches = icd10_mapping_result$modified_matches,
    rvss = rvs_mapping_result$rvss,
    mappable_rvs = rvs_mapping_result$mappable_rvs,
    unmappable_rvs = rvs_mapping_result$unmappable_rvs,
    multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
    without_drg = rvs_mapping_result$without_drg
  )
  invisible(gc())
  return(list(
    return_chunk = chunk,
    return_summary = chunk_summary
  ))
}
```

### 5.1.0.collapse_and_clean_icd_rvs_cols.R
```r
collapse_and_clean_icd_rvs_cols <- function(clin_icd_cols = NULL, clin_rvs_cols = NULL) {
  result <- list()
  process_columns <- function(cols, is_icd = TRUE) {
    cleaned_results <- lapply(cols, clean_column)
    cleaned_columns <- lapply(cleaned_results, function(res) res$cleaned_col)
    collapsed <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
      combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
      combined <- combined[!combined %chin% na_like_strings & combined != ""]
      if (length(combined) > 0) {
        paste(combined, collapse = "||")
      } else {
        NA_character_
      }
    })
    split <- split_to_vector(collapsed)
    if (is_icd) unlumped <- remove_lumped_icd_codes(split) # # Step 4: Further split any remaining lumped ICD-10 codes
    if (!is_icd) unlumped <- split
    return(unlumped)
  }
  if (!is.null(clin_icd_cols) && length(clin_icd_cols) > 0) {
    result$clin_icd <- process_columns(clin_icd_cols, is_icd = TRUE)
  }
  if (!is.null(clin_rvs_cols) && length(clin_rvs_cols) > 0) {
    result$clin_rvs <- process_columns(clin_rvs_cols, is_icd = FALSE)
  }
  return(result)
}
```

### 5.2.0.append_copy_and_remove_icd_rvs.R
```r
append_copy_and_remove_icd_rvs <- function(col, clin_rvs, clin_icd) {
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)
  datatable[, matches := lapply(col, function(x) {
    Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = covid_env, ifnotfound = list(NULL))[[1]])
    }, x)
  })]
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique_matches <- Filter(function(code) {
      !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]])
    }, matches)
    updated_rvs <- c(unique_matches[!unique_matches %in% rvs], rvs)
    updated_rvs[!duplicated(updated_rvs)]
  }, clin_rvs, matches, SIMPLIFY = FALSE)]
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code, envir = icd_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = phil_icds_env, ifnotfound = list(NULL))[[1]])
    }, rvs_vec)
    updated_icd <- c(icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec], icd_vec)
    updated_icd[!duplicated(updated_icd)]
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]
  datatable[, clin_icd := icd_matches]
  datatable[, col := lapply(col, function(x) {
    Filter(function(code) {
      is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]])
    }, x)
  })]
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = covid_env, ifnotfound = list(NULL))[[1]])
    }, rvs_vec)
  })]
  datatable[, clin_rvs := mapply(function(rvs_vec, icd_vec) {
    rvs_codes_in_icd <- Filter(function(code) {
      (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = covid_env, ifnotfound = list(NULL))[[1]])
    }, icd_vec)
    c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec]) # Append unique RVS codes to the END of clin_rvs
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]
  datatable[, clin_icd := mapply(function(icd_vec, rvs_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code, envir = icd_codes_env, ifnotfound = list(NULL))[[1]]) ||
        !is.null(mget(code, envir = phil_icds_env, ifnotfound = list(NULL))[[1]])
    }, rvs_vec)
    c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec]) # Append unique ICD codes to the END of clin_icd
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      return(NA_character_)
    } else {
      return(unlist(x, recursive = TRUE, use.names = FALSE))
    }
  })]
  invalid_matches <- lapply(datatable$matches, function(x) {
    Filter(function(code) {
      is.null(mget(code, envir = rvs_codes_env, ifnotfound = list(NULL))[[1]])
    }, x)
  })
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    data.table::setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }
  return(list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col,
    discarded_rvs = discarded_table
  ))
}
```

### 5.3.0.remap_patient_data.R
```r
remap_patient_data <- function(
    pat_type,
    pat_memcat_parent,
    pat_memcat_child,
    clin_discharge,
    claim_status,
    known_values,
    remapped_column) {
  mapped <- list()
  unmapped <- list()
  remapped <- list()
  columns_to_remap <- list(
    pat_type = pat_type,
    pat_memcat_parent = pat_memcat_parent,
    pat_memcat_child = pat_memcat_child,
    clin_discharge = clin_discharge,
    claim_status = claim_status
  )
  for (col_name in names(columns_to_remap)) {
    column_data <- columns_to_remap[[col_name]]
    dt <- data.table(column_data = column_data)
    remapped_col <- eval(remapped_column, envir = list(dt = dt, column_name = "column_data"))
    unknown_values <- setdiff(
      column_data[!is.na(column_data)],
      known_values[[col_name]]
    )
    if (length(unknown_values) > 0) {
      warning(sprintf(
        "Unmapped values in column '%s': %s",
        col_name, paste(unknown_values, collapse = ", ")
      ))
    }
    remapped[[col_name]] <- remapped_col
    mapped[[col_name]] <- unique(data.table(
      Original = column_data,
      Mapped = remapped_col
    ))
    unmapped[[col_name]] <- unknown_values
  }
  return(
    list(
      remapped = remapped,
      pat_type_mapped = mapped$pat_type,
      pat_memcat_parent_mapped = mapped$pat_memcat_parent,
      pat_memcat_child_mapped = mapped$pat_memcat_child,
      clin_discharge_mapped = mapped$clin_discharge,
      claim_status_mapped = mapped$claim_status,
      pat_type_unmapped = unmapped$pat_type,
      memcat_parent_unmapped = unmapped$pat_memcat_parent,
      memcat_child_unmapped = unmapped$pat_memcat_child,
      discharge_unmapped = unmapped$clin_discharge,
      claim_status_unmapped = unmapped$claim_status
    )
  )
}
```

### 5.4.0.map_rvs_icd9.R
```r
map_rvs_icd9 <- function(clin_rvs, rvs = rvs_icd9) {
  without_drg_codes <- unique(rvs[is_drg == FALSE]$rvs)
  setorder(rvs, rvs)
  unique_rvs <- rvs[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)
  icd9_list <- lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mapped_icd9 <- unique(unlist(lapply(codes, function(code) {
      if (code %chin% names(rvs_map_solo)) {
        rvs_map_solo[[code]]
      } else if (code %chin% names(rvs_map_list)) {
        rvs_map_list[[code]]
      } else {
        NULL
      }
    })))
    if (length(mapped_icd9) > 0) mapped_icd9 else NA_character_
  })
  rvss <- unique(unlist(clin_rvs))
  mappable_rvs <- intersect(rvss, rvs$rvs)
  unmappable_rvs <- setdiff(rvss, rvs$rvs)
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  mapped_without_drg <- intersect(mappable_rvs, without_drg_codes)
  return_list <- list(
    icd9_list = icd9_list,
    rvs_map_list = rvs_map_list,
    rvss = rvss,
    mappable_rvs = mappable_rvs,
    unmappable_rvs = unmappable_rvs,
    multi_mapped_rvs = multi_mapped_rvs,
    without_drg = mapped_without_drg
  )
  return(return_list)
}
```

### 5.5.0.map_icd10.R
```r
map_icd10 <- function(c1, c2, clin_icd) {
  trim_code <- function(code) {
    sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
  }
  code_exists <- function(code, env) {
    !is.null(mget(code, envir = env, ifnotfound = list(NULL))[[1]])
  }
  generate_icd10_mapping <- function(filtered_icds) {
    icd_mapping <- list()
    direct_matches <- character() # Store all directly matched codes
    modifiedmatches <- list(modified_matches = character(), modified_match = character()) # Store original-modified pairs
    for (code in filtered_icds) {
      code <- trimws(code)
      if (code_exists(code, icd_codes_env)) {
        icd_mapping[[code]] <- list(match_type = "Exact", original = code, mapped = code)
        direct_matches <- c(direct_matches, code)
        next
      }
      if (nchar(code) == 3) {
        modified_code <- paste0(code, "9")
        if (code_exists(modified_code, icd_codes_env)) {
          icd_mapping[[code]] <- list(match_type = "Modified (Added 9)", original = code, mapped = modified_code)
          modifiedmatches$modified_matches <- c(modifiedmatches$modified_matches, code)
          modifiedmatches$modified_match <- c(modifiedmatches$modified_match, modified_code)
          next
        }
      }
      trimmed_code <- trim_code(code)
      match_found <- FALSE
      for (i in 0:(nchar(trimmed_code) - 3)) {
        partial_code <- substr(trimmed_code, 1, nchar(trimmed_code) - i)
        if (nchar(partial_code) >= 3 && code_exists(partial_code, icd_codes_env)) {
          icd_mapping[[code]] <- list(match_type = "Modified (Trimmed)", original = code, mapped = partial_code)
          modifiedmatches$modified_matches <- c(modifiedmatches$modified_matches, code)
          modifiedmatches$modified_match <- c(modifiedmatches$modified_match, partial_code)
          match_found <- TRUE
          break
        }
      }
      if (!match_found) {
        icd_mapping[[code]] <- list(match_type = "Unmatched", original = code, mapped = NA_character_)
      }
    }
    list(mapping = icd_mapping, modified_matches = modifiedmatches, direct_matches = direct_matches)
  }
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !sapply(icds, function(code) code_exists(code, covid_rvs_neoplasm_env))]
  mapping_info <- generate_icd10_mapping(filtered_icds)
  icd_mapping <- mapping_info$mapping
  unmatched_codes <- names(Filter(function(x) x$match_type == "Unmatched", icd_mapping))
  unmatchedsources <- rbindlist(
    lapply(c("c1", "c2", "clin_icd"), function(col_name) {
      col_values <- get(col_name)
      flattened_values <- unlist(col_values)
      valid_codes <- flattened_values[!is.na(flattened_values) & flattened_values != ""]
      if (length(valid_codes) > 0) {
        data.table(code = valid_codes, source = col_name)[, .(count = .N), by = .(code, source)]
      } else {
        data.table(code = character(), source = character(), count = integer())
      }
    }),
    fill = TRUE
  )
  unmatchedsources <- unmatchedsources[code %chin% unmatched_codes]
  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    thai_icd10 = sapply(icd_mapping, `[[`, "mapped"),
    match_type = sapply(icd_mapping, `[[`, "match_type")
  )
  apply_icd10_mapping <- function(codes) {
    unname(sapply(codes, function(code) {
      if (!is.null(icd_mapping[[code]]) && !is.null(icd_mapping[[code]]$mapped)) {
        icd_mapping[[code]]$mapped
      } else {
        code
      }
    }))
  }
  c1_mapped <- lapply(c1, apply_icd10_mapping)
  c2_mapped <- lapply(c2, apply_icd10_mapping)
  clin_icd_mapped <- lapply(clin_icd, apply_icd10_mapping)
  list(
    c1 = c1_mapped,
    c2 = c2_mapped,
    clin_icd = clin_icd_mapped,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    unmatched_codes = unmatched_codes,
    unmatched_sources = unmatchedsources,
    icd_mapping_res = icd_mapping,
    modified_matches = mapping_info$modified_matches,
    direct_matches = mapping_info$direct_matches
  )
}
```

### 5.6.0.find_pdx.R
```r
find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx,
                     neoplasmsdtactual = neoplasms_dt_actual,
                     acrrvs = acr_rvs, covidrvs = covid_rvs, seed = global_seed) {
  acc_pdx_set <- unique(accpdx)
  neoplasm_codes <- unique(neoplasmsdtactual$icd10)
  rvs_codes <- unique(acrrvs$rvs)
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }
  filter_icds <- function(codes) {
    codes <- codes[!is.na(codes) & !grepl("^[0-9]", codes) &
      !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
      !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
      !(codes %chin% covidrvs)]
    codes[codes %chin% acc_pdx_set]
  }
  result <- mapply(function(c1, c2, clin_icd) {
    c1_split <- filter_icds(unlist(strsplit(c1, "\\|")))
    c2_split <- filter_icds(unlist(strsplit(c2, "\\|")))
    clin_icd_split <- filter_icds(unlist(strsplit(clin_icd, "\\|")))
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        return(list(pdx = cr_list[1], pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)))
      }
    }
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      return(list(pdx = NA_character_, pdx_code = 99))
    }
    if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(sapply(starting_codes, check_similarity, y = cr))]
          return(list(pdx = best_match, pdx_code = 5))
        }
      }
    }
    set.seed(seed)
    list(pdx = sample(pdxs, 1), pdx_code = 6)
  }, c1, c2, clin_icd, SIMPLIFY = FALSE)
  pdx <- sapply(result, `[[`, "pdx")
  pdx_code <- sapply(result, `[[`, "pdx_code")
  list(pdx = pdx, pdx_code = pdx_code)
}
```

### 6.0.aggregate_all_summaries.R
```r
aggregate_all_summaries <- function(summaries) {
  combine_summaries <- function(summaries) {
    combined_summary <- list(
      rename_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "rename_success")))),
      ICD_replacements_1 = rbindlist(lapply(summaries, function(s) safe_access(s, "ICD_replacements_1")), fill = TRUE),
      ICD_replacements_2 = rbindlist(lapply(summaries, function(s) safe_access(s, "ICD_replacements_2")), fill = TRUE),
      pat_type_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "pat_type_unmapped"))),
      memcat_parent_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "memcat_parent_unmapped"))),
      memcat_child_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "memcat_child_unmapped"))),
      discharge_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "discharge_unmapped"))),
      claim_status_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "claim_status_unmapped"))),
      discard_rvs_one = rbindlist(lapply(summaries, function(s) safe_access(s, "discard_rvs_one")), fill = TRUE),
      discard_rvs_two = rbindlist(lapply(summaries, function(s) safe_access(s, "discard_rvs_two")), fill = TRUE),
      replacement_summary = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "replacement_summary")), fill = TRUE), "NA_character_"),
      empty_replaced_with_na_1 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "empty_replaced_with_na_1")), fill = TRUE), "NA_character_"),
      NA_replaced_with_empty_1 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_1")), fill = TRUE), "character(0)"),
      NA_replaced_with_empty_2 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_2")), fill = TRUE), "character(0)"),
      NA_replaced_with_empty_3 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_3")), fill = TRUE), "character(0)"),
      unique_icds = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unique_icds")))),
      direct_matches = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "direct_matches")))),
      modified_matches = extract_modified_matches(summaries),
      unmatched_codes = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmatched_codes")))),
      unmatched_sources = rbindlist(lapply(summaries, function(s) safe_access(s, "unmatched_sources")), fill = TRUE),
      rvss = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "rvss")))),
      mappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "mappable_rvs")))),
      unmappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmappable_rvs")))),
      multi_mapped_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "multi_mapped_rvs")))),
      without_drg = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "without_drg")))),
      pdx_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "pdx_success")))),
      icd10_map_dt = rbindlist(lapply(summaries, function(s) safe_access(s, "icd10_map_dt")), fill = TRUE),
      pat_type_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_type_mapped")), fill = TRUE),
      pat_memcat_parent_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_parent_mapped")), fill = TRUE),
      pat_memcat_child_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_child_mapped")), fill = TRUE),
      clin_discharge_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "clin_discharge_mapped")), fill = TRUE),
      claim_status_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "claim_status_mapped")), fill = TRUE)
    )
    return(combined_summary)
  }
  if (all(sapply(summaries, is.list))) {
    summaries <- lapply(summaries, function(part) {
      combine_summaries(part) # Aggregate chunks within the part
    })
  }
  combine_summaries(summaries)
}
```

### 7.0.print_summary_tables.R
```r
print_summary_tables <- function(final_combined_summaries) {
  summary <- final_combined_summaries
  if (summary$rename_success) cat("\nRename Success:\n", summary$rename_success, "\n") else stop(paste0("Rename failed for ", year_to_load, "."))
  final_icd_replacements <- rbind(
    summary$ICD_replacements_1,
    summary$ICD_replacements_2
  )
  final_icd_replacements <- unique(final_icd_replacements)
  print_icd_normalized_table <- function(icd_replacements) {
    relevant_columns <- c("old_code", "new_code", "count")
    unique_replacements <- icd_replacements[, ..relevant_columns]
    if (nrow(unique_replacements) > 0) {
      print(knitr::kable(
        unique_replacements,
        format = "markdown",
        caption = "ICD Normalized Text for clin c1 & c2 Before Splitting
(Note: differences of only one period symbol are ignored)"
      ))
    } else {
      cat("\nNo ICD replacements found.\nNote: periods and whitespaces are ignored.\n")
    }
  }
  print_icd_normalized_table(unique(final_icd_replacements))
  display_mappings <- function(mapped_data, mapping_name) {
    if (!all(c("Original", "Mapped") %in% names(mapped_data))) {
      stop("Data must contain 'Original' and 'Mapped' columns.")
    }
    unique_mappings <- unique(mapped_data)
    print(knitr::kable(unique_mappings,
      format = "markdown",
      caption = sprintf("Unique Mappings for %s", mapping_name)
    ))
  }
  display_mappings(summary$pat_type_mapped, "Patient Type")
  display_mappings(summary$pat_memcat_parent_mapped, "Memcat Parent")
  display_mappings(summary$pat_memcat_child_mapped, "Memcat Child")
  display_mappings(summary$clin_discharge_mapped, "Discharge")
  display_mappings(summary$claim_status_mapped, "Claim Status")
  discarded_rvs_one <- combine_discarded_rvs_tables(list(summary), "discard_rvs_one")
  discarded_rvs_two <- combine_discarded_rvs_tables(list(summary), "discard_rvs_two")
  final_discard_rvs <- rbind(
    discarded_rvs_one,
    discarded_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]
  if (nrow(final_discard_rvs) > 0) {
    print(knitr::kable(final_discard_rvs,
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
  }
  display_replacements <- function(data, set_name, replace_with) {
    formatted_data <- final_combine_replace_tables(data, replace_with)
    replacement_desc <- if (replace_with == "NA_character_") {
      "Empty Strings Replaced"
    } else {
      "NA Strings Replaced"
    }
    if (nrow(formatted_data) > 0) {
      print(knitr::kable(
        formatted_data,
        format = "markdown",
        caption = paste0(replacement_desc, " (", set_name, " Set)")
      ))
    } else {
      cat(paste0("\nNo ", replacement_desc, " in the ", set_name, " set.\n\n"))
    }
  }
  display_replacements(summary$replacement_summary, "Zeroth", "NA_character_")
  display_replacements(summary$empty_replaced_with_na_1, "First", "NA_character_")
  display_replacements(summary$NA_replaced_with_empty_1, "Second", "character(0)")
  display_replacements(summary$NA_replaced_with_empty_2, "Third", "character(0)")
  display_replacements(summary$NA_replaced_with_empty_3, "Fourth", "character(0)")
  cat(sprintf("There are %d unique potential RVS codes in clin_rvs.\n", length(summary$rvss)))
  cat(sprintf("There are %d valid RVS codes without an ICD-9CM equivalent.\n", length(summary$without_drg)))
  cat(sprintf(
    "%d (%.2f%%) valid codes have an ICD-9-CM mapping.\n",
    length(summary$mappable_rvs),
    (length(summary$mappable_rvs) / length(summary$rvss)) * 100
  ))
  cat(sprintf(
    "%d (%.2f%%) valid codes have multiple ICD-9 equivalents.\n",
    length(summary$multi_mapped_rvs),
    (length(summary$multi_mapped_rvs) / length(summary$rvss)) * 100
  ))
  cat(
    sprintf("\nThere are %d unique ICD-10 codes.\n", length(summary$unique_icds)),
    sprintf(
      "%d (%.2f%%) are directly in the Thai ICD-10 library.\n",
      length(summary$direct_matches), (length(summary$direct_matches) / length(summary$unique_icds)) * 100
    ),
    sprintf(
      "Total %d codes were mapped to the Thai ICD10 library.\n",
      length(summary$unique_icds) - length(summary$unmatched_codes)
    ),
    sprintf(
      "%d codes were modified to match.\n",
      length(summary$unique_icds) - length(summary$unmatched_codes) - length(summary$direct_matches)
    ),
    sprintf("%d codes could not be mapped.\n", length(summary$unmatched_codes))
  )
  print_modified_icd10_codes <- function(summary) {
    unique_icd10_map <- summary$modified_matches
    if (!is.data.table(unique_icd10_map)) {
      unique_icd10_map <- as.data.table(unique_icd10_map)
    }
    if (nrow(unique_icd10_map) > 0) {
      if (!"char_diff" %in% names(unique_icd10_map)) {
        unique_icd10_map[, char_diff := abs(nchar(modified_matches) - nchar(modified_match))]
      }
      unique_icd10_map <- unique_icd10_map[order(-char_diff)]
      print(knitr::kable(
        unique_icd10_map,
        format = "markdown",
        caption = "Modified ICD-10 Codes"
      ))
      cat("nrow Modified ICD-10 Codes: ", nrow(unique_icd10_map), "\n")
    } else {
      cat("\nNo modified ICD-10 codes found.\n\n")
    }
  }
  print_modified_icd10_codes(summary)
  if (length(summary$unmatched_codes) > 0) {
    unmatched_codes <- data.table(
      code = summary$unmatched_codes
    )[, .(count = .N), by = code][order(-count)] # Aggregate by code and order by count
    print(knitr::kable(unmatched_codes,
      format = "markdown",
      caption = "Invalid ICD-10 Codes"
    ))
    cat("\nnrow invalid ICD-10 codes: ", nrow(unmatched_codes), "\n")
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }
  if (summary$pdx_success) cat("\nAll PDx's are in the list of acceptable PDx's:\n", summary$pdx_success, "\n") else stop(paste0("Not all pdx are in acceptable pdxs for ", year_to_load, "."))
}
```

## R Notebooks

### 00b-drg-partial.ipynb
```r
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow"
)
github_packages <- c("r-lib/styler")
n_cores <- parallel::detectCores()
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes", Ncpus = n_cores)
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
for (file in c(
  "0.1.0.params_fpaths.R",
  "2.0.split_and_save_part.R",
  "0.3.0.summary_helper_functions.R"
)) {
  source(here::here("data-cleaning/r_scripts_v2/", file))
}
gcs_base <- "gs://phic-claims-raw/"
to_hash <- FALSE # Flag to enable or disable hashing
escape_spaces <- function(path) {
  gsub(" ", "\\\\ ", path)
}
process_file <- function(year_to_load) {
  file_type <- if (year_to_load %in% c(2022, 2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year_to_load, file_type)
  gcs_path <- paste0(gcs_base, file_name)
  file_path <- here::here(raw_claims_path, file_name)
  file_path_escaped <- escape_spaces(file_path)
  md5_rds_path <- here::here(raw_claims_md5_path, paste0(year_to_load, "_md5.rds"))
  if (file.exists(file_path) && (to_hash == FALSE || file.exists(md5_rds_path))) {
    if (to_debug) message(paste("File", file_name, "and its MD5 already exist. Skipping download and hash generation.\n"))
    return(NULL)
  }
  if (!file.exists(file_path)) {
    if (!is.null(gcp_proj) && gcp_proj == "drg-pipeline") {
      system(paste("gsutil cp", escape_spaces(gcs_path), file_path_escaped), intern = FALSE, ignore.stderr = FALSE)
    } else {
      stop("Error: GCP Project is not null and is not drg-pipeline")
    }
  }
  if (!to_hash) {
    if (to_debug) message(paste("Hashing disabled. Skipping MD5 operations for", file_name, "\n"))
    return(NULL)
  }
  md5_hex <- digest::digest(file(file_path, "rb"), algo = "md5", file = TRUE)
  md5_raw <- as.raw(as.numeric(strtoi(substring(md5_hex, seq(1, nchar(md5_hex), 2), seq(2, nchar(md5_hex), 2)), 16)))
  md5_base64 <- base64enc::base64encode(md5_raw)
  local_md5 <- list(md5_hex = md5_hex, md5_base64 = md5_base64)
  saveRDS(local_md5, file = md5_rds_path)
  if (to_debug) {
    cat("Generated MD5 for local file:\n")
    print(local_md5)
  }
  gcs_md5_output <- system(paste("gsutil hash", escape_spaces(gcs_path)), intern = TRUE)
  md5_line <- gcs_md5_output[grepl("Hash \\(md5\\):", gcs_md5_output)]
  if (length(md5_line) == 0) {
    stop(paste("GCS MD5 hash not found for", file_name))
  } else if (!grepl(md5_base64, md5_line)) {
    stop(paste("Mismatch detected for", file_name))
  } else {
    message(paste("MD5 match confirmed for", file_name))
  }
  rm(md5_hex, md5_raw, local_md5, gcs_md5_output, md5_line)
  gc()
}
years <- 2018:2023
for (year in years) {
  process_file(year)
}
cat("All files are up-to-date and verified.\n")
print(split_parts)
for (year_to_load in c(2018:2023)) {
  year_to_load <<- year_to_load
  invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
  full_header <- data.table::fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE
  )
  partial_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year_to_load,
    "_part_", sprintf("%02d", split_parts),
    "_of_", split_parts, ".rds"
  ))
  if (!file.exists(partial_file)) {
    full_file <- data.table::fread(
      file = full_claims_file, colClasses = "character",
      header = TRUE, encoding = "Latin-1", sep = separator
    )
  }
  parallel::mclapply(
    1:split_parts,
    split_and_save_part,
    mc.cores = nthreads / 2
  )
  print_memory_usage_gb <- function(env = .GlobalEnv) {
    obj_names <- ls(envir = env)
    obj_sizes <- sapply(obj_names, function(x) object.size(get(x, envir = env)) / (1024^3)) # Convert bytes to GB
    obj_info <- data.frame(
      Object = obj_names,
      Size_GB = round(obj_sizes, 3) # Round to 3 decimal places for readability
    )
    obj_info <- obj_info[obj_info$Size_GB > 0, ]
    obj_info <- obj_info[order(obj_info$Size_GB, decreasing = TRUE), ]
    print(obj_info, row.names = FALSE)
  }
  print_memory_usage_gb()
  if (exists("full_file")) rm(full_file)
  invisible(gc())
}
hash_cache_dir <- here::here("data-cleaning/cache/partial_md5")
dir.create(hash_cache_dir, recursive = TRUE, showWarnings = FALSE)
calculate_md5 <- function(file_path) {
  md5sum <- digest::digest(file = file_path, algo = "md5")
  return(md5sum)
}
check_md5_changes <- function(year_to_load) {
  hash_file_path <- here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds"))
  current_hashes <- sapply(1:split_parts, function(part) {
    part_file <- here::here(
      raw_claims_parts_path,
      paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
    )
    calculate_md5(part_file)
  })
  if (file.exists(hash_file_path)) {
    saved_hashes <- readRDS(hash_file_path)
    if (identical(saved_hashes, current_hashes)) {
      message(paste("No changes in partial files for year", year_to_load))
      return(TRUE)
    }
  }
  return(FALSE)
}
check_and_save_md5 <- function(year_to_load) {
  if (check_md5_changes(year_to_load)) {
    return(TRUE)
  }
  total_rows_check <- 0
  for (part in 1:split_parts) {
    part_rows <- nrow(
      readRDS(
        here::here(
          raw_claims_parts_path,
          paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
        )
      )
    )
    total_rows_check <- total_rows_check + part_rows
  }
  expected_total_rows <- readRDS(here::here("data-cleaning/cache/total_rows", paste0("total_rows_", year_to_load, ".rds")))
  if (total_rows_check == expected_total_rows) {
    message(paste("Row count matches for year", year_to_load, "- saving MD5 hashes."))
    current_hashes <- sapply(1:split_parts, function(part) {
      part_file <- here::here(
        raw_claims_parts_path,
        paste0(full_claims_prefix, year_to_load, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
      )
      calculate_md5(part_file)
    })
    saveRDS(current_hashes, here::here(hash_cache_dir, paste0("md5_hashes_", year_to_load, ".rds")))
    return(TRUE)
  } else {
    message(paste("Row count mismatch for year", year_to_load, "- skipping MD5 save."))
    return(FALSE)
  }
}
results <- parallel::mclapply(2018:2023, check_and_save_md5, mc.cores = nthreads)
if (all(unlist(results))) {
  message("All row counts match and MD5 hashes are updated.")
} else {
  message("Discrepancies found in row counts or updates.")
}
invisible(source(here::here("data-cleaning/r_scripts_v2/3.0.create_sample_files.R")))
for (sample_size_divisor in c(625, 125, 25, 5)) {
  sample_size_divisor <<- sample_size_divisor
  message(paste0("Starting sampling for size ÷", sample_size_divisor))
  for (year_to_load in c(2018:2023)) {
    year_to_load <<- year_to_load
    message(paste0("Generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))
    invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
    parallel::mclapply(
      1:split_parts,
      function(mclapply_part) {
        sampled_claims_file <- here::here(raw_claims_samples_path, paste0(
          "sampled_claims_", year_to_load, "_", sample_size_divisor,
          "_part_", sprintf("%02d", mclapply_part), "_of_", split_parts, ".rds"
        ))
        if (!file.exists(sampled_claims_file)) {
          create_sample_files(mclapply_part, sampled_claims_file)
        }
      },
      mc.cores = nthreads
    )
    message(paste0("Done generating samples of size ÷", sample_size_divisor, " for year ", year_to_load))
  }
  message(paste0("Finished sampling for size ÷", sample_size_divisor, " for all years"))
}
```

### 01-drg-cleaning-v2.ipynb
```r
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow"
)
github_packages <- c("r-lib/styler")
n_cores <- parallel::detectCores()
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
install_from_github <- function(repo) {
  package_name <- basename(repo)
  if (!require(package_name, character.only = TRUE)) {
    if (!require("remotes", character.only = TRUE)) {
      install.packages("remotes", Ncpus = n_cores)
    }
    message("Installing ", package_name, " from GitHub (", repo, ")")
    remotes::install_github(repo, Ncpus = n_cores)
  } else {
    if (verbose_output) message("Loading ", package_name)
  }
  library(package_name, character.only = TRUE)
}
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
year_to_load <- 2018
for (file in list.files(here::here("data-cleaning/r_scripts_v2"), pattern = "\\.R$", full.names = TRUE)) invisible(source(file))
message(year_to_load)
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- FALSE # Set to TRUE to print mapping data tables
load_or_query <- function(query, var_name) {
  rds_path <- here(cache_path, "mapping", paste0(var_name, ".rds"))
  if (to_use_cache && file.exists(rds_path)) {
    if (verbose_output) message("Loading ", var_name, " from cache...")
    return(readRDS(rds_path))
  } else {
    if (verbose_output) message("Querying ", var_name, " from BigQuery...")
    dt <- query_bq_to_dt(query)
    saveRDS(dt, rds_path) # Save queried data to .rds cache file
    return(dt)
  }
}
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}
proc_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.proc`")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type
rvs_icd9_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_rvs_map`")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(
  rvs_icd9,
  proc[, .(CODE, DRGUSE)], # Select CODE and DRGUSE columns for merging
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][
  !is.na(rvs) & !is.na(icd9cm), -"DRGUSE"
]
acr_rvs_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_procedure`")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")
i10_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10`")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  assign(code, TRUE, envir = acc_pdx_env)
}
phl_icd10_query <- paste0("SELECT * FROM `", gcp_proj, ".icd.phl_icd10`")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")
neoplasms_dt_actual <- as.data.table(phl_icd10[
  grepl("/", icd10), .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])
i10vx_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10vx`")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)
hci_query <- paste0("SELECT * FROM `", gcp_proj, ".hci.temp_hci`")
hci <- load_or_query(hci_query, "hci")
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")
phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}
proc_env <- create_env_from_vector(proc$CODE)
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)
acc_pdx_env <- create_env_from_vector(acc_pdx)
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)
acc_icd_env <- create_env_from_vector(i10vx$code)
hci_env <- create_env_from_vector(hci$id_hci)
neoplasm_env <- create_env_from_vector(neoplasm_codes)
covid_env <- create_env_from_vector(covid_codes)
rvs_codes_env <- create_env_from_vector(rvs_codes)
phil_icds_env <- create_env_from_vector(phil_icds)
icd_codes_env <- create_env_from_vector(icd_codes)
covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)
covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)
save_all_data_to_file <- function(file_path, ...) {
  args <- list(...)
  sink(file_path) # Redirect output to the specified file
  cat("\n--- All Data Tables in One View ---\n") # Header for the file
  for (name in names(args)) {
    cat("\n---", name, "---\n") # Print table name as a header within the file
    print(args[[name]], nrow = Inf, max.print = Inf)
  }
  sink() # Stop redirecting output to the file
  if (verbose_output) message("All data tables saved to ", file_path) # Confirmation message
}
output_file <- here(debug_path, "mapping_data.txt")
if (to_print_mapping_data) {
  options(max.print = 999999)
  save_all_data_to_file(
    output_file,
    grouper_v5_proc = proc,
    phic_acr_rvs_map = rvs_icd9,
    phic_acr_procedure = acr_rvs,
    grouper_v5_i10 = tdrg_icd10,
    acc_pdx = acc_pdx,
    icd_phl_icd10 = phl_icd10,
    neoplasms_dt_actual = neoplasms_dt_actual,
    grouper_v5_i10vx = i10vx,
    acc_icd = acc_icd,
    hci_temp_hci = hci
  )
  options(max.print = 1000)
}
for (loop_part in 1:split_parts) {
  start_time <- Sys.time() # Record start time for processing
  cat(paste0("\rStart reading part ", loop_part, " of ", split_parts))
  flush.console()
  read_result <- read_appropriate_file(loop_part)
  read_in_dt <- read_result$read_result_dt
  read_in_replacement_summary <- read_result$read_result_replacement_summary
  cat(paste0("\rFinished reading part ", loop_part, " of ", split_parts))
  flush.console()
  cat(paste0("\rStart chunking part ", loop_part, " of ", split_parts))
  flush.console()
  chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
  chunks <- split(
    read_in_dt,
    rep(
      1:nthreads,
      each = chunk_size,
      length.out = nrow(read_in_dt)
    )
  )
  cat(paste0("\rFinished chunking part ", loop_part, " of ", split_parts))
  flush.console()
  cat(paste0("\rStart processing part ", loop_part, " of ", split_parts))
  flush.console()
  if (to_parallel) {
    parallel_results <- mclapply(
      chunks, process_chunk,
      mc.cores = nthreads
    )
  } else {
    if (!to_debug) parallel_results <- lapply(chunks, process_chunk) else parallel_results <- list(process_chunk(chunks[[1]]))
  }
  rbound_dt <- rbindlist(lapply(
    parallel_results,
    function(res) {
      res$return_chunk
    }
  ))
  invalid_pdx_indices <- which(
    !is.na(rbound_dt$pdx) & rbound_dt$pdx != "" &
      !sapply(rbound_dt$pdx, function(x) exists(x, acc_pdx_env))
  )
  if (length(invalid_pdx_indices) > 0) {
    message(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
    pdx_success_list[[loop_part]] <- FALSE
  } else {
    pdx_success_list[[loop_part]] <- TRUE
  }
  for (i in seq_along(parallel_results)) {
    parallel_results[[i]]$return_summary$pdx_success <- pdx_success_list[[loop_part]]
    parallel_results[[i]]$return_summary$replacement_summary <- read_in_replacement_summary
  }
  parallel_summaries <- lapply(
    parallel_results,
    function(res) {
      res$return_summary
    }
  )
  summarized_dt <- rbound_dt # Store the summarized data
  combined_chunk_summary[[loop_part]] <- parallel_summaries
  if (to_write) {
    saveRDS(
      summarized_dt, here(checkpoint_1_path, paste0(
        checkpoint_1_prefix, year_to_load, suffix,
        "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      )),
      compress = TRUE
    )
  }
  all_parts_summaries[[loop_part]] <- combined_chunk_summary[[loop_part]]
  processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(),
    start_time,
    units = "secs"
  ))
  print_status_update(loop_part, split_parts, processing_times, "clean")
  if (loop_part == 1) dim_dt <- dim(summarized_dt)
  nrow_end[[loop_part]] <- nrow(summarized_dt)
  rm(read_in_dt, rbound_dt, summarized_dt)
  invisible(gc())
}
if (to_post_cleaning_checks) print_summary_tables(aggregate_all_summaries(all_parts_summaries))
master_dt_list <- parallel::mclapply(1:split_parts, function(read_part) {
  cat(paste("\rStarted reading part", read_part))
  flush.console()
  return_dt <- readRDS(here(checkpoint_1_path, paste0(
    checkpoint_1_prefix, year_to_load, suffix,
    "part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
  )))
  cat(paste("\rFinished reading prt", read_part))
  flush.console()
  return(return_dt)
}, mc.cores = nthreads)
message("Commencing rbindlist")
master_dt <- rbindlist(master_dt_list, fill = TRUE)
rm(master_dt_list)
invisible(gc())
message("Finished rbindlist")
total_start_rows <- 0
total_end_rows <- 0
for (nrow_part in 1:split_parts) {
  total_start_rows <- total_start_rows + nrow_start[[nrow_part]]
  total_end_rows <- total_end_rows + nrow_end[[nrow_part]]
  if (nrow_start[[nrow_part]] != nrow_end[[nrow_part]]) {
    warning(
      "WARNING: Row Count Mismatch! Part ", nrow_part,
      " has ", nrow_start[[nrow_part]], " starting rows and ",
      nrow_end[[nrow_part]], " ending rows\n"
    )
    stop("ERROR: Row Count Mismatch")
  }
}
if (if (to_sample) total_rows / sample_size_divisor else total_rows == nrow(master_dt)) {
  message("\nRow Counts Match for All Parts and Sum to Total Rows\n")
} else {
  stop("ERROR: Total Row Count Mismatch")
}
if (to_write) {
  message("Commencing saveRDS")
  saveRDS(master_dt, here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ), compress = TRUE)
  message("Finished saveRDS")
}
if (exists("master_dt")) {
  message("master_dt exists, making a copy and deleting it")
  result <- data.table::copy(master_dt)
  rm(master_dt)
  invisible(gc())
  message("copied master_dt to result, deleted master_dt")
} else {
  message(paste0("master_dt doesn't exist, reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
  result <- readRDS(here(
    checkpoint_2_path, paste0(
      checkpoint_2_prefix, year_to_load, suffix, ".rds"
    )
  ))
  invisible(gc())
  message(paste0("finished reading ", paste0(
    checkpoint_2_prefix, year_to_load, suffix, ".rds"
  )))
}
message(paste0("Saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
), compress = TRUE)
message(paste0("Finished saving ", paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")))
data <- readRDS("/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_2_master_clean_claims/checkpoint_2_claims_2018_sampled_625_prefinal.rds")
fwrite(data, "test.csv")
if (to_post_cleaning_checks) {
  acc_pdx_set <- unique(acc_pdx)
  dt <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  str(dt)
  not_in_acc_pdx <- dt$clin_pdx[!dt$clin_pdx %in% acc_pdx_set & !is.na(dt$clin_pdx)]
  all_in_acc_pdx <- length(not_in_acc_pdx) == 0
  if (all_in_acc_pdx) {
    message("All entries in dt$clin_pdx are in acc_pdx.")
  } else {
    message("Not all entries in dt$clin_pdx are in acc_pdx. Entries not in acc_pdx are:")
    print(unique(not_in_acc_pdx)) # Print unique entries not in acc_pdx
  }
  rm(all_in_acc_pdx, not_in_acc_pdx)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  count_data <- dt[, .N, by = clin_pdx_source]
  setorder(count_data, clin_pdx_source)
  count_data[, clin_pdx_source := factor(clin_pdx_source, levels = c(1, 2, 3, 6, 99))]
  total_count <- sum(count_data$N)
  ggplot(count_data, aes(x = clin_pdx_source, y = N)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Histogram of clin_pdx_source", x = "clin_pdx_source", y = "Count") +
    theme_minimal() +
    scale_x_discrete(drop = FALSE) + # Ensures all categories are shown
    geom_text(aes(label = N), vjust = -0.5) + # Display count above each bar
    annotate("text", x = Inf, y = -Inf, label = paste("Total N =", total_count), hjust = 1.1, vjust = -1.5) # Display total count below
  rm(count_data, total_count)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  output <- capture.output({
    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("dt\n")
    str(dt)
    cat("Structure of non-empty elements in each specified column:\n\n")
    cat("c1:\n")
    str(dt[!is.na(c1) & sapply(c1, function(x) length(x) > 0 && any(nzchar(x)))]$c1)
    cat("\nc2:\n")
    str(dt[!is.na(c2) & sapply(c2, function(x) length(x) > 0 && any(nzchar(x)))]$c2)
    cat("\nclin_sdx:\n")
    str(dt[!is.na(clin_sdx) & sapply(clin_sdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_sdx)
    cat("\nclin_pdx:\n")
    str(dt[!is.na(clin_pdx) & sapply(clin_pdx, function(x) length(x) > 0 && any(nzchar(x)))]$clin_pdx)
    cat("\nclin_proc:\n")
    str(dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]$clin_proc)
  })
  cat(paste(output, collapse = "\n"))
  rm(output)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  unique_values <- unique(unlist(dt$clin_rvs))
  unique_values <- unique_values[!is.na(unique_values) & unique_values != "NA"]
  matched_values <- unique_values[unique_values %in% rvs_icd9$rvs]
  cat(paste(matched_values, collapse = "\n"))
  rm(unique_values, matched_values)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  non_empty_clin_proc_rows <- dt[!is.na(clin_proc) & sapply(clin_proc, function(x) length(x) > 0 && any(nzchar(x)))]
  print(non_empty_clin_proc_rows)
  rm(non_empty_clin_proc_rows)
  invisible(gc())
}
if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  setkey(result, NULL) # Removes any existing key
  print(result[id_series %like% "e"])
  print(result[id_pin %like% "e"])
  print(result[id_hci %like% "e"])
  flattened_clin_sdx <- unlist(result$clin_sdx, use.names = FALSE, recursive = TRUE)
  if ("A" %chin% flattened_clin_sdx) {
    cat("Found 'A' in clin_sdx\n")
    rows_with_A <- result[sapply(result$clin_sdx, function(x) any("A" %chin% x))]
    for (i in seq_len(nrow(rows_with_A))) {
      print(rows_with_A[i])
    }
  } else {
    cat("No 'A' found in clin_sdx\n")
  }
}
if (to_post_cleaning_checks) {
  result <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
  ))
  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )
  rows_with_old_dates <- result[Reduce(`|`, lapply(
    .SD,
    function(x) x < as.Date("1900-01-01")
  )), .SDcols = date_cols]
  print(rows_with_old_dates)
}
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "prefinal", ".rds")
))
print(nrow(result))
print(nrow(result))
result[, is_covid := {
  covid_found <- rep(FALSE, .N)
  not_found <- !covid_found
  covid_found[not_found] <- clin_c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- clin_c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_rvs[not_found], function(row) any(row %chin% covid_rvs))
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_sdx[not_found], function(row) any(row %chin% covid_rvs))
  not_found <- !covid_found
  covid_found[not_found] <- sapply(clin_proc[not_found], function(row) any(row %chin% covid_rvs))
  covid_found
}]
print(nrow(result))
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))
if (to_post_cleaning_checks) {
  before <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, ".rds")
  ))
  after <- readRDS(here(
    checkpoint_2_path,
    paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
  ))
  setkey(before, id_series)
  setkey(after, id_series)
  pat_age_diff_na <- before[after,
    on = .(id_series), nomatch = 0,
    .(id_series, pat_bdate,
      pat_age_before = x.pat_age,
      pat_age_after = i.pat_age
    ),
    by = .EACHI
  ]
  pat_age_diff_na <- pat_age_diff_na[
    (is.na(pat_age_before) & !is.na(pat_age_after)) |
      (!is.na(pat_age_before) & is.na(pat_age_after)) |
      (pat_age_before != pat_age_after)
  ]
  cat("Rows where pat_age is NA in one table but
not in the other, or where the values differ:\n")
  print(pat_age_diff_na)
  rm(before, after)
  invisible(gc())
}
message("Reading final")
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final", ".rds")
))
message("Finished reading final, commencing subsetting")
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm, time_adm,
  date_dis, time_dis, date_rec, date_ref, date_check, pat_type, pat_rel, pat_bdate,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
message("Finished subsetting, commencing saveRDS")
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_time", ".rds")
))
message("Finished saveRDS, commencing subsetting")
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp, date_adm,
  date_dis, date_rec, date_ref, date_check, pat_type, pat_rel,
  pat_age, pat_ageday, pat_sex, pat_bwt, pat_memcat_parent,
  pat_memcat_child, claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
message("Finished subsetting, commencing saveRDS")
saveRDS(result, here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))
message("Finished saveRDS")
result <- readRDS(here(
  checkpoint_2_path,
  paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset", ".rds")
))
if (to_bq) {
  if (!to_sample) bq_table <- paste0("claims_", year_to_load)
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        stop(e)
      }
    }
  )
  tryCatch(
    {
      bq_table_create(
        bq_table(gcp_proj, bq_dataset, bq_table),
        fields = fromJSON(here(
          "data-cleaning/r_scripts_v2",
          "bq_schema_cleaning.json"
        ), simplifyDataFrame = FALSE)
      )
      message("Table created successfully.\n")
    },
    error = function(e) {
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        stop(e)
      }
    }
  )
  if (to_write) {
    chunk_size <- 250000 # Adjust the chunk size based on memory availability
    num_chunks <- ceiling(nrow(result) / chunk_size)
    for (i in seq_len(num_chunks)) {
      cat(paste("\rUploading chunk no.:", i))
      flush.console()
      chunk <- result[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result)),
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
}
```

### 02-drg-grouping-v2.ipynb
```r
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", "here", "tictoc", "stringr", "stringi", "lubridate",
  "profvis", "hash", "future", "future.apply", "knitr", "htmlwidgets",
  "parallelly", "stringdist", "parallel", "reticulate", "bigrquery",
  "jsonlite", "googleCloudStorageR", "haven", "fst", "httr", "ggplot2",
  "rmarkdown", "digest", "base64enc", "arrow"
)
github_packages <- c("r-lib/styler")
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message("Installing ", package)
    install.packages(package, dependencies = TRUE)
  } else {
    if (verbose_output) message("Loading ", package)
  }
  library(package, character.only = TRUE)
}
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
message("Installing/loading required CRAN packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(required_packages, install_and_load)
  )
)
message("Installing/loading required GitHub packages...")
invisible(
  suppressPackageStartupMessages(
    lapply(github_packages, install_from_github)
  )
)
nthreads <- parallelly::availableCores()
scripts_path <- here("data-cleaning/r_scripts_v2")
r_files <- list.files(scripts_path, pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  if (verbose_output) message(Sys.time(), " Sourcing: ", file)
  invisible(source(file))
}
message(year_to_load)
bq_dataset <- "drg_claims"
for (year in 2018:2023) {
  file_type <- if (year %in% c(2022:2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year, file_type)
  bq_name <- paste0(full_claims_bq_prefix, year, file_type)
  file_path <- here(raw_claims_path, file_name)
  exists <- file.exists(file_path)
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
  }
}
to_use_cache <- TRUE # Set to TRUE to enable saving and loading of .rds files
to_print_mapping_data <- TRUE # Set to TRUE to print mapping data tables
load_or_query <- function(query, var_name) {
  rds_path <- here(cache_path, "mapping", paste0(var_name, ".rds"))
  if (to_use_cache && file.exists(rds_path)) {
    if (verbose_output) message("Loading ", var_name, " from cache...")
    return(readRDS(rds_path))
  } else {
    if (verbose_output) message("Querying ", var_name, " from BigQuery...")
    dt <- query_bq_to_dt(query)
    saveRDS(dt, rds_path) # Save queried data to .rds cache file
    return(dt)
  }
}
if (to_print_mapping_data) {
  print_all <- function(dt, title) {
    cat("\n---", title, "---\n") # Print table title
    print(dt, nrow = Inf) # Print all rows of the data.table
  }
}
proc_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.proc`")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type
rvs_icd9_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_rvs_map`")
rvs_icd9 <- load_or_query(rvs_icd9_query, "rvs_icd9")
rvs_icd9 <- rvs_icd9[, .(
  rvs = as.character(rvs),
  icd9cm = as.character(as.numeric(icd9cm) * 100)
)]
rvs_icd9 <- merge(
  rvs_icd9,
  proc[, .(CODE, DRGUSE)], # Select CODE and DRGUSE columns for merging
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][
  !is.na(rvs) & !is.na(icd9cm), -"DRGUSE"
]
acr_rvs_query <- paste0("SELECT * FROM `", gcp_proj, ".phic_libraries.acr_procedure`")
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")
i10_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10`")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  assign(code, TRUE, envir = acc_pdx_env)
}
phl_icd10_query <- paste0("SELECT * FROM `", gcp_proj, ".icd.phl_icd10`")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")
neoplasms_dt_actual <- as.data.table(phl_icd10[
  grepl("/", icd10), .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])
i10vx_query <- paste0("SELECT * FROM `", gcp_proj, ".grouper_v5.i10vx`")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)
hci_query <- paste0("SELECT * FROM `", gcp_proj, ".hci.temp_hci`")
hci <- load_or_query(hci_query, "hci")
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")
phil_icds <- unique(gsub("[^A-Za-z0-9]", "", phl_icd10[!grepl("/", icd10), icd10]))
icd_codes <- unique(tdrg_icd10$CODE)
create_env_from_vector <- function(vec) {
  env <- new.env(parent = emptyenv())
  list2env(setNames(as.list(rep(TRUE, length(vec))), vec), envir = env)
  return(env)
}
proc_env <- create_env_from_vector(proc$CODE)
rvs_env <- create_env_from_vector(rvs_icd9$rvs)
icd9cm_env <- create_env_from_vector(rvs_icd9$icd9cm)
acr_rvs_env <- create_env_from_vector(acr_rvs$rvs)
acc_pdx_env <- create_env_from_vector(acc_pdx)
phl_icd10_env <- create_env_from_vector(phl_icd10$icd10)
acc_icd_env <- create_env_from_vector(i10vx$code)
hci_env <- create_env_from_vector(hci$id_hci)
neoplasm_env <- create_env_from_vector(neoplasm_codes)
covid_env <- create_env_from_vector(covid_codes)
rvs_codes_env <- create_env_from_vector(rvs_codes)
phil_icds_env <- create_env_from_vector(phil_icds)
icd_codes_env <- create_env_from_vector(icd_codes)
covid_neoplasm_codes <- unique(c(covid_codes, neoplasm_codes))
covid_neoplasm_env <- create_env_from_vector(covid_neoplasm_codes)
covid_rvs_neoplasm_codes <- unique(c(covid_codes, rvs_codes, neoplasm_codes))
covid_rvs_neoplasm_env <- create_env_from_vector(covid_rvs_neoplasm_codes)
covid_rvs_neoplasm_pattern <- paste(
  c(covid_codes, rvs_codes, neoplasm_codes),
  collapse = "|"
)
save_all_data_to_file <- function(file_path, ...) {
  args <- list(...)
  sink(file_path) # Redirect output to the specified file
  cat("\n--- All Data Tables in One View ---\n") # Header for the file
  for (name in names(args)) {
    cat("\n---", name, "---\n") # Print table name as a header within the file
    print(args[[name]], nrow = Inf, max.print = Inf)
  }
  sink() # Stop redirecting output to the file
  if (verbose_output) message("All data tables saved to ", file_path) # Confirmation message
}
output_file <- here(debug_path, "mapping_data.txt")
if (to_print_mapping_data) {
  options(max.print = 999999)
  save_all_data_to_file(
    output_file,
    grouper_v5_proc = proc,
    phic_acr_rvs_map = rvs_icd9,
    phic_acr_procedure = acr_rvs,
    grouper_v5_i10 = tdrg_icd10,
    acc_pdx = acc_pdx,
    icd_phl_icd10 = phl_icd10,
    neoplasms_dt_actual = neoplasms_dt_actual,
    grouper_v5_i10vx = i10vx,
    acc_icd = acc_icd,
    hci_temp_hci = hci
  )
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
    result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]
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
    zero_mask <- result[, pat_age >= 0 & pat_age < 1]
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
      result[!is.na(pat_age) & is.na(pat_bdate) & !is.na(date_adm), pat_bdate := as.Date(date_adm) - round(pat_age * 365.25)]
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
      zero_mask <- result[, pat_age >= 0 & pat_age < 1]
      result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, function(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
      result[!zero_mask, pat_bwt := NA_real_]
      cat("\rWriting final\n")
      flush.console()
      saveRDS(result, here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
    }
  }
  if (!to_thai_all_years && any(duplicated(result$id_series))) {
    duplicate_ids <- result$id_series[duplicated(result$id_series)]
    duplicate_rows <- result[id_series %in% duplicate_ids, ]
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)
    stop("The 'id_series' column contains duplicates. Execution stopped.")
  }
  print(nrow(result))
  if (!to_thai_all_years && any(duplicated(result$caseid))) {
    duplicate_ids <- result$caseid[duplicated(result$caseid)]
    duplicate_rows <- result[caseid %in% duplicate_ids, ]
    cat("Rows with duplicate 'caseid':\n")
    print(duplicate_rows)
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
bw_data <- data.frame(bw_dist = bw_dist)
ggplot(bw_data, aes(x = bw_dist)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(
    title = "Generated Live Filipino Infant Birthweights (Bin Width 0.5 kg)",
    x = "Newborn Birthweight (in kg)",
    y = "Number of Cases"
  ) +
  theme_minimal()
if (to_python && !to_generate_subset && to_generate_feather) {
  cat("\rReading final\n")
  flush.console()
  result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
}
if (to_python && to_generate_feather) {
  message("Renaming columns")
  result[, patage := as.numeric(pat_age)]
  result[, patsex := as.character(pat_sex)]
  result[, birthweight := as.numeric(pat_bwt)]
  result[, discharge := as.integer(clin_discharge)]
  result[, dob := as.Date(pat_bdate)]
  result[, ageday := as.integer(pat_ageday)]
  result[, pdx := clin_pdx]
  split_codes_from_list <- function(dt, column, prefix, max_cols) {
    split_list <- dt[[column]] # Extract the list column
    split_cols <- parallel::mclapply(
      1:max_cols,
      function(i) sapply(split_list, function(x) if (length(x) >= i) x[[i]] else NA_character_),
      mc.cores = nthreads # Automatically use all available cores
    )
    split_dt <- as.data.table(split_cols)
    setnames(split_dt, paste0(prefix, 1:max_cols))
    return(split_dt)
  }
  message("Splitting clin_sdx")
  sdx_columns <- split_codes_from_list(result, "clin_sdx", "sdx", 12)
  message("Splitting clin_proc")
  proc_columns <- split_codes_from_list(result, "clin_proc", "proc", 20)
  message("cbind results")
  result <- cbind(result, sdx_columns, proc_columns)
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
  message("Writing to csv")
  fwrite(for_fwrite, here(checkpoint_7_path, paste0(checkpoint_7b_prefix, suffix, ".csv")))
  message("Creating summary table")
  summary_table <- for_fwrite[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = names(for_fwrite)]
  summary_table <- transpose(summary_table)
  setnames(summary_table, "Non-Null Count")
  summary_table[, Column := names(for_fwrite)]
  setcolorder(summary_table, c("Column", "Non-Null Count"))
  message("Printing summary table")
  print(summary_table)
  saveRDS(for_fwrite, here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
}
if (to_python) {
  if (!to_generate_py_fwrite && to_generate_feather) for_fwrite <- readRDS(here(checkpoint_7_path, paste0("for_fwrite_", year_to_load, suffix, ".rds")))
  if (to_generate_feather) write_feather(as.data.frame(for_fwrite), here(checkpoint_7_path, paste0("python_input_", year_to_load, suffix, ".feather")))
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
  setnames(output_dt,
    old = c("drg", "pdc", "pccl", "error_code", "warning_code"),
    new = c("py_drg", "py_pdc", "py_pccl", "py_err", "py_warn"), skip_absent = TRUE
  )
  required_columns <- c("id_series", "py_drg", "py_pdc", "py_pccl", "py_err", "py_warn")
  output_dt <- output_dt[, ..required_columns]
  output_dt[, py_drg := as.character(py_drg)]
  output_dt[, py_pdc := as.character(py_pdc)]
  output_dt[, py_pccl := as.numeric(py_pccl)]
  array_columns <- c("py_err", "py_warn")
  process_error_warning_column <- function(col) {
    lapply(col, function(x) {
      x <- unlist(x)
      x <- as.character(x)
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }
      x <- x[!is.na(x)]
      x <- x[!(x %in% c("None", "NA", "NaN", ""))]
      if (length(x) == 0) {
        return(character(0))
      }
      split_x <- unlist(strsplit(x, ",\\s*"))
      split_x <- split_x[!(split_x %in% c("", "NaN", "NA", "None")) & !is.na(split_x)]
      if (length(split_x) == 0) {
        return(character(0))
      } else {
        return(split_x)
      }
    })
  }
  output_dt[, (array_columns) := mclapply(.SD, process_error_warning_column, mc.cores = nthreads), .SDcols = array_columns]
  output_dt[, py_drg := ifelse(is.na(py_drg), "", py_drg)]
  output_dt[, py_pdc := ifelse(is.na(py_pdc), "", py_pdc)]
  print(head(output_dt, 100))
}
if (to_python) {
  if (to_debug) fwrite(output_dt, "test3.csv")
}
if (to_python && any(duplicated(output_dt$id_series))) {
  stop("The 'id_series' column contains duplicates. Execution stopped.")
}
if (to_python && to_py_bq) {
  bq_table <- if (nrow(output_dt) == nrow(result)) {
    paste0("python_", year_to_load)
  } else {
    paste0("temp_python_", year_to_load)
  }
  tryCatch(
    {
      bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
      message("Table dropped successfully.\n")
    },
    error = function(e) {
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.\n")
      } else {
        stop(e)
      }
    }
  )
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
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        stop(e)
      }
    }
  )
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
      result[, caseid := as.character(seq_len(nrow(result)))]
      result_mapping <- result[, .(id_series, caseid)]
      cat("\rExporting for grouper\n")
      flush.console()
      chunk_size <- 5000000
      num_chunks <- ceiling(nrow(result) / chunk_size)
      for (i in seq_len(num_chunks)) {
        output_file <- here(
          checkpoint_4_path,
          paste0(
            checkpoint_4_prefix, year_to_load, suffix,
            "part_", i, "_of_", num_chunks, ".txt"
          )
        )
        start_row <- (i - 1) * chunk_size + 1
        end_row <- min(i * chunk_size, nrow(result))
        chunk <- result[start_row:end_row, ]
        export_for_grouper(chunk, output_file, i)
        message("Saved part ", i, " of ", num_chunks, " to ", output_file)
        message("Uploading part ", i, " of ", num_chunks, " to GCS")
        gcs_upload(
          file = output_file,
          bucket = gcs_bucket,
          name = paste0(gcs_pre_fpath, "/", basename(output_file)),
          predefinedAcl = "bucketLevel"
        )
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
        result[, caseid := as.character(seq_len(nrow(result)))]
        result_mapping <- result[, .(id_series, caseid)]
        cat("\rExporting for grouper\n")
        flush.console()
        chunk_size <- 5000000
        num_chunks <- ceiling(nrow(result) / chunk_size)
        for (i in seq_len(num_chunks)) {
          output_file <- here(
            checkpoint_4_path,
            paste0(
              checkpoint_4_prefix, year_to_load, suffix,
              "part_", i, "_of_", num_chunks, ".txt"
            )
          )
          start_row <- (i - 1) * chunk_size + 1
          end_row <- min(i * chunk_size, nrow(result))
          chunk <- result[start_row:end_row, ]
          export_for_grouper(chunk, output_file, i)
          message("Saved part ", i, " of ", num_chunks, " to ", output_file)
          message("Uploading part ", i, " of ", num_chunks, " to GCS")
          gcs_upload(
            file = output_file,
            bucket = gcs_bucket,
            name = paste0(gcs_pre_fpath, "/", basename(output_file)),
            predefinedAcl = "bucketLevel"
          )
          rm(chunk)
          gc()
        }
      }
    } else {
      message("Skipping thai txt generation")
    }
  }
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
    cat("\rReading final\n")
    flush.console()
    result <- readRDS(here(checkpoint_2_path, paste0(checkpoint_2_prefix, year_to_load, suffix, "final_subset_with_bdate_with_time", ".rds")))
    result[, caseid := as.character(seq_len(nrow(result)))]
    result_mapping <- result[, .(id_series, caseid)]
    chunk_size <- 5000000
    num_chunks <- ceiling(nrow(result) / chunk_size)
    thai_result <- list()
    for (i in seq_len(num_chunks)) {
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
      message("Downloading part ", i, " of ", num_chunks, " from GCS")
      gcs_get_object(
        object_name = remote_file,
        bucket = gcs_bucket,
        saveToDisk = local_file,
        overwrite = TRUE
      )
      part_data <- fread(local_file, colClasses = "character")
      thai_result[[i]] <- part_data
      rm(part_data)
      gc()
    }
    thai_result <- rbindlist(thai_result, use.names = FALSE, fill = FALSE)
    cat(paste("nrow thai_result:", nrow(thai_result), "\n"))
    cat(paste("nrow result_mapping:", nrow(result_mapping), "\n"))
    cat(paste("nrow result:", nrow(result), "\n"))
    message("All parts downloaded and combined successfully.\n")
    if (to_debug) print(head(thai_result))
    if (any(duplicated(thai_result$caseid))) {
      duplicate_ids <- thai_result$caseid[duplicated(thai_result$caseid)]
      duplicate_rows <- thai_result[caseid %in% duplicate_ids, ]
      cat("Rows with duplicate 'id_series':\n")
      print(duplicate_rows)
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
  }
}
if (to_thai && !to_thai_all_years) {
  print(nrow(thai_result))
  print(nrow(thai_result[thai_err == "6"]))
}
if (to_python && to_thai && !to_thai_all_years) {
  if (exists("output_dt")) {
    before_merge <- data.table::copy(output_dt)
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
  print(result_after_thai[grepl("e", id_series)])
}
if (to_thai && !to_thai_all_years) {
}
if (to_thai && !to_thai_all_years) {
  if (any(duplicated(result_after_thai$id_series))) {
    duplicate_ids <- result_after_thai$id_series[duplicated(result_after_thai$id_series)]
    duplicate_rows <- result_after_thai[id_series %in% duplicate_ids, ]
    cat("Rows with duplicate 'id_series':\n")
    print(duplicate_rows)
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
  prefix <- if (!to_sample) "thai_" else "temp_thai_"
  bq_table <- paste0(prefix, year_to_load)
  if (!to_spc) {
    tryCatch(
      {
        bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table))
        message("Table dropped successfully.\n")
      },
      error = function(e) {
        if (grepl("Not found", e, ignore.case = TRUE)) {
          message("Table does not exist, nothing to drop.\n")
        } else {
          stop(e)
        }
      }
    )
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
        if (grepl("already exists", e, ignore.case = TRUE)) {
          message("Table already exists. Skipping creation and upload.")
        } else {
          stop(e)
        }
      }
    )
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
```

## Python Notebook

### 02b-drg-grouping-py-v2.ipynb
```python
from rpy2.robjects import r, globalenv
from rpy2.robjects.packages import importr
import os
r_source = r['source']
r_source("~/drg-pipeline/data-cleaning/00a-parameters.r")
thread_offset = r['thread_offset'][0]
sample_size_divisor = int(r['sample_size_divisor'][0])
to_sample = bool(r['to_sample'][0])
to_write = bool(r['to_write'][0])
to_flush = bool(r['to_flush'][0])
to_parallel = bool(r['to_parallel'][0])
to_debug = bool(r['to_debug'][0])
verbose_output = bool(r['verbose_output'][0])
to_generate_subset = bool(r['to_generate_subset'][0])
to_py_prompt = bool(r['to_py_prompt'][0])
to_python = bool(r['to_python'][0])
to_generate_py_fwrite = bool(r['to_generate_py_fwrite'][0])
to_generate_feather = bool(r['to_generate_feather'][0])
to_py_bq = bool(r['to_py_bq'][0])
to_thai_prompt = bool(r['to_thai_prompt'][0])
to_thai = bool(r['to_thai'][0])
to_thai_bq = bool(r['to_thai_bq'][0])
to_generate_thai_txt = bool(r['to_generate_thai_txt'][0])
to_thai_all_years = bool(r['to_thai_all_years'][0])
to_spc = bool(r['to_spc'][0])
print("Parallelization:", to_parallel)
year_file_path = os.path.expanduser("~/drg-pipeline/data-cleaning/cache/year_to_load.txt")
with open(year_file_path, "r") as file:
    year_to_load = file.read().strip()  # .strip() removes any surrounding whitespace or newlines
if to_sample:
    suffix = f"_sampled_{sample_size_divisor}_"
else:
    suffix = "_full_"
print(suffix)
print(year_to_load)
import pandas as pd
import os
import numpy as np
from grouper import seeker
from multiprocessing import Pool, cpu_count
import traceback
import swifter
import traceback
import sys
import io
import pyarrow
import gc
feather_file_path = f"~/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_input_{year_to_load}{suffix}.feather"
feather_file_path = os.path.expanduser(feather_file_path)
pandas_df = pd.read_feather(feather_file_path)
print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])
print("Converting data types")
pandas_df['patage'] = pd.to_numeric(pandas_df['patage'], errors='coerce')
pandas_df['ageday'] = pd.to_numeric(pandas_df['ageday'], errors='coerce')
pandas_df['birthweight'] = pd.to_numeric(pandas_df['birthweight'], errors='coerce')
pandas_df['discharge'] = pandas_df['discharge'].astype('Int64')
string_columns = ['id_series', 'patsex', 'pdx', 'sdx1', 'sdx2', 'sdx3', 'sdx4', 'sdx5', 'sdx6', 'sdx7', 'sdx8', 'sdx9', 'sdx10', 'sdx11', 'sdx12',
                  'proc1', 'proc2', 'proc3', 'proc4', 'proc5', 'proc6', 'proc7', 'proc8', 'proc9', 'proc10', 'proc11', 'proc12',
                  'proc13', 'proc14', 'proc15', 'proc16', 'proc17', 'proc18', 'proc19', 'proc20', 'date_adm', 'date_dis', 'dob']
print("Replacing with None")
pandas_df.replace([pd.NA, np.nan, '<NA>', 'None', 'NA', -2147483648], None, inplace=True)
print("Converting to string")
pandas_df[string_columns] = pandas_df[string_columns].astype('string')
print("Replacing -2147483648 with None")
pandas_df.replace(-2147483648, None, inplace=True)
print("Generating info()")
pandas_df.info()
print(pandas_df)
print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])
print(pandas_df)
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_final_input_{year_to_load}{suffix}.feather"
pandas_df.to_feather(file_path)
print("Initializing Libraries")
libs = seeker.Libraries()
def process_patient(row, libs):
    try:
        patient = seeker.Patient(row.to_dict(), libs)
        result = {
            'id_series': row['id_series'],  # Ensure `id_series` is carried forward
            'pdc': patient.pdc,
            'pccl': patient.pccl,
            'drg': patient.drg,
            'error_code': patient.error_code,
            'warning_code': patient.warning_code
        }
        return result
    except Exception as e:
        print(f'''Error processing patient with id_series {row['id_series']}: {e}''')
        traceback.print_exc()  # Print the full stack trace for more details
        return {
            'id_series': row['id_series'],  # Ensure `id_series` is carried forward
            'pdc': None,
            'pccl': None,
            'drg': None,
            'error_code': None,
            'warning_code': None
        }
def process_chunk(chunk):
    return [process_patient(row, libs) for _, row in chunk.iterrows()]
if __name__ == "__main__":
    print("Splitting DataFrame into chunks")
    num_cores = cpu_count()  # Use the number of available CPU cores
    chunk_size = len(pandas_df) // num_cores
    chunks = [pandas_df[i:i + chunk_size] for i in range(0, len(pandas_df), chunk_size)]
    print("Processing chunks with multiprocessing")
    with Pool(num_cores) as pool:
        results = pool.map(process_chunk, chunks)
    print("Combining results")
    processed_data = pd.DataFrame([row for chunk in results for row in chunk])
    print("Renaming columns")
    processed_data.rename(columns={'drg': 'py_drg'}, inplace=True)
    print("Reordering columns")
    desired_columns = [
        'id_series', 'pdc', 
        'pccl', 'py_drg', 'error_code', 
        'warning_code'
    ]
    print("Subsetting columns")
    pandas_df = processed_data[desired_columns]
    print(pandas_df)
print(pandas_df)
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_8_py_output/python_output_{year_to_load}{suffix}.feather"
pandas_df.to_feather(file_path)
```
