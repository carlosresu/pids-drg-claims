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
set.seed(global_seed)
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
all_parts_summaries <- master_dt_list <- combined_chunk_summary <- pdx_success_list <- replacement_summary_list <- icd_mapping_list <- list() # initialize lists
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table::data.table() # initialize data.tables
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
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
flatten_then_check_null_na <- function(input) {
  input <- unlist(input, recursive = TRUE)
  if (length(input) == 0 || all(is.null(input)) || all(is.na(input))) {
    return(character(0)) # Return empty character vector if all NULL/NA
  } else {
    return(input) # Already a flat character vector
  }
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
prep_icd_for_mapping <- function(text) {
  text %>%
    manual_replacement() %>%
    collapse_to_string() %>%
    split_to_vector() %>%
    remove_lumped_icd_codes() %>%
    flatten_then_check_null_na()
}
filter_icds <- function(codes, neoplasm_codes, covidrvs, acc_pdx_set) {
  codes <- codes[!is.null(codes) & !is.na(codes) & !grepl("^[0-9]", codes) &
    !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
    !(codes %chin% neoplasm_codes) & !(codes %chin% rvs_codes) &
    !(codes %chin% covidrvs)]
  codes[codes %chin% acc_pdx_set]
}
safe_split <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(NA_character_) # Return an empty character vector for consistency
  }
  unlist(strsplit(x, "\\|")) # Split valid strings by '|'
}
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
process_chunk <- function(chunk,
                          yr_to_load = year_to_load,
                          col_maps = column_mappings,
                          known_vals = known_values,
                          remap_master = col_remap_master,
                          avail_cols = available_columns) {
  setnames(chunk,
    old = avail_cols[avail_cols %in% names(col_maps)],
    new = sapply(
      avail_cols[avail_cols %in% names(col_maps)],
      function(col) col_maps[[col]]
    )
  )
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
  c2_result <- clean_column(chunk$c2)
  chunk[, `:=`(c2_orig = c2, c2 = c2_result$cleaned_col)]
  is_covid_c1 <- c1_result$is_covid
  is_covid_c2 <- c2_result$is_covid
  chunk[, is_covid := (is_covid_c1 | is_covid_c2)]
  chunk[, `:=`(
    c1 = lapply(c1, prep_icd_for_mapping),
    c2 = lapply(c2, prep_icd_for_mapping)
  )]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    clin_icd_list <- c(manual_replacement(clin_icd[[i]]), c1[[i]], c2[[i]])
    return(flatten_then_check_null_na(clin_icd_list))
  })]
  replace_result <- replace_na_or_empty(
    dt = chunk, replace_with = "NA_character_"
  )
  chunk <- replace_result$return_data
  replace_empty_result_1 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_1$return_data
  c1_results <- append_copy_and_remove_icd_rvs(
    chunk$c1, chunk$clin_rvs, chunk$clin_icd
  )
  chunk[, `:=`(
    clin_rvs = c1_results$clin_rvs,
    c1 = c1_results$col,
    clin_icd = c1_results$clin_icd
  )]
  c2_results <- append_copy_and_remove_icd_rvs(
    chunk$c2, chunk$clin_rvs, chunk$clin_icd
  )
  chunk[, `:=`(
    clin_rvs = c2_results$clin_rvs,
    c2 = c2_results$col,
    clin_icd = c2_results$clin_icd
  )]
  replace_result <- replace_na_or_empty(
    dt = chunk, replace_with = "NA_character_"
  )
  chunk <- replace_result$return_data
  replace_empty_result_1 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_1$return_data
  cols_to_remap <- c(
    "pat_type", "pat_memcat_parent",
    "pat_memcat_child", "clin_discharge", "claim_status"
  )
  chunk[, (cols_to_remap) := lapply(.SD, remap_patient_data, remap_master), .SDcols = cols_to_remap]
  chunk[, icd9_list := map_rvs_icd9(clin_rvs)$icd9_list]
  chunk[, `:=`(
    c1 = lapply(
      c1, function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    ),
    c2 = lapply(
      c2, function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    )
  )]
  chunk[, `:=`(
    c1 = map_icd10(c1),
    c2 = map_icd10(c2),
    clin_icd = map_icd10(clin_icd)
  )]
  replace_empty_result_2 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_2$return_data
  pdx_inputs <- prep_pdx_inputs(
    chunk$c1, chunk$c2, chunk$clin_icd,
    acc_pdx, neoplasms_dt_actual, acr_rvs, covid_rvs
  )
  pdx_result <- find_pdx(
    pdx_inputs$c1, pdx_inputs$c2, pdx_inputs$clin_icd,
    global_seed
  )
  chunk[, c("pdx", "pdx_code") := .(pdx_result$pdx, pdx_result$pdx_code)]
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
  chunk[, clin_sdx := lapply(
    clin_sdx, function(x) if (is.null(x)) character(0) else unlist(x)
  )]
  replace_empty_result_3 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)",
    additional_columns = c("c1", "c2", "pdx")
  )
  chunk <- replace_empty_result_3$return_data
  chunk[, (char_cols) := lapply(.SD, function(col) {
    iconv(col, from = "", to = "UTF-8")
  }), .SDcols = char_cols]
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
    "claim_charge", "is_covid", "clin_discharge", "clin_outpatient",
    "clin_emergency", "clin_acc", "clin_c1", "c1", "clin_c2", "c2",
    "clin_sdx", "clin_proc", "clin_rvs", "clin_pdx", "clin_pdx_source"
  ))
  chunk[, clin_discharge := as.integer(clin_discharge)]
  chunk[, clin_sdx := lapply(clin_sdx, function(x) head(x, 12))]
  chunk[, clin_proc := lapply(clin_proc, function(x) head(x, 20))]
  chunk[
    !is.na(pat_bdate) & !is.na(date_adm),
    pat_age := floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
  ]
  chunk[
    !is.na(pat_bdate) & !is.na(date_adm) & !is.na(pat_age) & pat_bdate > as.Date(date_adm),
    pat_bdate := NA_Date_
  ]
  chunk[
    grepl("99432", c1) & !is.na(pat_age) & pat_age < 0 & pat_age >= -1,
    pat_age := 0
  ]
  chunk[
    !is.na(pat_age) & pat_age > 0 & pat_age <= 124,
    pat_age := floor(pat_age)
  ]
  chunk[
    !is.na(pat_age) & (pat_age < 0 | pat_age > 124),
    pat_age := NA_integer_
  ]
  invisible(gc())
  return(chunk)
}
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
remap_patient_data <- function(col, remapping) {
  eval(remapping,
  list(dt = data.table(data = col),
  column_name = "data"))
}
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
map_icd10 <- function(col) {
  icds <- unique(unlist(col))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !vapply(icds, function(code) exists(x = code, envir = covid_rvs_neoplasm_env, inherits = FALSE), logical(1))]
  icd_mapping <- list() # Mapping to store results
  for (code in filtered_icds) {
    code <- trimws(code)
    if (exists(x = code, envir = icd_codes_env, inherits = FALSE)) {
      icd_mapping[[code]] <- code
      next
    }
    if (nchar(code) == 3) {
      modified_code <- paste0(code, "9")
      if (exists(x = modified_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- modified_code
        next
      }
    }
    trimmed_code <-
      if (nchar(code) > 4) {
        sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
      } else if (nchar(code) == 4) {
        code
      } else {
        NULL
      }
    if (!is.null(trimmed_code)) {
      if (exists(x = trimmed_code, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_code
        next
      }
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (exists(x = trimmed_to_3, envir = icd_codes_env, inherits = FALSE)) {
        icd_mapping[[code]] <- trimmed_to_3
        next
      }
    }
    icd_mapping[[code]] <- NA_character_
  }
  col_mapped <- lapply(col, function(codes) {
    unname(sapply(codes, function(code) {
      if (!is.null(icd_mapping[[code]]) && !is.na(icd_mapping[[code]])) {
        icd_mapping[[code]]
      } else {
        code
      }
    }))
  })
  return(col_mapped)
}
prep_pdx_inputs <- function(
    c1_orig, c2_orig, clin_icd_orig,
    accpdx = acc_pdx, neoplasmsdtactual = neoplasms_dt_actual,
    acrrvs = acr_rvs, covidrvs = covid_rvs) {
  acc_pdx_set_final <- unique(accpdx)
  neoplasm_codes_final <- unique(neoplasmsdtactual$icd10)
  rvs_codes_final <- unique(acrrvs$rvs)
  covidrvsfinal <- unique(covidrvs)
  c1_temp <- lapply(c1_orig, function(x) safe_split(remove_whitespace(x)))
  c2_temp <- lapply(c2_orig, function(x) safe_split(remove_whitespace(x)))
  clin_icd_temp <- lapply(clin_icd_orig, function(x) safe_split(remove_whitespace(x)))
  c1_final <- lapply(c1_temp, filter_icds, neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final)
  c2_final <- lapply(c2_temp, filter_icds, neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final)
  clin_icd_final <- lapply(clin_icd_temp, filter_icds, neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final)
  pdx_inputs <- list(
    c1 = c1_final, c2 = c2_final, clin_icd = clin_icd_final
  )
  return(pdx_inputs)
}
find_pdx <- function(
    c1_split, c2_split, clin_icd_split,
    seed) {
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }
  algo_result <- mapply(function(c1_split, c2_split, clin_icd_split) {
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        return(list(
          pdx = cr_list[1],
          pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)
        ))
      }
    }
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      return(list(
        pdx = NA_character_,
        pdx_code = 99
      ))
    }
    if (length(pdxs) == 1) {
      return(list(
        pdx = pdxs[1],
        pdx_code = 3
      ))
    }
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(
            pdx = starting_codes[1],
            pdx_code = 4
          ))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          return(list(
            pdx = best_match,
            pdx_code = 5
          ))
        }
      }
    }
    set.seed(seed)
    return(list(
      pdx = sample(pdxs, 1),
      pdx_code = 6
    ))
  }, c1_split, c2_split, clin_icd_split, SIMPLIFY = FALSE)
  return(list(
    pdx = sapply(algo_result, `[[`, "pdx"),
    pdx_code = sapply(algo_result, `[[`, "pdx_code")
  ))
}
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
      rvss = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "rvss")))),
      mappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "mappable_rvs")))),
      unmappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmappable_rvs")))),
      multi_mapped_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "multi_mapped_rvs")))),
      without_drg = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "without_drg")))),
      pdx_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "pdx_success")))),
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
  if (summary$pdx_success) cat("\nAll PDx's are in the list of acceptable PDx's:\n", summary$pdx_success, "\n") else stop(paste0("Not all pdx are in acceptable pdxs for ", year_to_load, "."))
}
