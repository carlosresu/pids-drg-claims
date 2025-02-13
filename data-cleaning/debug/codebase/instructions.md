# Codebase Context

## Helper Scripts

### 0.1.0.params_fpaths.R
```r
tictoc::tic("Time spent (total)               ")
nthreads <- parallelly::availableCores()
nthreads <- if (nthreads >= 16) nthreads - thread_offset else nthreads
dir.create(dirname(here::here("data-cleaning/debug/cache/year.txt")),
  recursive = TRUE, showWarnings = FALSE
)
if (!file.exists(here::here("data-cleaning/debug/cache/year.txt"))) {
  writeLines("2018", here::here("data-cleaning/debug/cache/year.txt"))
}
if (!exists("year")) {
  year <- data.table::fread(here::here("data-cleaning", "debug", "cache", "year.txt"),
    header = FALSE, colClasses = "character"
  )[[1]]
}
file_type <- if (year %in% c(2022:2023)) ".tsv" else ".csv"
separator <- if (file_type == ".tsv") "\t" else ","
thai_prompt <- TRUE
to_prompt <- FALSE
split_parts <- 15
if (exists("sample_size_divisor")) {
  sample_size_divisor <- sample_size_divisor
} else {
  sample_size_divisor <- 5
}
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 21:170) # Continuation
)
drop_cols_manual <- c(
  "MEMCAT_SUBCHILD_DESC" # Drop as per Cel's suggestion
)
manual_patterns_to_replace <- c("\\b0800\\b", "\\b080\\b", "\\b0809\\b")
manual_code_replacements <- c("O800", "O80", "O809")
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
gcs_bucket <- "phic-claims-chkpts"
gcs_pre_fpath <- "pre-tdrg"
gcs_post_fpath <- "post-tdrg"
gcs_spc_fpath <- "spc"
bq_dataset <- "phic_claims"
bq_table <- paste0("temp_claims_", year)
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
chkpt_5_prefix <- toupper(paste0(gcs_pre_fpath, "_", chkpt_4_prefix))
chkpt_6_prefix <- "chkpt_6_grouped_claims"
chkpt_7a_prefix <- "python_input_1"
chkpt_7b_prefix <- "python_input_2"
chkpt_10_prefix <- "stata"
chkpt_path <- file.path(data_prefix, "chkpts")
chkpt_1_path <- file.path(chkpt_path, "chkpt_1_partial_clean_claims")
chkpt_2_path <- file.path(chkpt_path, "chkpt_2_master_clean_claims")
chkpt_3_path <- file.path(chkpt_path, "chkpt_3_thai_partial_input")
chkpt_4_path <- file.path(chkpt_path, "chkpt_4_thai_master_input")
chkpt_5_path <- file.path(chkpt_path, "chkpt_5_thai_output")
chkpt_6_path <- file.path(chkpt_path, "chkpt_6_thai_merged")
chkpt_7_path <- file.path(chkpt_path, "chkpt_7_py_input")
chkpt_8_path <- file.path(chkpt_path, "chkpt_8_py_output")
chkpt_9_path <- file.path(chkpt_path, "chkpt_9_grouper_differences")
chkpt_10_path <- file.path(chkpt_path, "chkpt_10_stata")
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
  paste0(full_claims_prefix, year, file_type)
)
ram_limit <- (1 - 0.10) * 64 * (1024^3)
options(future.globals.maxSize = ram_limit)
total_rows_file <- here::here(
  cache_path, "total_rows",
  paste0("total_rows_", year, ".rds")
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
suffix <- paste0(ifelse(to_sample, paste0(
  "_sampled_",
  sample_size_divisor, "_"
), "_full_"))
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
  "ICDCODES_ITEM7" = "clin_icd1",
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
    "id_series", "id_pin", "id_hci", "id_hcp",
    "date_adm", "time_adm", "date_dis", "time_dis",
    "date_rec", "date_ref", "date_check", "date_ext",
    "pat_type", "pat_rel", "pat_sex", "pat_memcat_parent",
    "pat_memcat_child", "claim_status", "clin_pdx"
  ),
  "integer" = c("id_year", "clin_pdx_source"),
  "factor" = c(
    "pat_type", "pat_memcat_parent", "pat_memcat_child",
    "clin_discharge", "claim_status"
  ),
  "numeric" = c("pat_age", "pat_bwt", "claim_payout", "claim_charge")
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
all_parts_summaries <- master_dt_list <- icd_mapping_list <- list()
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- split_processing_times <-
  nrow_start <- nrow_end <- numeric(split_parts)
master_dt <- data.table::data.table() # initialize data.tables
message(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))
```

### 0.2.0.process_helper_functions.R
```r
manual_replacement <- function(text) {
  replaced <- stri_replace_all_regex(
    text,
    manual_patterns_to_replace,
    manual_code_replacements,
    vectorize_all = FALSE
  )
  return(replaced)
}
remove_periods_and_whitespaces <- function(x) {
  x <- sapply(x, function(elem) {
    element <- iconv(elem, from = "latin1", to = "UTF-8")
    return(element)
  }, USE.NAMES = FALSE)
  cleaned <- gsub("[.\\s]", "", x)
  return(cleaned)
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
    neoplasm_matches <- gregexpr(neoplasm_pattern,
      long_string,
      perl = TRUE
    )[[1]]
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
    clean_result <- final_result[final_result != ""]
    return(clean_result)
  })
}
remove_lumped_icd_codes <- function(column) {
  unlumped <- lapply(as.list(column), function(vec) {
    processed <- unlist(lapply(vec, function(element) {
      if ((is.na(element) || element == "")
      ) {
        return(character(0))
      } else {
        return(unlist(strsplit(element, "(?<=\\d)(?=[A-Z][0-9]{2,})",
          perl = TRUE
        )))
      }
    }))
    processed <- processed[processed != ""]
    return(processed)
  })
  return(unlumped)
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
  modified_column <- sapply(
    as.character(column),
    function(code) {
      if (is.na(code) || code == "" || is.null(code)) {
        return(NA_character_)
      }
      code_clean <- gsub("\\|", "", code)
      code_clean <- gsub("[^A-Z0-9]", "", code_clean)
      if (nchar(code_clean) == 0) {
        return(NA_character_)
      }
      if (nchar(code_clean) %% 5 != 0) {
        return(NA_character_)
      }
      modified_code <- gsub("(.{5})", "\\1||", code_clean)
      modified_code <- gsub("\\|\\|$", "", modified_code)
      return(modified_code)
    },
    USE.NAMES = FALSE
  )
  return(modified_column) # Return the modified column with split RVS codes
}
collapse_to_string <- function(vec) {
  vec <- vec[vec != "" & !is.na(vec)]
  if (length(vec) > 0) {
    vector <- paste(vec, collapse = "||")
    return(vector)
  } else {
    empty_vec <- NA_character_
    return(empty_vec)
  }
}
replace_na_or_empty_col <- function(col, replace_with) {
  if (replace_with == "NA_character_") {
    if (is.list(col)) {
      return(lapply(col, function(x) {
        if (all(is.na(x)) || identical(x, "") || identical(x, "NA")) character(0) else x
      }))
    } else {
      col[is.na(col) | col %chin% c("", "NA", "character(0)")] <- NA_character_
      if (is.factor(col)) {
        col <- factor(col, levels = c(levels(col), NA))
      }
    }
  } else { # If replace_with == "character(0)"
    if (is.list(col)) {
      col <- lapply(col, function(x) {
        if (all(is.na(x)) || identical(x, "") || identical(x, "NA")) character(0) else x
      })
    }
  }
  return(col)
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
  neopl <- setNames(
    neoplasms_dt_actual$icd10,
    gsub("/", "", neoplasms_dt_actual$icd10)
  )
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[
    matched_indices[!is.na(matched_indices)]
  ]
  return(cleaned_col)
}
remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  } else {
    return(gsub("\\s+", "", x))
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
  filtered <- codes[codes %chin% acc_pdx_set]
  return(filtered)
}
safe_split <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(NA_character_)
  }
  unlisted_and_split <- unlist(strsplit(x, "\\|"))
  return(unlisted_and_split)
}
print_status_update <- function(
    status_part, split_parts,
    processing_times, phase) {
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
    trimmed_time <- trimws(paste(time_components, collapse = " "))
    return(trimmed_time)
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
```

### 0.3.0.grouping_functions.R
```r
export_for_grouper <- function(dt, output_txt_file, chunk_number) {
  output_dt_thai <- data.table()
  output_dt_thai[, CASEID := dt$caseid]
  output_dt_thai[, DOB := as.character(
    format(as.Date(dt$pat_bdate), "%d/%m/%Y")
  )]
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt_thai[, DateAdm := format(
    as.Date(dt$date_adm), "%d/%m/%Y"
  )]
  output_dt_thai[, TimeAdm := format(
    as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M"
  )]
  output_dt_thai[, DateDsc := format(
    as.Date(dt$date_dis), "%d/%m/%Y"
  )]
  output_dt_thai[, TimeDsc := format(
    as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M"
  )]
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
      bq_table_download(bq_project_query(gcp_proj, query),
        n_max = max_bq_rows
      )
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
    full_claims_prefix, year,
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
create_sample_files <- function(
    sample_part,
    sampled_claims_file, seed = global_seed) {
  partial_file_for_sampling <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year,
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
      "sampled_claims_", year, "_", sample_size_divisor,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".rds"
    ))
  }
  dt <- readRDS(chunk_file)
  available_columns <<- colnames(dt)
  cols_to_drop <- intersect(available_columns, c(drop_cols, drop_cols_manual))
  if (length(cols_to_drop) > 0) {
    dt <- dt[, (cols_to_drop) := NULL]
  }
  nrow_start[[read_part]] <<- nrow(dt)
  return(dt)
}
```

### 5.1.0.collapse_clean_icd_rvs_cols.R
```r
collapse_clean_icd_rvs_cols <- function(cols, is_icd) {
  cleaned_columns <- lapply(cols, clean_column)
  collapsed <- lapply(seq_along(cleaned_columns[[1]]), function(i) {
    combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
    combined <- combined[!combined %chin% na_like_strings & combined != ""]
    if (length(combined) > 0) {
      combined
    } else {
      character(0) # Return an empty vector instead of NA
    }
  })
  split <- split_to_vector(collapsed)
  unlumped <- if (is_icd) remove_lumped_icd_codes(split) else split
  return(unlumped) # Always return a list of vectors
}
```

### 5.2.0.append_copy_remove_icd_rvs.R
```r
append_copy_remove_icd_rvs <- function(col, clin_rvs, clin_icd) {
  datatable <- data.table(clin_rvs = clin_rvs, col = col, clin_icd = clin_icd)
  datatable[, matches := lapply(col, function(x) {
    valid_rvs_codes <- Filter(function(code) {
      flag <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(flag)
    }, x)
    return(valid_rvs_codes)
  })]
  datatable[, clin_rvs := mapply(function(rvs, matches) {
    unique_matches <- Filter(function(code) {
      valid_rvs <- !is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
      return(valid_rvs)
    }, matches)
    updated_rvs <- c(unique_matches[!unique_matches %in% rvs], rvs)
    deduplicated_rvs <- updated_rvs[!duplicated(updated_rvs)]
    return(deduplicated_rvs)
  }, clin_rvs, matches, SIMPLIFY = FALSE)]
  datatable[, icd_matches := mapply(function(rvs_vec, icd_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      flag <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code,
          envir = icd_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = phil_icds_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(flag)
    }, rvs_vec)
    updated_icd <- c(icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec], icd_vec)
    deduplicated_icd <- updated_icd[!duplicated(updated_icd)]
    return(deduplicated_icd)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]
  datatable[, clin_icd := icd_matches]
  datatable[, col := lapply(col, function(x) {
    filtered_col <- Filter(function(code) {
      invalid_rvs <- is.null(mget(code,
        envir = rvs_codes_env,
        ifnotfound = list(NULL)
      )[[1]])
      return(invalid_rvs)
    }, x)
    return(filtered_col)
  })]
  datatable[, clin_rvs := lapply(clin_rvs, function(rvs_vec) {
    filtered_rvs <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_rvs)
    }, rvs_vec)
    return(filtered_rvs)
  })]
  datatable[, clin_rvs := mapply(function(rvs_vec, icd_vec) {
    rvs_codes_in_icd <- Filter(function(code) {
      valid_rvs <- (nchar(code) == 5 && grepl("^[0-9]", code)) ||
        grepl("^[A-Z]{2}", code) ||
        !is.null(mget(code,
          envir = rvs_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = covid_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_rvs)
    }, icd_vec)
    updated_rvs <- c(rvs_vec, rvs_codes_in_icd[!rvs_codes_in_icd %in% rvs_vec])
    return(updated_rvs)
  }, clin_rvs, clin_icd, SIMPLIFY = FALSE)]
  datatable[, clin_icd := mapply(function(icd_vec, rvs_vec) {
    icd_codes_in_rvs <- Filter(function(code) {
      valid_icd <- (!grepl("^[0-9]{5}$", code) &&
        !grepl("^[A-Z]{2}", code) &&
        !grepl("/", code)) ||
        !is.null(mget(code,
          envir = icd_codes_env,
          ifnotfound = list(NULL)
        )[[1]]) ||
        !is.null(mget(code,
          envir = phil_icds_env,
          ifnotfound = list(NULL)
        )[[1]])
      return(valid_icd)
    }, rvs_vec)
    updated_icd <- c(icd_vec, icd_codes_in_rvs[!icd_codes_in_rvs %in% icd_vec])
    return(updated_icd)
  }, clin_icd, clin_rvs, SIMPLIFY = FALSE)]
  datatable[, col := lapply(col, function(x) {
    if (is.null(x) || all(is.na(x))) {
      processed_col <- NA_character_
    } else {
      processed_col <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    return(processed_col)
  })]
  ret_list <- list(
    clin_rvs = datatable$clin_rvs,
    clin_icd = datatable$clin_icd,
    col = datatable$col
  )
  return(ret_list)
}
```

### 5.3.0.remap_patient_data.R
```r
remap_patient_data <- function(col, remapping) {
  remapped <- eval(
    remapping,
    list(
      dt = data.table(data = col),
      column_name = "data"
    )
  )
  return(remapped)
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
  result <- lapply(clin_rvs, function(x) {
    mapped_icd9 <- unique(unlist(lapply(unlist(x), function(code) {
      rvs_map_solo[[code]] %||% rvs_map_list[[code]] %||% NULL
    })))
    if (length(mapped_icd9)) mapped_icd9 else NA_character_
  })
  return(result)
}
```

### 5.5.0.map_icd10.R
```r
map_icd10 <- function(col) {
  icds <- unique(unlist(col))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !vapply(icds, function(code) {
      exists(x = code, envir = covid_rvs_neoplasm_env, inherits = FALSE)
    }, logical(1))]
  icd_mapping <- list()
  for (code in filtered_icds) {
    code <- trimws(code) # Trim whitespace
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
    trimmed_code <- if (nchar(code) > 4) {
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
  ret <- lapply(col, function(codes) {
    unname(lapply(codes, function(code) {
      if (!is.null(icd_mapping[[code]]) && !is.na(icd_mapping[[code]])) {
        return(icd_mapping[[code]])
      } else {
        return(code) # Keep original code if no mapping found
      }
    }))
  })
  return(ret) # Always return a list of vectors
}
```

### 5.6.1.0.prep_pdx_inputs.R
```r
prep_pdx_inputs <- function(
    c1_orig, c2_orig, clin_icd_orig,
    accpdx = acc_pdx, neoplasmsdtactual = neoplasms_dt_actual,
    acrrvs = acr_rvs, covidrvs = covid_rvs) {
  acc_pdx_set_final <- unique(accpdx)
  neoplasm_codes_final <- unique(neoplasmsdtactual$icd10)
  rvs_codes_final <- unique(acrrvs$rvs)
  covidrvsfinal <- unique(covidrvs)
  c1_temp <- lapply(c1_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })
  c2_temp <- lapply(c2_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })
  clin_icd_temp <- lapply(clin_icd_orig, function(x) {
    split <- safe_split(remove_whitespace(x))
    return(split)
  })
  c1_final <- lapply(
    c1_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )
  c2_final <- lapply(
    c2_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )
  clin_icd_final <- lapply(
    clin_icd_temp, filter_icds,
    neoplasm_codes_final, covidrvsfinal, acc_pdx_set_final
  )
  ret_list <- list(c1 = c1_final, c2 = c2_final, clin_icd = clin_icd_final)
  return(ret_list)
}
```

### 5.6.2.0.find_pdx.R
```r
find_pdx <- function(
    c1_split, c2_split, clin_icd_split,
    seed) {
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    ret_flag <- sum(substr(x, 1, min_len) == substr(y, 1, min_len))
    return(ret_flag)
  }
  algo_result <- mapply(function(c1_split, c2_split, clin_icd_split) {
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        ret_list <- list(
          clin_pdx = cr_list[1],
          clin_pdx_source = ifelse(cr_list[1] %in% c1_split, 1, 2)
        )
        return(ret_list)
      }
    }
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      ret_list <- list(
        clin_pdx = NA_character_,
        clin_pdx_source = 99
      )
      return(ret_list)
    }
    if (length(pdxs) == 1) {
      ret_list <- list(
        clin_pdx = pdxs[1],
        clin_pdx_source = 3
      )
      return(ret_list)
    }
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          ret_list <- list(
            clin_pdx = starting_codes[1],
            clin_pdx_source = 4
          )
          return(ret_list)
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          ret_list <- list(
            clin_pdx = best_match,
            clin_pdx_source = 5
          )
          return(ret_list)
        }
      }
    }
    set.seed(seed)
    ret_list <- list(
      clin_pdx = sample(pdxs, 1),
      clin_pdx_source = 6
    )
    return(ret_list)
  }, c1_split, c2_split, clin_icd_split, SIMPLIFY = FALSE)
  ret_list <- list(
    clin_pdx = sapply(algo_result, `[[`, "clin_pdx"),
    clin_pdx_source = sapply(algo_result, `[[`, "clin_pdx_source")
  )
  return(ret_list)
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
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
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
process_file <- function(year) {
  file_type <- if (year %in% c(2022, 2023)) ".tsv" else ".csv"
  file_name <- paste0(full_claims_prefix, year, file_type)
  gcs_path <- paste0(gcs_base, file_name)
  file_path <- here::here(raw_claims_path, file_name)
  file_path_escaped <- escape_spaces(file_path)
  md5_rds_path <- here::here(raw_claims_md5_path, paste0(year, "_md5.rds"))
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
for (year in c(2018:2023)) {
  year <<- year
  invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
  full_header <- data.table::fread(
    file = full_claims_file,
    nrows = 1, colClasses = "character",
    header = TRUE
  )
  partial_file <- here::here(raw_claims_parts_path, paste0(
    full_claims_prefix, year,
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
    obj_sizes <- sapply(obj_names, \(x) object.size(get(x, envir = env)) / (1024^3)) # Convert bytes to GB
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
hash_cache_dir <- here::here("data-cleaning/debug/cache/partial_md5")
dir.create(hash_cache_dir, recursive = TRUE, showWarnings = FALSE)
calculate_md5 <- function(file_path) {
  md5sum <- digest::digest(file = file_path, algo = "md5")
  return(md5sum)
}
check_md5_changes <- function(year) {
  hash_file_path <- here::here(hash_cache_dir, paste0("md5_hashes_", year, ".rds"))
  current_hashes <- sapply(1:split_parts, \(part) {
    part_file <- here::here(
      raw_claims_parts_path,
      paste0(full_claims_prefix, year, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
    )
    calculate_md5(part_file)
  })
  if (file.exists(hash_file_path)) {
    saved_hashes <- readRDS(hash_file_path)
    if (identical(saved_hashes, current_hashes)) {
      message(paste("No changes in partial files for year", year))
      return(TRUE)
    }
  }
  return(FALSE)
}
check_and_save_md5 <- function(year) {
  if (check_md5_changes(year)) {
    return(TRUE)
  }
  total_rows_check <- 0
  for (part in 1:split_parts) {
    part_rows <- nrow(
      readRDS(
        here::here(
          raw_claims_parts_path,
          paste0(full_claims_prefix, year, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
        )
      )
    )
    total_rows_check <- total_rows_check + part_rows
  }
  expected_total_rows <- readRDS(here::here("data-cleaning/debug/cache/total_rows", paste0("total_rows_", year, ".rds")))
  if (total_rows_check == expected_total_rows) {
    message(paste("Row count matches for year", year, "- saving MD5 hashes."))
    current_hashes <- sapply(1:split_parts, \(part) {
      part_file <- here::here(
        raw_claims_parts_path,
        paste0(full_claims_prefix, year, "_part_", sprintf("%02d", part), "_of_", split_parts, ".rds")
      )
      calculate_md5(part_file)
    })
    saveRDS(current_hashes, here::here(hash_cache_dir, paste0("md5_hashes_", year, ".rds")))
    return(TRUE)
  } else {
    message(paste("Row count mismatch for year", year, "- skipping MD5 save."))
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
  for (year in c(2018:2023)) {
    year <<- year
    message(paste0("Generating samples of size ÷", sample_size_divisor, " for year ", year))
    invisible(source(here::here("data-cleaning/r_scripts_v2/0.1.0.params_fpaths.R")))
    parallel::mclapply(
      1:split_parts,
      \(mclapply_part) {
        sampled_claims_file <- here::here(raw_claims_samples_path, paste0(
          "sampled_claims_", year, "_", sample_size_divisor,
          "_part_", sprintf("%02d", mclapply_part), "_of_", split_parts, ".rds"
        ))
        if (!file.exists(sampled_claims_file)) {
          create_sample_files(mclapply_part, sampled_claims_file)
        }
      },
      mc.cores = nthreads
    )
    message(paste0("Done generating samples of size ÷", sample_size_divisor, " for year ", year))
  }
  message(paste0("Finished sampling for size ÷", sample_size_divisor, " for all years"))
}
```

### 01-drg-cleaning-v2.ipynb
```r
source("~/drg-pipeline/data-cleaning/00a-parameters.r")
system("git submodule update --init --recursive")
required_packages <- c(
  "data.table", # Fast data manipulation
  "here", # Simplifies file path management
  "tictoc", # Timing code execution
  "stringr", # String manipulation
  "stringi", # Unicode string processing
  "lubridate", # Date-time handling
  "profvis", # Profiling R code
  "hash", # Hashing utility
  "future", # Parallel processing
  "future.apply", # Parallelized apply functions
  "knitr", # Dynamic report generation
  "htmlwidgets", # Interactive HTML widgets
  "parallelly", # Advanced parallel computing
  "stringdist", # String distance calculations
  "parallel", # Base parallel computing
  "reticulate", # Interface to Python
  "bigrquery", # BigQuery client
  "jsonlite", # JSON parsing
  "googleCloudStorageR", # Google Cloud Storage access
  "haven", # Read/write Stata, SPSS, SAS files
  "fst", # Fast serialization
  "httr", # HTTP requests
  "ggplot2", # Data visualization
  "rmarkdown", # Dynamic markdown documents
  "digest", # Create cryptographic hashes
  "base64enc", # Base64 encoding/decoding
  "arrow", # Apache Arrow for fast data storage
  "tidyverse", # Collection of data science packages,
  "fasttime" # for fastPOSIXct
)
github_packages <- c(
  "r-lib/styler" # Code formatting
)
invisible(lapply(
  required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
  }
))
invisible(lapply(
  github_packages, function(repo) {
    if (!require(basename(repo), character.only = TRUE)) {
      remotes::install_github(repo)
    }
  }
))
invisible(lapply(required_packages, library, character.only = TRUE))
invisible(lapply(basename(github_packages), library, character.only = TRUE))
year <- 2018
for (file in list.files(
  here::here("data-cleaning/r_scripts_v2"),
  pattern = "\\.R$", full.names = TRUE
)) {
  invisible(source(file))
}
message(year)
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
proc_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.proc")
proc <- load_or_query(proc_query, "proc")
proc[, CODE := as.character(CODE)] # Ensure the CODE column is of character type
rvs_icd9_query <- paste0(
  "SELECT * FROM ",
  gcp_proj, ".phic_libraries.acr_rvs_map"
)
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
acr_rvs_query <- paste0(
  "SELECT * FROM ",
  gcp_proj, ".phic_libraries.acr_procedure"
)
acr_rvs <- load_or_query(acr_rvs_query, "acr_rvs")
i10_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10")
tdrg_icd10 <- load_or_query(i10_query, "tdrg_icd10")
setkey(tdrg_icd10, "CODE") # Set the CODE column as key for efficient lookups
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])
acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
for (code in acc_pdx) {
  assign(code, TRUE, envir = acc_pdx_env)
}
phl_icd10_query <- paste0("SELECT * FROM ", gcp_proj, ".icd.phl_icd10")
phl_icd10 <- load_or_query(phl_icd10_query, "phl_icd10")
neoplasms_dt_actual <- as.data.table(phl_icd10[
  grepl("/", icd10), .(icd10)
][, icd10 := sapply(strsplit(icd10, ","), function(x) trimws(x[2]))])
i10vx_query <- paste0("SELECT * FROM ", gcp_proj, ".grouper_v5.i10vx")
i10vx <- load_or_query(i10vx_query, "i10vx")
setkey(i10vx, "code") # Set the code column as key for efficient lookup
acc_icd <- unique(i10vx[, code]) # Extract unique ICD codes from this table
acc_icd_set <- unique(acc_icd)
hci_query <- paste0("SELECT * FROM ", gcp_proj, ".hci.temp_hci")
hci <- load_or_query(hci_query, "hci")
neoplasm_codes <- unique(neoplasms_dt_actual$icd10) # Unique neoplasm codes
covid_codes <- unique(covid_rvs) # Unique COVID-related codes
rvs_codes <- unique(acr_rvs$rvs) # Unique RVS codes
neoplasm_pattern <- paste0("(", paste(neoplasm_codes, collapse = "|"), ")")
covid_pattern <- paste0("(", paste(covid_codes, collapse = "|"), ")")
rvs_pattern <- paste0("(", paste(rvs_codes, collapse = "|"), ")")
phil_icds <- unique(gsub(
  "[^A-Za-z0-9]", "",
  phl_icd10[!grepl("/", icd10), icd10]
))
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
process_chunk <- function(
    chunk, yr_to_load = year, col_maps = column_mappings,
    known_vals = known_values, remap_master = col_remap_master,
    avail_cols = available_columns) {
  setnames(chunk,
    old = avail_cols[avail_cols %in% names(col_maps)],
    new = unlist(col_maps[avail_cols[avail_cols %in% names(col_maps)]])
  )
  cols <- c("c1", "c2", "c1_orig", "c2_orig")
  vals <- c("clin_c1", "clin_c2", "c1", "c2")
  for (i in seq_along(cols)) set(chunk, j = cols[i], value = chunk[[vals[i]]])
  if (!"id_year" %in% names(chunk)) set(chunk, j = "id_year", value = yr_to_load)
  if (!"pat_bwt" %in% names(chunk)) set(chunk, j = "pat_bwt", value = NA_real_)
  int_cols <- intersect(names(chunk), unlist(expected_types["integer"]))
  num_cols <- intersect(names(chunk), unlist(expected_types["numeric"]))
  char_cols <- intersect(names(chunk), unlist(expected_types["character"]))
  factor_cols <- intersect(names(chunk), unlist(expected_types["factor"]))
  bool_cols <- intersect(names(chunk), c("clin_outpatient", "clin_emergency"))
  chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  chunk[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
  chunk[, (bool_cols) := lapply(.SD, as.logical), .SDcols = bool_cols]
  id_cols <- c("id_series", "id_pin")
  chunk[, id_cols := lapply(.SD, trimws), .SDcols = id_cols]
  time_cols <- c("time_adm", "time_dis")
  chunk[, (time_cols) := lapply(
    .SD,
    function(col) {
      idx <- grepl("AM|PM", col) # Identify rows with AM/PM format
      col[idx] <- format(
        fastPOSIXct(sub("\\.\\d+ ", " ", col[idx]),
          format = "%m/%d/%Y %I:%M:%S %p"
        ),
        "%H:%M"
      )
      col # Return modified column
    }
  ), .SDcols = time_cols]
  c1_c2_cols <- c1_c2_cols
  chunk[, c1_c2_cols := lapply(.SD, clean_column), .SDcols = c1_c2_cols]
  chunk[, c1_c2_cols := lapply(.SD, prep_icd_for_mapping), .SDcols = c1_c2_cols]
  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    clin_icd_list <- c(manual_replacement(clin_icd[[i]]), c1[[i]], c2[[i]])
    return(flatten_then_check_null_na(clin_icd_list))
  })]
  setcolorder(chunk, c(
    "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
    "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
    "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
    "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent",
    "pat_memcat_child", "claim_status", "claim_payout",
    "claim_charge", "clin_discharge", "clin_outpatient",
    "clin_emergency", "clin_acc", "clin_c1", "c1", "clin_c2", "c2",
    "clin_sdx", "clin_proc", "clin_rvs", "clin_pdx", "clin_pdx_source"
  ))
  chunk[, `:=`(
    clin_icd = collapse_clean_icd_rvs_cols(as.list(.SD), TRUE),
    clin_rvs = collapse_clean_icd_rvs_cols(as.list(.SD), FALSE)
  ), .SDcols = patterns("^clin_icd\\d+$", "^clin_rvs\\d+$")]
  chunk[, (patterns("^(clin_icd\\d+|clin_rvs\\d+)$")) := NULL]
  chunk[, (names(chunk)) := lapply(
    .SD,
    function(col) {
      replace_na_or_empty_col(
        replace_na_or_empty_col(
          col, "NA_character_"
        ), "character(0)"
      )
    }
  ), .SDcols = names(chunk)]
  for (col in c1_c2_cols) {
    results <- append_copy_remove_icd_rvs(
      chunk[[col]], chunk$clin_rvs, chunk$clin_icd
    )
    set(chunk, j = "clin_rvs", value = results$clin_rvs)
    set(chunk, j = col, value = results$col)
    set(chunk, j = "clin_icd", value = results$clin_icd)
  }
  chunk[, (names(chunk)) := lapply(
    .SD,
    function(col) {
      replace_na_or_empty_col(
        replace_na_or_empty_col(
          col, "NA_character_"
        ), "character(0)"
      )
    }
  ), .SDcols = names(chunk)]
  remap_cols <- c(
    "pat_type", "pat_memcat_parent",
    "pat_memcat_child", "clin_discharge", "claim_status"
  )
  chunk[, (remap_cols) := lapply(
    .SD, remap_patient_data, remap_master
  ), .SDcols = remap_cols]
  chunk[, icd9_list := map_rvs_icd9(clin_rvs)]
  chunk[, c1_c2_cols := lapply(.SD, function(x) {
    lapply(x, \(y) if (is.null(y) || all(is.na(y))) character(0) else y)
  }), .SDcols = c1_c2_cols]
  icd_cols <- c("c1", "c2", "clin_icd")
  chunk[, icd_cols := lapply(.SD, map_icd10), .SDcols = icd_cols]
  chunk[, (names(chunk)) := lapply(
    .SD,
    replace_na_or_empty_col(
      col, "character(0)"
    )
  ), .SDcols = names(chunk)]
  pdx_inputs <- prep_pdx_inputs(
    chunk$c1, chunk$c2, chunk$clin_icd,
    acc_pdx, neoplasms_dt_actual, acr_rvs, covid_rvs
  )
  pdx_result <- find_pdx(
    pdx_inputs$c1, pdx_inputs$c2, pdx_inputs$clin_icd,
    global_seed
  )
  chunk[, c("clin_pdx", "clin_pdx_source") := .(pdx_result$clin_pdx, pdx_result$clin_pdx_source)]
  chunk[, c("c1", "c2", "clin_icd") := lapply(.SD, function(col) {
    lapply(seq_len(.N), function(i) {
      lst <- col[[i]]
      pdx_val <- pdx[i]
      if (!is.na(pdx_val)) lst <- setdiff(lst, pdx_val)
      return(as.character(lst))
    })
  }), .SDcols = c("c1", "c2", "clin_icd")]
  chunk[, `:=`(
    clin_c1 = c1_orig, clin_c2 = c2_orig,
    clin_proc = icd9_list
  )][, `:=`(c1_orig = NULL, c2_orig = NULL, icd9_list = NULL)]
  chunk[, clin_sdx := Map(
    function(pdx_var, sdx_var) {
      sdx_var[sdx_var != pdx_var]
    }, pdx, clin_icd
  )][, clin_icd := NULL]
  if (!"pat_bdate" %in% names(chunk)) chunk[, pat_bdate := NA_Date_]
  date_cols <- c(
    "date_adm", "date_dis", "date_rec", "date_ref",
    "date_check", "pat_bdate", "date_ext"
  )
  chunk[, (date_cols) := lapply(.SD, function(x) {
    x <- fastPOSIXct(x, tz = "UTC") # Faster parsing
    x[x < as.POSIXct("1900-01-01", tz = "UTC")] <- NA_Date_
    as.Date(x) # Convert to Date format
  }), .SDcols = date_cols]
  chunk[, c("time_adm", "time_dis") := lapply(.SD, function(x) {
    as.character(ifelse(is.na(x), "00:00:00", paste0(x, ":00")))
  }), .SDcols = c("time_adm", "time_dis")]
  chunk[, date_adm := as.POSIXct(paste(date_adm, time_adm),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]
  chunk[, date_dis := as.POSIXct(paste(date_dis, time_dis),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]
  chunk[, c("clin_outpatient", "clin_emergency") := lapply(.SD, function(col) {
    as.logical(as.integer(col))
  }), .SDcols = c("clin_outpatient", "clin_emergency")]
  num_cols <- c(
    "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
    "claim_charge", "id_year", "clin_pdx_source"
  )
  chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  int_cols <- c("clin_discharge", "id_year", "clin_pdx_source")
  chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  char_cols <- c(
    "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
    "pat_memcat_parent", "pat_memcat_child", "claim_status", "clin_pdx"
  )
  chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  char_cols <- names(chunk)[sapply(chunk, is.character)]
  chunk[, pat_ageday := NA_integer_]
  chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
    valid_codes <- codes[codes %chin% acc_icd_set]
    return(ifelse(length(valid_codes) > 0, valid_codes, NA_character_))
  })]
  chunk[, clin_sdx := lapply(clin_sdx, function(x) {
    if (is.null(x)) character(0) else unlist(x)
  })]
  chunk[, (names(chunk)) := lapply(
    .SD,
    replace_na_or_empty_col,
    "character(0)"
  ), .SDcols = names(chunk)]
  chunk[, (char_cols) := lapply(.SD, function(col) {
    col <- iconv(col, from = "", to = "UTF-8")
    return(fifelse(col %chin% c("None", ""), NA_character_, col))
  }), .SDcols = char_cols]
  num_cols <- names(chunk)[sapply(chunk, is.numeric)]
  chunk[, (num_cols) := lapply(.SD, function(col) {
    col[is.nan(col)] <- NA_real_
    return(col)
  }), .SDcols = num_cols]
  chunk[, id_hcp := lapply(
    strsplit(id_hcp, "\\s*,\\s*|\\|\\||\\|"),
    function(y) {
      if (is.null(y) || length(y) == 0L || all(is.na(y))) character(0) else y
    }
  )]
  trim_cols <- list(clin_sdx = 12, clin_proc = 20)
  chunk[, (names(trim_cols)) := lapply(
    .SD,
    function(col, n) {
      trimmed <- head(col, n)
      return(trimmed)
    }
  ),
  .SDcols = names(trim_cols),
  n = unname(trim_cols)
  ]
  chunk[, pat_age := fifelse(
    !is.na(pat_bdate) & !is.na(date_adm),
    floor(as.numeric(date_adm - pat_bdate) / 365.25),
    NA_integer_
  )]
  chunk[!is.na(pat_bdate) & !is.na(date_adm) & !is.na(pat_age) &
    pat_bdate > as.Date(date_adm), pat_bdate := NA_Date_]
  chunk[grepl("99432", c1) & !is.na(pat_age) & pat_age < 0 &
    pat_age >= -1, pat_age := 0]
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
for (loop_part in 1:split_parts) {
  start_time <- Sys.time() # Record start time for processing
  read_in_dt <- read_appropriate_file(loop_part)
  chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
  chunks <- split(read_in_dt, rep(1:nthreads,
    each = chunk_size,
    length.out = nrow(read_in_dt)
  ))
  cat(paste0("\rStart processing part  ", loop_part, " of ", split_parts))
  flush.console()
  parallel_results <-
    if (to_parallel) {
      mclapply(chunks, process_chunk, mc.cores = nthreads)
    } else if (!to_debug) {
      lapply(chunks, process_chunk)
    } else {
      list(process_chunk(chunks[[1]]))
    }
  summarized_dt <- rbindlist(parallel_results)
  if (to_write) {
    saveRDS(
      summarized_dt, here(chkpt_1_path, paste0(
        chkpt_1_prefix, year, suffix,
        "part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
      )),
      compress = TRUE
    )
  }
  processing_times[[loop_part]] <- as.numeric(difftime(Sys.time(),
    start_time,
    units = "secs"
  ))
  print_status_update(loop_part, split_parts, processing_times, "clean")
  rm(read_in_dt, summarized_dt)
  invisible(gc())
}
master_dt_list <- mclapply(1:split_parts,
  function(split_part) {
    read_part <- readRDS(
      here(chkpt_1_path, paste0(
        chkpt_1_prefix, year, suffix, "part_",
        sprintf("%02d", split_part), "_of_", split_parts, ".rds"
      ))
    )
    return(read_part)
  },
  mc.cores = nthreads
)
message("Commencing rbindlist")
master_dt <- rbindlist(master_dt_list, fill = TRUE)
rm(master_dt_list)
invisible(gc())
message("Finished rbindlist")
if (to_write) {
  message("Commencing saveRDS")
  saveRDS(master_dt, here(
    chkpt_2_path, paste0(
      chkpt_2_prefix, year, suffix, ".rds"
    )
  ), compress = TRUE)
  message("Finished saveRDS")
}
message(paste0(
  "Saving ",
  paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
))
saveRDS(master_dt, here(
  chkpt_2_path,
  paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
), compress = TRUE)
message(paste0(
  "Finished saving ",
  paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
))
fwrite(
  readRDS(here(
    chkpt_2_path,
    paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
  )),
  "~/drg-pipeline/data-cleaning/debug/test.csv"
)
result <- readRDS(here(
  chkpt_2_path,
  paste0(chkpt_2_prefix, year, suffix, "tmp", ".rds")
))
result[, is_covid := {
  covid_found <- rep(FALSE, .N) # Initialize all rows as FALSE
  not_found <- !covid_found
  covid_found[not_found] <- clin_c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- clin_c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c2[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- c1[not_found] %chin% covid_rvs
  not_found <- !covid_found
  covid_found[not_found] <- sapply(
    clin_rvs[not_found],
    function(row) any(row %chin% covid_rvs)
  ) # Check procedure codes
  not_found <- !covid_found
  covid_found[not_found] <- sapply(
    clin_sdx[not_found],
    function(row) any(row %chin% covid_rvs)
  ) # Check supporting diagnoses
  not_found <- !covid_found
  covid_found[not_found] <- sapply(
    clin_proc[not_found],
    function(row) any(row %chin% covid_rvs)
  ) # Check performed procedures
  covid_found # Return logical vector of COVID matches
}]
result <- result[, .(
  id_series, id_pin, id_hci, id_hcp,
  date_adm, date_dis, date_rec, date_ref, date_check,
  pat_type, pat_rel, pat_age, pat_ageday, pat_sex,
  pat_bwt, pat_memcat_parent, pat_memcat_child,
  claim_status, claim_payout, claim_charge, is_covid,
  clin_discharge, clin_outpatient, clin_emergency, clin_acc,
  clin_c1, clin_c2, clin_sdx, clin_proc, clin_pdx, clin_pdx_source
)]
saveRDS(result, here(
  chkpt_2_path,
  paste0(chkpt_2_prefix, year, suffix, "final", ".rds")
))
if (to_bq) {
  if (!to_sample) bq_table <- paste0("claims_", year)
  tryCatch(
    bq_table_delete(bq_table(gcp_proj, bq_dataset, bq_table)),
    error = function(e) {
      if (grepl("Not found", e, ignore.case = TRUE)) {
        message("Table does not exist, nothing to drop.")
      } else {
        stop(e)
      }
    }
  )
  tryCatch(
    bq_table_create(
      bq_table(gcp_proj, bq_dataset, bq_table),
      fields = fromJSON(here(
        "data-cleaning/r_scripts_v2",
        "bq_schema_cleaning.json"
      ), simplifyDataFrame = FALSE)
    ),
    error = function(e) {
      if (grepl("already exists", e, ignore.case = TRUE)) {
        message("Table already exists. Skipping creation and upload.")
      } else {
        stop(e)
      }
    }
  )
  if (to_write) {
    chunk_size <- 250000
    num_chunks <- ceiling(nrow(result) / chunk_size)
    for (i in seq_len(num_chunks)) {
      chunk <- result[
        ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(result)),
      ]
      bq_table_upload(
        bq_table(gcp_proj, bq_dataset, bq_table),
        values = chunk,
        write_disposition = if (i == 1) "WRITE_EMPTY" else "WRITE_APPEND"
      )
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
  "rmarkdown", "digest", "base64enc", "arrow", "tidyverse"
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
message(year)
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
][, icd10 := sapply(strsplit(icd10, ","), \(x) trimws(x[2]))])
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
    result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_time", ".rds")))
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
    result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, \(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
    result[!zero_mask, pat_bwt := NA_real_]
    cat("\rWriting final\n")
    flush.console()
    saveRDS(result, here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
  } else if (to_thai_all_years) {
    for (year in c(2018:2023)) {
      year <<- year
      year <- year
      cat("\rReading final\n")
      flush.console()
      result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_time", ".rds")))
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
      result[(is.na(pat_bwt) | pat_bwt <= 0) & zero_mask, pat_bwt := sapply(.SD$pat_bwt, \(x) sample(bw_dist, 1)), .SDcols = "pat_bwt"]
      result[!zero_mask, pat_bwt := NA_real_]
      cat("\rWriting final\n")
      flush.console()
      saveRDS(result, here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
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
  result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
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
      \(i) sapply(split_list, \(x) if (length(x) >= i) x[[i]] else NA_character_),
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
  fwrite(for_fwrite, here(chkpt_7_path, paste0(chkpt_7b_prefix, suffix, ".csv")))
  message("Creating summary table")
  summary_table <- for_fwrite[, lapply(.SD, \(x) sum(!is.na(x))), .SDcols = names(for_fwrite)]
  summary_table <- transpose(summary_table)
  setnames(summary_table, "Non-Null Count")
  summary_table[, Column := names(for_fwrite)]
  setcolorder(summary_table, c("Column", "Non-Null Count"))
  message("Printing summary table")
  print(summary_table)
  saveRDS(for_fwrite, here(chkpt_7_path, paste0("for_fwrite_", year, suffix, ".rds")))
}
if (to_python) {
  if (!to_generate_py_fwrite && to_generate_feather) for_fwrite <- readRDS(here(chkpt_7_path, paste0("for_fwrite_", year, suffix, ".rds")))
  if (to_generate_feather) write_feather(as.data.frame(for_fwrite), here(chkpt_7_path, paste0("python_input_", year, suffix, ".feather")))
  if (to_py_prompt) {
    response <- tolower(readline(prompt = "Have you run the Python grouper manually? (y/n): "))
    if (response != "y") {
      stop("Python Grouper not run yet. Script terminated. Continue on manually if necessary")
    }
    message("Continuing with the script...\n")
  } else {
    message("Python Grouper is assumed to have been run already. Continuing with the script...\n")
  }
  output_dt <- as.data.table(read_feather(here(chkpt_8_path, paste0("python_output_", year, suffix, ".feather"))))
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
    lapply(col, \(x) {
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
    paste0("python_", year)
  } else {
    paste0("temp_python_", year)
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
        result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "stata_subset_with_bdate", ".rds")))
      } else {
        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
      }
      result[, caseid := as.character(seq_len(nrow(result)))]
      result_mapping <- result[, .(id_series, caseid)]
      cat("\rExporting for grouper\n")
      flush.console()
      chunk_size <- 5000000
      num_chunks <- ceiling(nrow(result) / chunk_size)
      for (i in seq_len(num_chunks)) {
        output_file <- here(
          chkpt_4_path,
          paste0(
            chkpt_4_prefix, year, suffix,
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
      for (year in c(2018:2023)) {
        year <<- year
        year <- year
        cat("\rReading final\n")
        flush.console()
        result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
        result[, caseid := as.character(seq_len(nrow(result)))]
        result_mapping <- result[, .(id_series, caseid)]
        cat("\rExporting for grouper\n")
        flush.console()
        chunk_size <- 5000000
        num_chunks <- ceiling(nrow(result) / chunk_size)
        for (i in seq_len(num_chunks)) {
          output_file <- here(
            chkpt_4_path,
            paste0(
              chkpt_4_prefix, year, suffix,
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
    result <- readRDS(here(chkpt_2_path, paste0(chkpt_2_prefix, year, suffix, "final_subset_with_bdate_with_time", ".rds")))
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
            chkpt_5_prefix, year, suffix,
            "part_", i, "_of_", num_chunks
          )
        ),
        "Res.TXT"
      )
      local_file <- here(
        chkpt_5_path,
        paste0(
          toupper(
            paste0(
              chkpt_5_prefix, year, suffix,
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
    fwrite(diff_merged, here(chkpt_9_path, paste0("chkpt_9_grouper_differences_", year, suffix, ".csv")))
  }
}
if (to_python && to_thai && !to_thai_all_years) {
  if (exists("merged")) { # str(merged)
    if (to_debug) fwrite(merged, paste0(year, "test4.csv"))
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
  saveRDS(result_after_thai, here(chkpt_6_path, paste0(chkpt_6_prefix, year, suffix, ".rds")))
  prefix <- if (!to_sample) "thai_" else "temp_thai_"
  bq_table <- paste0(prefix, year)
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
year_file_path = os.path.expanduser("~/drg-pipeline/data-cleaning/debug/cache/year.txt")
with open(year_file_path, "r") as file:
    year = file.read().strip()  # .strip() removes any surrounding whitespace or newlines
if to_sample:
    suffix = f"_sampled_{sample_size_divisor}_"
else:
    suffix = "_full_"
print(suffix)
print(year)
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
feather_file_path = f"~/drg-pipeline/data-cleaning/data/chkpts/chkpt_7_py_input/python_input_{year}{suffix}.feather"
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
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/chkpts/chkpt_7_py_input/python_final_input_{year}{suffix}.feather"
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
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/chkpts/chkpt_8_py_output/python_output_{year}{suffix}.feather"
pandas_df.to_feather(file_path)
```
