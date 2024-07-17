suppressPackageStartupMessages({
  # library(rprojroot)
  # library(conflicted)
  # library(tidyverse)
  library(data.table)
  library(here)
  library(tictoc)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(docstring)
  library(profvis)
  library(hash)
  # library(foreach)
  # library(doParallel)
  # library(parallel)
  library(future)
  library(future.apply)
  library(knitr)
})
# print("Packages loaded successfully.")

source(here("data-cleaning", "r_scripts", "libraries.R"))

year_to_load <- "2018"
version <- "v2"

sample_size <- 1 * 1e3
seed <- 123

drop_cols <- c(
  paste0("ICDCODE", 13:14),
  "ICCODED15",
  paste0("ICDCODE", 16:170)
)
icd_cols <- paste0("clin_icd", 1:12)
rvs_cols <- paste0("clin_rvs", 1:20)

to_read <- FALSE
to_sample <- TRUE
to_write <- TRUE
to_group <- TRUE
to_filter <- FALSE # unused
to_profvis <- FALSE
to_chunk <- TRUE
to_view_checks <- TRUE

set.seed(seed)

options(future.globals.maxSize = 1024 * 1024^2)

global_seed <- seed # for parallelized operations

na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

# Define column types
character_cols <- c(
  "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "HCI_PMCC_NO", "HCP_NO_LIST",
  "PRIMARY_ILLNESS", "SECONDARY_ILLNESS", paste0("ICDCODE", c(1:14, 16:170)),
  "ICCODED15", paste0("RVSCODE", 1:20), "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT",
  "PAT_BDAY", "EXTRACTION_DATE"
)
integer_cols <- c("OUT_PATIENT", "EMERGENCY")
factor_cols <- c(
  "PATIENT_TYPE", "ROOM_TYPE", "DEP_REL", "PATSEX", "MEMCAT_PARENT_DESC",
  "MEMCAT_CHILD_DESC",
  "MEMCAT_SUBCHILD_DESC", "DISPOSITION", "CLAIMS_STATUS"
)
numeric_cols <- c(
  "PATAGE", "PAT_BWT_KG", "CLAIMS_PAID_AMT",
  "ACR_AMOUNT_ACTUAL"
)

covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

# Define column classes
col_classes <- c(
  rep("character", length(character_cols)),
  rep("integer", length(integer_cols)),
  rep("factor", length(factor_cols)),
  rep("numeric", length(numeric_cols))
)

names(col_classes) <- c(character_cols, integer_cols, factor_cols, numeric_cols)

old_colnames <- c(
  "SRC_YR", "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT", "EXTRACTION_DATE",
  "HCI_PMCC_NO", "HCP_NO_LIST", "PATIENT_TYPE", "DEP_REL", "PATSEX", "PATAGE",
  "PAT_BDAY", "PAT_BWT_KG", "MEMCAT_PARENT_DESC", "MEMCAT_CHILD_DESC",
  "MEMCAT_SUBCHILD_DESC", "OUT_PATIENT", "EMERGENCY", "ROOM_TYPE",
  "DISPOSITION", "PRIMARY_ILLNESS", "SECONDARY_ILLNESS",
  paste0("ICDCODE", 1:12), paste0("RVSCODE", 1:20),
  "CLAIMS_STATUS", "ACR_AMOUNT_ACTUAL", "CLAIMS_PAID_AMT"
)

new_colnames <- c(
  "id_year", "id_series", "id_pin", "date_adm", "time_adm",
  "date_dis", "time_dis", "date_rec", "date_ref", "date_check", "date_ext",
  "id_hci", "id_hcp", "pat_type", "pat_rel", "pat_sex", "pat_age",
  "pat_bdate", "pat_bwt", "pat_memcat_parent", "pat_memcat_child",
  "pat_memcat_subchild", "clin_outpatient", "clin_emergency", "clin_acc",
  "clin_discharge", "clin_c1", "clin_c2", paste0("clin_icd", 1:12),
  paste0("clin_rvs", 1:20), "claim_status", "claim_charge", "claim_payout"
)

path_to_raw_claims <- "git-ignored-files/raw-claims"
path_to_intermediate <- "git-ignored-files/intermediate-claims"
path_to_cache <- "data-cleaning/cache"
path_to_aux <- "git-ignored-files/aux-files"
path_to_excel <- "git-ignored-files/Excel"
path_to_cleaned_claims <- "git-ignored-files/cleaned-claims"
path_to_grouper_output <- "git-ignored-files/grouper-output"
path_to_chunks <- "git-ignored-files/chunked-samples"

# Here() let's you find files in your project directory
suffix <- paste0(ifelse(to_sample, "_sampled_", "_full_"), version)
sampled_claims <- here(
  path_to_raw_claims,
  paste0(
    "sampled_claims_extract_CLAIMS_", year_to_load,
    suffix, paste0("_", sample_size), ".csv"
  )
)
full_claims <- here(
  path_to_raw_claims,
  paste0("claims_extract_CLAIMS_", year_to_load, ".csv")
)
intermediate_file <- here(
  path_to_intermediate,
  paste0("intermediate_claims_", year_to_load, "_processed", suffix, ".csv")
)
cleaned_claims_file <- here(
  path_to_cleaned_claims,
  paste0("cleaned_claims_extract_CLAIMS_", year_to_load, suffix, ".csv")
)
output_txt_file <- here(
  path_to_grouper_output,
  paste0("DRG_Grouped", "_", year_to_load, suffix, ".txt")
)
grouper_result_file <- here(
  path_to_grouper_output,
  toupper(paste0("DRG_Grouped", "_", year_to_load, suffix, "Res.TXT"))
)
total_rows_file <- here(
  path_to_cache,
  paste0("total_rows_", year_to_load, ".rds")
)

read_entire_file <- function(drop_cols) {
  dt <- fread(full_claims,
    na.strings = na_values, drop = drop_cols,
    colClasses = col_classes
  )
  return(dt)
}

read_sampled_file <- function() {
  dt <- fread(sampled_claims, na.strings = na_values, colClasses = col_classes)
  return(dt)
}

sample_data <- function(dt) {
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

rename_columns <- function(dt) {
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_column <- function(column_to_clean, na_like_strings) {
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- iconv(column_to_clean, to = "UTF-8", sub = "byte")
  cleaned_col <- toupper(cleaned_col)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[ \n]", "")
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d\\/\\s]+", "")
  cleaned_col <- stri_trim_both(cleaned_col)
  cleaned_col <- ifelse(cleaned_col %in% na_like_strings,
    NA_character_, cleaned_col
  )

  return(cleaned_col)
}

collapse_columns <- function(cols_to_process, na_like_strings) {
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_column(col, na_like_strings)
  })
  collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
  collapsed_column <- ifelse(collapsed_column %in% na_like_strings,
    NA_character_, collapsed_column
  )
  return(collapsed_column)
}

replace_empty_with_na <- function(dt, to_view_checks) {
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  if (to_view_checks) {
    replacement_summary <- data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    )
  }

  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Using set to avoid copying
    dt[
      get(col_name) == "" |
        get(col_name) == "NA" |
        get(col_name) == "character(0)", (col_name) := NA_character_
    ]

    if (is.factor(col)) {
      set(dt, j = col_name, value = factor(dt[[col_name]],
        levels = c(levels(col), NA)
      ))
    }

    if (to_view_checks) {
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out rows where all counts are zero
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]

    if (nrow(replacement_summary) > 0) {
      print(kable(replacement_summary,
        format = "markdown",
        col.names = c(
          "Column", "\"\" Replaced",
          "\"NA\" Replaced", "\"character(0)\" Replaced"
        )
      ))
    } else {
      cat("No replacements were made.\n")
    }
  }

  return(dt)
}

split_to_vector <- function(column) {
  result <- lapply(column, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })
  return(result)
}

remap_patient_type <- function(pat_type) {
  known_types <- c("MEMBER", "DEPENDENT")
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )
  unknown_types <- setdiff(
    pat_type[!is.na(pat_type)],
    known_types
  )
  list(remapped = remapped_pat_type, unmapped = unknown_types)
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  known_parents <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR")
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )
  unknown_parents <- setdiff(
    pat_memcat_parent[!is.na(pat_memcat_parent)],
    known_parents
  )
  list(remapped = remapped_memcat_parent, unmapped = unknown_parents)
}

remap_memcat_child_desc <- function(pat_memcat_child) {
  known_children <- c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER"
  )
  remapped_memcat_child <- fcase(
    pat_memcat_child == "EMPLOYED PRIVATE", "FORMAL",
    pat_memcat_child == "SELF-EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "SENIOR CITIZEN", "SENIOR",
    pat_memcat_child == "INDIGENT", "INDIGENT",
    pat_memcat_child == "LIFETIME MEMBER", "LIFETIME",
    pat_memcat_child == "SPONSORED", "SPONSORED",
    pat_memcat_child == "MIGRANT WORKER", "INFORMAL",
    pat_memcat_child == "EMPLOYED GOVERNMENT", "FORMAL",
    pat_memcat_child == "INFORMAL ECONOMY", "INFORMAL",
    pat_memcat_child == "HOUSEHOLD HELP/KASAMBAHAY", "FORMAL",
    pat_memcat_child == "FOREIGN NATIONAL", "INFORMAL",
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "INFORMAL",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL"
  )
  unknown_children <- setdiff(
    pat_memcat_child[!is.na(pat_memcat_child)],
    known_children
  )
  list(remapped = remapped_memcat_child, unmapped = unknown_children)
}

remap_disposition <- function(clin_discharge) {
  known_dispositions <- c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE",
    "ABSCONDED", "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED"
  )
  remapped_discharge <- fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )
  unknown_dispositions <- setdiff(
    clin_discharge[!is.na(clin_discharge)],
    known_dispositions
  )
  list(remapped = remapped_discharge, unmapped = unknown_dispositions)
}


format_large_numbers <- function(x) {
  if (x >= 1e9) {
    return(sprintf("%.1fb", x / 1e9))
  } else if (x >= 1e6) {
    return(sprintf("%.1fm", x / 1e6))
  } else if (x >= 1e3) {
    return(sprintf("%.1fk", x / 1e3))
  } else {
    return(as.character(x))
  }
}

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  require(lubridate)

  dob <- rep(NA_character_, length(ages))

  # Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(
    mdy(bdays[valid_bdays_indices]),
    "%d/%m/%Y"
  )

  # Identify indices where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- mdy(date_adms[missing_bday_indices])

  # Handle cases where ages are zero
  zero_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
  )
  dob[missing_bday_indices[zero_age_indices]] <- format(
    ref_dates[zero_age_indices] - days(
      sample(
        1:27, length(zero_age_indices),
        replace = TRUE
      )
    ), "%d/%m/%Y"
  )

  # Handle cases where ages are positive
  positive_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
  )
  truncated_ages <- floor(
    ages[missing_bday_indices][positive_age_indices]
  )
  dob[missing_bday_indices[positive_age_indices]] <- format(
    ref_dates[positive_age_indices] - years(truncated_ages) - days(
      sample(1:170, length(positive_age_indices), replace = TRUE)
    ), "%d/%m/%Y"
  )

  return(dob)
}

generate_dob_column <- function(dt) {
  generate_dob_vectorized(dt$pat_bdate, dt$pat_age, dt$date_adm)
}

format_dates <- function(date_vector) {
  format(mdy(date_vector), "%d/%m/%Y")
}

format_times <- function(time_vector) {
  gsub(":", "", time_vector)
}

split_icd_codes_for_batch_grouper <- function(icd_str) {
  codes <- unlist(icd_str)
  length(codes) <- 12
  codes
}

split_rvs_codes_for_batch_grouper <- function(rvs_str) {
  codes <- unlist(rvs_str)
  length(codes) <- 20
  codes
}

prepare_and_write_output <- function(output_dt, output_txt_file) {
  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  # Write the data.table to a file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}

export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob_column(dt)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format_dates(dt$date_adm)]
  output_dt[, TimeAdm := format_times(dt$time_adm)]
  output_dt[, DateDsc := format_dates(dt$date_dis)]
  output_dt[, TimeDsc := format_times(dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]

  icd_codes_list <- lapply(dt$clin_icd, split_icd_codes_for_batch_grouper)
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes_for_batch_grouper)
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  prepare_and_write_output(output_dt, output_txt_file)
}

remove_lumped_icd_codes <- function(column) {
  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

create_thai_icd10_environment <- function(thai_icd10_codes) {
  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

find_direct_icd_matches <- function(icds, thai_icd10_env) {
  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  icd_mapping <- list()
  modified_count <- 0
  for (d in icds) {
    d <- str_trim(d)
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (
      !exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        for (i in seq_len(nchar(d) - 3)) {
          new_d <- substr(d, 1, nchar(d) - i)
          if (exists(new_d, thai_icd10_env)) {
            icd_mapping[[d]] <- new_d
            modified_count <- modified_count + 1
            break
          }
        }
      }
    }
  }
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}

# Helper function to map ICD-10 codes to columns
apply_icd10_mapping_to_columns <- function(clin_c1, clin_c2, clin_icd, icd10_env) {
  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }

  clin_c1_mapped <- lapply(clin_c1, map_icd10_helper)
  clin_c2_mapped <- lapply(clin_c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  return(
    list(
      clin_c1 = clin_c1_mapped,
      clin_c2 = clin_c2_mapped,
      clin_icd = clin_icd_mapped
    )
  )
}

# Main function to process ICD-10 mappings
implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10, rows_to_show = Inf) {
  # Extract unique ICD codes
  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  # Create environments for Thai ICD-10 codes and neoplasms
  thai_icd10_env <- create_thai_icd10_environment(unique(tdrg_icd10$CODE))
  neoplasms_env <- create_thai_icd10_environment(unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"]))

  # Identify direct matches
  direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      length(icds), length(direct_match_codes),
      length(direct_match_codes) * 100 / length(icds)
    ),
    " are directly in the Thai ICD-10 library\n"
  )

  # Create ICD-10 mapping
  icd_mapping_info <- generate_icd10_mapping(icds, thai_icd10_env, neoplasms_env)
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf(
    "The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n",
    length(icd_mapping)
  ))
  cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

  # Identify unmatched ICD codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf("There are %d codes that could not be mapped to the Thai ICD10 library:\n", length(unmatched_icds)))
    unmatched_sources <- data.table(code = unmatched_icds, source = NA_character_, count = 0)

    for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[code %in% unlist(col_values), source := col_name]
      unmatched_sources[code %in% unlist(col_values), count := count + table(unlist(col_values))[code]]
    }

    unmatched_sources <- unmatched_sources[order(-count)]
    print(kable(head(unmatched_sources, rows_to_show), format = "markdown", caption = "Unmapped ICD Codes"))
  }

  # Create ICD-10 mapping data.table and environment
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  icd10_env <- list2env(setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10))

  # Map ICD-10 codes in the specific columns
  return(apply_icd10_mapping_to_columns(clin_c1, clin_c2, clin_icd, icd10_env))
}

ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  # Convert lists to data.table for efficient processing
  dt <- data.table(clin_c1 = clin_c1, clin_c2 = clin_c2, clin_icd = clin_icd)

  # Deduplicate each column
  dt[, clin_c1 := lapply(clin_c1, unique)]
  dt[, clin_c2 := lapply(clin_c2, unique)]
  dt[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  dt[, clin_icd := Map(function(c1, c2, icd) setdiff(icd, union(c1, c2)), clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  dt[, clin_c1 := Map(function(c1, c2) setdiff(c1, c2), clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  dt[, clin_c2 := Map(function(c1, c2) setdiff(c2, c1), clin_c1, clin_c2)]

  return(list(clin_c1 = dt$clin_c1, clin_c2 = dt$clin_c2, clin_icd = dt$clin_icd))
}

generate_comparison_table <- function(original, modified) {
  original_unlisted <- unlist(original, use.names = FALSE)
  modified_unlisted <- unlist(modified, use.names = FALSE)

  comparison <- data.table(old_code = original_unlisted, new_code = modified_unlisted)

  comparison <- comparison[old_code != new_code, .(count = .N), by = .(old_code, new_code)]

  return(comparison)
}

pad_list_elements <- function(list1, list2) {
  max_length <- max(lengths(list1), lengths(list2))

  pad_with_na <- function(lst, max_length) {
    lapply(lst, function(x) {
      if (length(x) < max_length) {
        x <- c(x, rep(NA, max_length - length(x)))
      }
      return(x)
    })
  }

  list1 <- pad_with_na(list1, max_length)
  list2 <- pad_with_na(list2, max_length)

  return(list(list1, list2))
}

map_then_compare_icd_mappings <- function(tdrg_icd10, rows_to_show = Inf, invalid_rows_to_show = Inf) {
  # Ensure the dt variable is in the global environment
  if (!exists("dt", envir = .GlobalEnv)) {
    stop("The global variable 'dt' does not exist.")
  }

  # Store the original data for comparison
  original_dt <- data.table::copy(dt)

  # Process ICD-10 mappings
  mapped_columns <- implement_icd10_mapping(
    original_dt$clin_c1, original_dt$clin_c2,
    original_dt$clin_icd, tdrg_icd10,
    rows_to_show = rows_to_show
  )

  # Update the global dt with mapped columns
  dt$clin_c1 <- mapped_columns$clin_c1
  dt$clin_c2 <- mapped_columns$clin_c2
  dt$clin_icd <- mapped_columns$clin_icd

  # Ensure unique ICD codes
  unique_icd_codes <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt$clin_c1 <- unique_icd_codes$clin_c1
  dt$clin_c2 <- unique_icd_codes$clin_c2
  dt$clin_icd <- unique_icd_codes$clin_icd

  # Pad lists to ensure they have the same length
  padded_c1 <- pad_list_elements(original_dt$clin_c1, dt$clin_c1)
  original_dt$clin_c1 <- padded_c1[[1]]
  dt$clin_c1 <- padded_c1[[2]]

  padded_c2 <- pad_list_elements(original_dt$clin_c2, dt$clin_c2)
  original_dt$clin_c2 <- padded_c2[[1]]
  dt$clin_c2 <- padded_c2[[2]]

  padded_icd <- pad_list_elements(original_dt$clin_icd, dt$clin_icd)
  original_dt$clin_icd <- padded_icd[[1]]
  dt$clin_icd <- padded_icd[[2]]

  # Generate comparison table
  comparison_table <- rbind(
    generate_comparison_table(original_dt$clin_c1, dt$clin_c1),
    generate_comparison_table(original_dt$clin_c2, dt$clin_c2),
    generate_comparison_table(original_dt$clin_icd, dt$clin_icd)
  )

  # Sort the comparison table by count in descending order
  comparison_table <- comparison_table[order(-count)]

  # Print the kable output with a specified number of rows
  print(kable(head(comparison_table, rows_to_show),
    format = "markdown",
    caption = "Comparison of ICD Codes Before and After Mapping"
  ))

  # Check if all resulting ICD codes are in either the Thai library or the PhilHealth library
  all_icds <- unique(
    c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd))
  )
  valid_icds <- unique(c(tdrg_icd10$CODE, rvs_icd9$icd9cm))
  invalid_icds <- setdiff(all_icds, valid_icds)
  invalid_icds <- invalid_icds[!is.na(invalid_icds) & invalid_icds != "NA"]

  if (length(invalid_icds) > 0) {
    invalid_icds_table <- data.table(
      code = invalid_icds,
      count = sapply(
        invalid_icds,
        function(icd) {
          sum(c(
            unlist(dt$clin_c1),
            unlist(dt$clin_c2),
            unlist(dt$clin_icd)
          ) == icd, na.rm = TRUE)
        }
      )
    )

    invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
    invalid_icds_table <- invalid_icds_table[order(-count)]

    print(kable(head(invalid_icds_table, invalid_rows_to_show),
      format = "markdown",
      caption = "Invalid ICD Codes Not Found in Thai or PhilHealth Libraries"
    ))
  } else {
    cat("All resulting ICD codes are valid and present in the libraries.\n")
  }
}

main_read_function <- function() {
  if (to_read) {
    if (to_view_checks) {
      print("Reading the entire file...")
    }
    dt <- read_entire_file(drop_cols)

    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste(
              "Sampled file does not match sample size. Expected:",
              sample_size, "Found:", nrow(dt), "Re-sampling..."
            ))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste(
                "to_write is TRUE. Writing the new sample data to file:",
                sampled_claims
              ))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist. Creating new sample...")
        }
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste(
              "to_write is TRUE. Writing the new sample data to file:",
              sampled_claims
            ))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    }
  } else {
    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste(
              "Sampled file does not match sample size. Expected:",
              sample_size, "Found:", nrow(dt), "Re-sampling..."
            ))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste(
                "to_write is TRUE. Writing the new sample data to file:",
                sampled_claims
              ))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist.")
          print("Reading entire file and creating new sample...")
        }
        dt <- read_entire_file(drop_cols)
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste(
              "to_write is TRUE. Writing the new sample data to file:",
              sampled_claims
            ))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    } else {
      if (!file.exists(intermediate_file)) {
        stop("Cannot proceed: to_read is FALSE and to_sample is FALSE.
           At least one must be TRUE.")
      } else {
        if (to_view_checks) {
          print("Using existing intermediate file.")
        }
      }
    }
  }
  return(dt)
}

clean_data <- function(dt) {
  # Add year column
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if all columns were successfully renamed
  if (!all(new_colnames %in% colnames(dt))) {
    missing_cols <- setdiff(new_colnames, colnames(dt))
    warning("Failed to rename the following columns: ", paste(missing_cols, collapse = ", "))
    stop("Column renaming failed.")
  }

  if (to_view_checks) {
    print("Successfully renamed columns; All expected columns exist")
  }

  # Collapse columns clin_icd1 to clin_icd12 into clin_icd
  dt[, clin_icd := collapse_columns(
    mget(paste0("clin_icd", 1:12),
      envir = as.environment(dt)
    ),
    na_like_strings
  )]
  dt[, paste0("clin_icd", 1:12) := NULL]

  # Collapse columns clin_rvs1 to clin_rvs20 into clin_rvs
  dt[, clin_rvs := collapse_columns(
    mget(paste0("clin_rvs", 1:20),
      envir = as.environment(dt)
    ),
    na_like_strings
  )]
  dt[, paste0("clin_rvs", 1:20) := NULL]

  # Remove lumped ICD codes from clin_icd
  dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]

  # Turn clin_icd and clin_rvs into lists
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]

  # Clean and unlump clin_c1 and clin_c2
  dt[, clin_c1_orig := clin_c1]
  dt[, clin_c1 := clean_column(dt$clin_c1, na_like_strings)]
  clin_c1_cleaning_comparison <- dt[
    clin_c1 != clin_c1_orig,
    .(clin_c1_orig, clin_c1)
  ]
  if (to_view_checks) {
    print(head(clin_c1_cleaning_comparison)) # Check: Print head of changes
  }
  dt[, clin_c1_orig := NULL]

  dt[, clin_c2_orig := clin_c2]
  dt[, clin_c2 := clean_column(dt$clin_c2, na_like_strings)]
  clin_c2_cleaning_comparison <- dt[
    clin_c2 != clin_c2_orig,
    .(clin_c2_orig, clin_c2)
  ]
  if (to_view_checks) {
    print(head(clin_c2_cleaning_comparison)) # Check: Print head of changes
  }
  dt[, clin_c2_orig := NULL]

  dt[, clin_c1 := remove_lumped_icd_codes(dt$clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(dt$clin_c2)]

  dt[, clin_c1 := split_to_vector(clin_c1)]
  clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c1
  )
  dt[, clin_icd := clin_c1_result$clin_icd]
  dt[, clin_c1 := clin_c1_result$col_first]

  dt[, clin_c2 := split_to_vector(clin_c2)]
  clin_c2_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c2)
  dt[, clin_icd := clin_c2_result$clin_icd]
  dt[, clin_c2 := clin_c2_result$col_first]

  clin_c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c1, rvs_icd9
  )
  dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
  dt[, clin_c1 := clin_c1_rvs_results$col]

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]

  dt[, clin_rvs := lapply(clin_rvs, unique)]
  dedup_result <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]

  # Replace empty strings in character and factor columns with NA
  dt <- replace_empty_with_na(dt, to_view_checks)

  warning_thrown <- FALSE

  # Remap and check for patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Patient Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Patient Types:")
    print(unique(dt$pat_type))
  }

  warning_thrown <- FALSE

  # Remap and check for member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Memcat Parent Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Memcat Parent Types:")
    print(unique(dt$pat_memcat_parent))
  }

  warning_thrown <- FALSE

  # Remap and check for member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Memcat Child Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Memcat Child Types:")
    print(unique(dt$pat_memcat_child))
  }

  warning_thrown <- FALSE

  # Remap and check for clinical discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Discharge Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Discharge Types:")
    print(unique(dt$clin_discharge))
  }

  return(dt)
}

process_chunk <- function(chunk) {
  # Suppress output
  if (to_view_checks) {
    print("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  # Clean data
  chunk <- clean_data(chunk)

  # Map RVS codes
  chunk[, icd9_list := map_rvs_icd9(clin_rvs, rvs_icd9)]

  # Map ICD codes
  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  mapped_columns <- implement_icd10_mapping(
    clin_c1,
    clin_c2,
    clin_icd,
    tdrg_icd10,
    rows_to_show = 10
  )

  # Save the results back to the data.table
  chunk[, clin_c1 := mapped_columns$clin_c1]
  chunk[, clin_c2 := mapped_columns$clin_c2]
  chunk[, clin_icd := mapped_columns$clin_icd]

  # Replace empty strings with NA values
  chunk <- replace_empty_with_na(chunk, to_view_checks)

  # Find PDX
  # chunk <- apply_find_pdx(chunk)
  pdx_result <- apply_find_pdx(chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx)
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  return(chunk)
}

find_pdx_from_icd <- function(clin_icd) {
  pdxs <- intersect(clin_icd, acc_pdx)
  result <- if (length(pdxs) == 0) { # Check if no acceptable PDX codes
    # are found
    list(pdx = NA_character_, pdx_code = 99)
  } else if (length(pdxs) == 1) { # Check if exactly one acceptable PDX
    # code is found
    list(pdx = pdxs[1], pdx_code = 3)
  } else {
    list(pdx = sample(pdxs, 1), pdx_code = 6) # If multiple acceptable
    # PDX codes are found, return a random one
  }
  return(result)
}

find_most_similar_pdx <- function(code, pdxs) {
  starting_letter <- substr(code, 1, 1)
  starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

  result <- if (length(starting_codes) == 1) { # Check if exactly one
    # PDX code starts with the same letter
    list(pdx = starting_codes[1], pdx_code = 4)
  } else if (length(starting_codes) > 1) { # Check if multiple PDX
    # codes start with the same letter
    similarities <- sapply(starting_codes, function(candidate) {
      sum(
        substr(
          code, 1, nchar(candidate)
        ) == substr(
          candidate,
          1,
          nchar(candidate)
        )
      )
    })
    most_similar_pdx <- starting_codes[which.max(similarities)]
    list(pdx = most_similar_pdx, pdx_code = 5)
  } else {
    list(pdx = NA_character_, pdx_code = NA_integer_)
  }

  return(result)
}

find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  clin_icd <- unlist(clin_icd)

  # Helper function to check if a clinical code is an acceptable PDX
  assess_pdx_code <- function(code, code_num, acc_pdx) {
    if (!is.null(code) && code %in% acc_pdx) {
      return(list(pdx = code, pdx_code = code_num))
    } else {
      return(list(pdx = NA_character_, pdx_code = NA_integer_))
    }
  }

  # Check if clin_c1 or clin_c2 is an acceptable PDX
  pdx_check <- assess_pdx_code(clin_c1, 1, acc_pdx)
  if (!is.na(pdx_check$pdx)) {
    return(pdx_check)
  }

  pdx_check <- assess_pdx_code(clin_c2, 2, acc_pdx)
  if (!is.na(pdx_check$pdx)) {
    return(pdx_check)
  }

  # Find PDX from clinical ICD codes
  pdx_result <- find_pdx_from_icd(clin_icd)
  if (!is.na(pdx_result$pdx)) {
    return(pdx_result)
  }

  # Find the most similar PDX based on clin_c1 or clin_c2
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {
      most_similar_pdx <- find_most_similar_pdx(cr, pdx_result$pdx)
      if (!is.na(most_similar_pdx$pdx)) {
        return(most_similar_pdx)
      }
    }
  }

  # If no specific match, return the result from find_pdx_from_icd
  return(pdx_result)
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  n <- length(clin_c1)
  pdx <- character(n)
  pdx_code <- integer(n)

  # Assign PDX based on clin_c1 and clin_c2
  pdx[clin_c1 %in% acc_pdx] <- clin_c1[clin_c1 %in% acc_pdx]
  pdx_code[clin_c1 %in% acc_pdx] <- 1

  pdx[clin_c2 %in% acc_pdx] <- clin_c2[clin_c2 %in% acc_pdx]
  pdx_code[clin_c2 %in% acc_pdx] <- 2

  # Identify rows without a PDX
  missing_pdx_indices <- which(is.na(pdx) | pdx == "")

  if (length(missing_pdx_indices) > 0) {
    # Check if there are rows without a PDX
    for (i in missing_pdx_indices) {
      result <- find_pdx(clin_c1[i], clin_c2[i], clin_icd[[i]], acc_pdx)
      if (!is.na(result$pdx) && !(result$pdx %in% acc_pdx)) {
        stop(sprintf("Invalid PDX code found: %s", result$pdx))
      }
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }
  }

  return(list(pdx = pdx, pdx_code = pdx_code))
}

split_rvs_codes <- function(rvs_icd9) {
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

create_rvs_map_lists <- function(with_drg) {
  with_drg <- with_drg[order(rvs, -is_drg)]
  unique_rvs <- unique(with_drg$rvs)
  rvs_grouped <- split(with_drg, with_drg$rvs)

  rvs_map_list <- list()
  rvs_map_solo <- list()

  for (r in unique_rvs) {
    sub <- rvs_grouped[[r]]
    if (nrow(sub) == 1) {
      rvs_map_solo[[r]] <- sub$icd9cm[1]
    } else {
      rvs_map_list[[r]] <- sub$icd9cm
    }
  }

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

print_summary_statistics <- function(rvss, rvs_icd9, rvs_map_list) {
  without_drg <- rvs_icd9[!rvs %in% names(rvs_map_list)]
  cat(sprintf(
    "There are %d",
    length(unique(without_drg$rvs))
  ), "RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n")

  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf(
    "Of these, %d (%.2f%%)",
    length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)
  ), "have a mapping to an ICD-9-CM code.\n")

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      length(multi_mapped_rvs),
      length(multi_mapped_rvs) * 100 / length(mappable_rvs)
    ),
    "with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n"
  )

  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf(
    "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n",
    length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)
  ))
}

get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[
      !is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))
    ]
    if (length(mappable) > 0) {
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })
}

map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvss <- unique(unlist(clin_rvs))
  rvss <- intersect(rvss, rvs_icd9$rvs)

  print_summary_statistics(rvss, rvs_icd9, rvs_maps$rvs_map_list)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

  return(icd9_list)
}

find_and_append_valid_rvs <- function(dt, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  dt[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  dt[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]
  dt[, clin_rvs := lapply(
    seq_along(clin_rvs),
    function(i) unique(c(clin_rvs[[i]], dt$valid_matches[[i]]))
  )]
}

remove_5_digit_codes <- function(col) {
  regex_5_digit <- "\\b\\d{5}\\b"
  lapply(col, function(x) gsub(regex_5_digit, "", x))
}

warn_invalid_rvs <- function(dt, valid_rvs_codes) {
  dt[, invalid_matches := lapply(
    matches, function(x) x[!x %in% valid_rvs_codes]
  )]
  discarded_codes <- unlist(dt$invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    print(kable(discarded_table, col.names = c("CODE", "Counts"), format = "markdown"))
  } else {
    print("No RVS codes discarded")
  }
}

append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  dt <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(dt, valid_rvs_codes)
  dt[, col := remove_5_digit_codes(col)]
  warn_invalid_rvs(dt, valid_rvs_codes)

  return(list(clin_rvs = dt$clin_rvs, col = dt$col))
}
