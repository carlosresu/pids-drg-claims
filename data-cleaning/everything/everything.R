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
# # source(here("data-cleaning", "r_scripts", "libraries.R"))

na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

# Define column types
character_cols_before_drop <- c(
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

# Define column classes
col_classes_before_drop <- c(
  rep("character", length(character_cols_before_drop)),
  rep("integer", length(integer_cols)),
  rep("factor", length(factor_cols)),
  rep("numeric", length(numeric_cols))
)

names(col_classes_before_drop) <- c(
  character_cols_before_drop, integer_cols,
  factor_cols, numeric_cols
)

# Define column types
character_cols_after_drop <- c(
  "PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "HCI_PMCC_NO", "HCP_NO_LIST",
  "PRIMARY_ILLNESS", "SECONDARY_ILLNESS", paste0("ICDCODE", c(1:12)),
  paste0("RVSCODE", 1:20), "DATE_ADM", "TIME_ADM",
  "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT",
  "PAT_BDAY", "EXTRACTION_DATE"
)

# Define column classes
col_classes_after_drop <- c(
  rep("character", length(character_cols_after_drop)),
  rep("integer", length(integer_cols)),
  rep("factor", length(factor_cols)),
  rep("numeric", length(numeric_cols))
)

names(col_classes_after_drop) <- c(
  character_cols_after_drop, integer_cols,
  factor_cols, numeric_cols
)

covid_rvs <- c(
  "C19T1", "C19T2", "C19T3", "C19X1", "C19X2", "C19X3", "C19FRP",
  "C19IP1", "C19IP2", "C19IP3", "C19IP4", "C19PP1", "C19PP2",
  "C19PP3", "C19PP4", "MP01", "IMP02", "C19CI", "C19H1", "C19VIH",
  "C19VID"
)

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
# source(here("data-cleaning", "r_scripts", "libraries.R"))

path_to_intermediate <- "git-ignored-files/intermediate-claims"
path_to_cache <- "data-cleaning/cache"
path_to_aux <- "git-ignored-files/aux-files"
path_to_excel <- "git-ignored-files/Excel"
path_to_cleaned_claims <- "git-ignored-files/cleaned-claims"
path_to_grouper_output <- "git-ignored-files/grouper-output"
path_to_chunks <- "git-ignored-files/chunked-samples"
path_to_raw_claims_parts <- "git-ignored-files/raw-claims/parts"
path_to_raw_claims_samples <- "git-ignored-files/raw-claims/samples"
path_to_raw_claims <- "git-ignored-files/raw-claims"

# Here() let's you find files in your project directory
suffix <- paste0(ifelse(to_sample, "_sampled_", "_full_"), version)

sampled_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("sampled_claims_extract_CLAIMS_", year_to_load, suffix, "_", sample_size)
  } else {
    paste0("sampled_claims_extract_CLAIMS_", year_to_load, suffix, "_", sample_size, "_part_", part, "_of_", split_chunks)
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_raw_claims_samples, filename))
}

full_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("claims_extract_CLAIMS_", year_to_load)
  } else {
    paste0("claims_extract_CLAIMS_", year_to_load, "_part_", part, "_of_", split_chunks)
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  if (is.null(part)) {
    return(here(path_to_raw_claims, filename))
  } else {
    return(here(path_to_raw_claims_parts, filename))
  }
}

intermediate_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("intermediate_claims_", year_to_load, "_processed", suffix)
  } else {
    paste0(
      "intermediate_claims_", year_to_load,
      "_processed", suffix, "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_intermediate, filename))
}

cleaned_claims_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("cleaned_claims_extract_CLAIMS_", year_to_load, suffix)
  } else {
    paste0(
      "cleaned_claims_extract_CLAIMS_",
      year_to_load, suffix, "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(path_to_cleaned_claims, filename))
}

output_txt_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("DRG_Grouped", "_", year_to_load, suffix)
  } else {
    paste0(
      "DRG_Grouped", "_", year_to_load, suffix,
      "_part_", part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".txt")
  }
  return(here(path_to_grouper_output, filename))
}

grouper_result_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    toupper(paste0("DRG_Grouped", "_", year_to_load, suffix, "Res"))
  } else {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load,
      suffix, "Res_", part, "_of_", split_chunks
    ))
  }
  if (fileext) {
    filename <- paste0(filename, ".TXT")
  }
  return(here(path_to_grouper_output, filename))
}

total_rows_file <- function(part = NULL, fileext = TRUE) {
  filename <- if (is.null(part)) {
    paste0("total_rows_", year_to_load)
  } else {
    paste0(
      "total_rows_", year_to_load, "_part_",
      part, "_of_", split_chunks
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".rds")
  }
  return(here(path_to_cache, filename))
}
# # source(here("data-cleaning", "r_scripts", "libraries.R"))

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

  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

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
      set(dt, j = col_name, value = factor(
        dt[[col_name]],
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
    # print("Replacement Summary:")
    # print(replacement_summary)
  }

  return(list(data = dt, replacement_summary = replacement_summary))
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

combine_comparison_tables <- function(
    summaries, comparison_field, rows_to_show = 10) {
  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code, count)]
    }
    return(summary_data)
  })

  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(), count = integer()
    ))
  }

  combined_comparison <- combined_comparison[,
    .(count = sum(count, na.rm = TRUE)),
    by = .(old_code, new_code)
  ]
  combined_comparison <- combined_comparison[order(-count)]
  combined_comparison <- head(combined_comparison, rows_to_show)

  return(combined_comparison)
}



combine_discarded_rvs_tables <- function(summaries, field, rows_to_show = 10) {
  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, rows_to_show)

  return(combined_discarded)
}

combine_replace_empty_tables <- function(summaries, field, rows_to_show = 10) {
  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]
  combined_replace_empty <- combined_replace_empty[
    order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)
  ]
  combined_replace_empty <- head(combined_replace_empty, rows_to_show)

  return(combined_replace_empty)
}

# Function to combine and sum the counts for comparison table
combine_icd_comparison_table <- function(tables) {
  combined_table <- rbindlist(tables, fill = TRUE)
  # print(colnames(combined_table))
  combined_table <- combined_table[,
    .(count = sum(count, na.rm = TRUE)),
    by = .(old_code, new_code)
  ]
  combined_table <- combined_table[order(-count)]
  return(combined_table)
}

# Function to combine and sum the counts for invalid ICD codes table
combine_invalid_icd_table <- function(tables) {
  combined_table <- rbindlist(tables)
  combined_table <- combined_table[, .(count = sum(count)), by = code]
  combined_table[order(-count)]
}

combine_all_parts_summaries <- function(
    all_parts_summaries, rows_to_show = 10) {
  combined_summary <- list(
    rename_success = all(
      sapply(
        all_parts_summaries,
        function(summary) summary$rename_success
      )
    ),
    ICD_replacements_1 = combine_comparison_tables(
      all_parts_summaries, "ICD_replacements_1", rows_to_show
    ),
    ICD_replacements_2 = combine_comparison_tables(
      all_parts_summaries, "ICD_replacements_2", rows_to_show
    ),
    pat_type_unmapped = unique(
      unlist(lapply(
        all_parts_summaries,
        function(summary) summary$pat_type_unmapped
      ))
    ),
    memcat_parent_unmapped = unique(
      unlist(lapply(
        all_parts_summaries,
        function(summary) summary$memcat_parent_unmapped
      ))
    ),
    memcat_child_unmapped = unique(
      unlist(lapply(
        all_parts_summaries,
        function(summary) summary$memcat_child_unmapped
      ))
    ),
    discharge_unmapped = unique(
      unlist(lapply(
        all_parts_summaries,
        function(summary) summary$discharge_unmapped
      ))
    ),
    discard_rvs_one = combine_discarded_rvs_tables(
      all_parts_summaries, "discard_rvs_one", rows_to_show
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      all_parts_summaries, "discard_rvs_two", rows_to_show
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      all_parts_summaries, "empty_strings_replaced_1", rows_to_show
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      all_parts_summaries, "empty_strings_replaced_2", rows_to_show
    ),
    icd_comparison_table = combine_icd_comparison_table(
      lapply(
        all_parts_summaries,
        function(summary) summary$icd_comparison_table
      )
    ),
    invalid_icds_table = combine_invalid_icd_table(
      lapply(
        all_parts_summaries,
        function(summary) summary$invalid_icds_table
      )
    )
  )

  return(combined_summary)
}

combine_all_parts_statistics <- function(all_parts_statistics) {
  total_statistics <- list(
    total_rvs_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$total_rvs_count
    )),
    without_drg_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$without_drg_count
    )),
    mappable_rvs_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$mappable_rvs_count
    )),
    mappable_rvs_percentage = sum(sapply(
      all_parts_statistics,
      function(stat) stat$mappable_rvs_count
    )) /
      sum(sapply(
        all_parts_statistics,
        function(stat) stat$total_rvs_count
      )) * 100,
    multi_mapped_rvs_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$multi_mapped_rvs_count
    )),
    multi_mapped_rvs_percentage = sum(sapply(
      all_parts_statistics,
      function(stat) stat$multi_mapped_rvs_count
    )) /
      sum(sapply(
        all_parts_statistics,
        function(stat) stat$mappable_rvs_count
      )) * 100,
    unmappable_rvs_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$unmappable_rvs_count
    )),
    unmappable_rvs_percentage = sum(sapply(
      all_parts_statistics,
      function(stat) stat$unmappable_rvs_count
    )) /
      sum(sapply(
        all_parts_statistics,
        function(stat) stat$total_rvs_count
      )) * 100,
    total_unique_icd_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$total_unique_icd_count
    )),
    direct_match_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$direct_match_count
    )),
    direct_match_percentage = sum(sapply(
      all_parts_statistics,
      function(stat) stat$direct_match_count
    )) /
      sum(sapply(
        all_parts_statistics,
        function(stat) stat$total_unique_icd_count
      )) * 100,
    total_mapped_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$total_mapped_count
    )),
    modified_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$modified_count
    )),
    unmapped_icd_count = sum(sapply(
      all_parts_statistics,
      function(stat) stat$unmapped_icd_count
    ))
  )

  combined_unmapped_icds <- rbindlist(lapply(
    all_parts_statistics,
    function(stat) stat$combined_unmapped_icds
  ))
  total_statistics$combined_unmapped_icds <- combined_unmapped_icds[,
    .(count = sum(count)),
    by = code
  ][order(-count)]

  return(total_statistics)
}

# Helper function to determine if a file is a partial file
is_partial_file <- function(part) {
  if (is.na(split_chunk_to_process)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Function to check if a file exists
file_exists <- function(filepath) {
  return(file.exists(filepath))
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

clean_data <- function(dt) {
  # Add year column
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if all columns were successfully renamed
  if (!all(new_colnames %in% colnames(dt))) {
    missing_cols <- setdiff(new_colnames, colnames(dt))
    warning(
      "Failed to rename the following columns: ",
      paste(missing_cols, collapse = ", ")
    )
    rename_success <- FALSE
    stop("Column renaming failed.")
  }

  rename_success <- TRUE

  # Collapse columns clin_icd1 to clin_icd12 into clin_icd
  dt[, clin_icd := collapse_columns(
    mget(paste0("clin_icd", 1:12), envir = as.environment(dt)),
    na_like_strings
  )]
  dt[, paste0("clin_icd", 1:12) := NULL]

  # Collapse columns clin_rvs1 to clin_rvs20 into clin_rvs
  dt[, clin_rvs := collapse_columns(
    mget(paste0("clin_rvs", 1:20), envir = as.environment(dt)),
    na_like_strings
  )]
  dt[, paste0("clin_rvs", 1:20) := NULL]

  # Remove lumped ICD codes from clin_icd
  dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]

  # Turn clin_icd and clin_rvs into lists
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]

  # Ensure clean_column function and na_like_
  # strings are correctly defined and applied
  dt[, clin_c1_orig := dt$clin_c1]
  # Ensure clin_c1_orig captures original values
  dt[, clin_c1 := clean_column(clin_c1, na_like_strings)] # Clean clin_c1

  # Generate cleaning comparison table
  clin_c1_cleaning_comparison <- dt[
    clin_c1 != clin_c1_orig,
    .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
    by = .(clin_c1_orig, clin_c1)
  ]

  # Optionally remove clin_c1_orig from dt if no longer needed
  dt[, clin_c1_orig := NULL]

  dt[, clin_c2_orig := dt$clin_c2] # Capture original clin_c2

  # Clean clin_c2 within the data.table context
  dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]

  # Create cleaning comparison table
  clin_c2_cleaning_comparison <- dt[
    clin_c2 != clin_c2_orig, # Compare cleaned clin_c2 with original
    .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
    by = .(clin_c2_orig, clin_c2)
  ]

  # Optionally remove clin_c2_orig from dt if no longer needed
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
  clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]
  clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

  dt[, clin_rvs := lapply(clin_rvs, unique)]
  dedup_result <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]

  # Replace empty strings in character and factor columns with NA
  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  empty_strings_replaced_1 <- replace_result$replacement_summary

  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL

  warning_thrown <- FALSE

  # Remap and check for patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    pat_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    parent_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    child_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for clinical discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    discharge_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    discard_rvs_one = clin_c1_discarded_rvs,
    discard_rvs_two = clin_c2_discarded_rvs,
    empty_strings_replaced_1 = empty_strings_replaced_1
  ))
}

process_chunk <- function(
    chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  # Suppress output
  if (to_view_checks) {
    # print("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  # Clean data
  clean_result <- clean_data(chunk)
  chunk <- clean_result$data

  # Store chunk summaries
  summary <- list()
  summary$rename_success <- clean_result$rename_success
  summary$ICD_replacements_1 <- clean_result$ICD_replacements_1
  summary$ICD_replacements_2 <- clean_result$ICD_replacements_2
  summary$pat_type_unmapped <- clean_result$pat_type_unmapped
  summary$memcat_parent_unmapped <- clean_result$memcat_parent_unmapped
  summary$memcat_child_unmapped <- clean_result$memcat_child_unmapped
  summary$discharge_unmapped <- clean_result$discharge_unmapped
  summary$discard_rvs_one <- clean_result$discard_rvs_one
  summary$discard_rvs_two <- clean_result$discard_rvs_two
  summary$empty_strings_replaced_1 <- clean_result$empty_strings_replaced_1

  # Map RVS codes and collect summary statistics
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]
  summary$rvs_mapping_summary <- rvs_mapping_result$summary_statistics

  # Map ICD codes
  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  mapped_columns <- implement_icd10_mapping(
    clin_c1,
    clin_c2,
    clin_icd,
    tdrg_icd10,
    rows_to_show = Inf
  )

  # Save the results back to the data.table
  chunk[, clin_c1 := mapped_columns$clin_c1]
  chunk[, clin_c2 := mapped_columns$clin_c2]
  chunk[, clin_icd := mapped_columns$clin_icd]

  # Replace empty strings with NA values again after mapping
  chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
  chunk <- chunk_replace_result$data
  summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

  # Find PDX
  pdx_result <- apply_find_pdx(
    chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Collect ICD-10 mapping statistics
  summary$total_unique_icd_count <- mapped_columns$var1
  summary$direct_match_count <- mapped_columns$var2
  summary$modified_count <- mapped_columns$var5
  summary$total_mapped_count <- mapped_columns$var4
  summary$unmapped_icd_count <- mapped_columns$var6
  summary$unmapped_icds <- mapped_columns$var7
  summary$comparison_table <- mapped_columns$comparison_table
  summary$invalid_icds_table <- mapped_columns$invalid_icds_table

  return(list(chunk = chunk, summary = summary))
}


parallelize_and_summarize <- function(
    dt, num_cores, to_view_checks, global_seed,
    rows_to_show, rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize) {
  chunk_size <- ceiling(nrow(dt) / num_cores)
  chunks <- split(dt, rep(1:num_cores,
    each = chunk_size, length.out = nrow(dt)
  ))

  if (to_parallelize) {
    # Plan for parallel processing
    plan(multisession, workers = num_cores)

    # Process each chunk in parallel
    parallel_results <- future_lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx,
      future.seed = global_seed
    )
  } else {
    # Process each chunk sequentially
    parallel_results <- lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx
    )
  }

  # Combine processed chunks
  processed_chunks <- lapply(parallel_results, function(res) res$chunk)
  dt <- rbindlist(processed_chunks)

  # Combine summaries
  summaries <- lapply(parallel_results, function(res) res$summary)

  consolidated_summary <- list(
    rename_success = all(sapply(summaries, function(s) s$rename_success)),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1", rows_to_show
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", rows_to_show
    ),
    pat_type_unmapped = unique(
      unlist(lapply(summaries, function(s) s$pat_type_unmapped))
    ),
    memcat_parent_unmapped = unique(
      unlist(lapply(summaries, function(s) s$memcat_parent_unmapped))
    ),
    memcat_child_unmapped = unique(
      unlist(lapply(summaries, function(s) s$memcat_child_unmapped))
    ),
    discharge_unmapped = unique(
      unlist(lapply(summaries, function(s) s$discharge_unmapped))
    ),
    discard_rvs_one = combine_discarded_rvs_tables(
      summaries, "discard_rvs_one", rows_to_show
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two", rows_to_show
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1",
      rows_to_show = Inf
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2",
      rows_to_show = Inf
    ),
    icd_comparison_table = combine_icd_comparison_table(
      lapply(summaries, function(s) s$comparison_table)
    ),
    invalid_icds_table = combine_invalid_icd_table(
      lapply(summaries, function(s) s$invalid_icds_table)
    )
  )

  # Aggregate summary statistics
  rvss <- unique(unlist(dt$clin_rvs))
  total_rvs_count <- length(rvss)
  rvs_map_list <- create_rvs_map_lists(
    split_rvs_codes(rvs_icd9)$with_drg
  )$rvs_map_list

  without_drg_count <- length(
    unique(rvs_icd9[!rvs %in% names(rvs_map_list)]$rvs)
  )
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  mappable_rvs_count <- length(mappable_rvs)
  mappable_rvs_percentage <- (mappable_rvs_count / length(rvss)) * 100

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  multi_mapped_rvs_count <- length(multi_mapped_rvs)
  multi_mapped_rvs_percentage <- (
    multi_mapped_rvs_count / mappable_rvs_count) * 100

  unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
  unmappable_rvs_count <- length(unmappable_rvs)
  unmappable_rvs_percentage <- (unmappable_rvs_count / length(rvss)) * 100

  aggregate_statistics <- list(
    total_rvs_count = total_rvs_count,
    without_drg_count = without_drg_count,
    mappable_rvs_count = mappable_rvs_count,
    mappable_rvs_percentage = mappable_rvs_percentage,
    multi_mapped_rvs_count = multi_mapped_rvs_count,
    multi_mapped_rvs_percentage = multi_mapped_rvs_percentage,
    unmappable_rvs_count = unmappable_rvs_count,
    unmappable_rvs_percentage = unmappable_rvs_percentage
  )

  # Combine ICD-10 mapping statistics
  icd10_stats <- aggregate_icd10_stats(summaries)
  aggregate_statistics <- c(aggregate_statistics, icd10_stats)

  return(
    list(
      dt = dt,
      consolidated_summary = consolidated_summary,
      aggregate_statistics = aggregate_statistics
    )
  )
}

print_aggregate_summary_stats <- function(aggregate_statistics, rows_to_show) {
  cat(
    sprintf(
      "There are %d RVS codes without an ICD-9CM",
      aggregate_statistics$without_drg_count
    ), "equivalent recognized by the TDRG ICD9CM\n"
  )
  cat(
    sprintf(
      "There are %d unique RVS codes that appear in the claims.\n",
      aggregate_statistics$total_rvs_count
    )
  )
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      aggregate_statistics$mappable_rvs_count,
      aggregate_statistics$mappable_rvs_percentage
    )
  )
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      aggregate_statistics$multi_mapped_rvs_count,
      aggregate_statistics$multi_mapped_rvs_percentage
    ), "with more than one ICD9 equivalent",
    "recognized by the Thai ICD9 library.\n"
  )
  cat(
    sprintf(
      "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n\n\n",
      aggregate_statistics$unmappable_rvs_count,
      aggregate_statistics$unmappable_rvs_percentage
    )
  )
  cat(
    sprintf(
      "There are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      aggregate_statistics$total_unique_icd_count,
      aggregate_statistics$direct_match_count,
      aggregate_statistics$direct_match_percentage
    ), "are directly in the Thai ICD-10 library\n"
  )
  cat(
    sprintf(
      "The modifications led to a total of %d codes ",
      aggregate_statistics$total_mapped_count
    ), "being mapped to an equivalent in the Thai ICD10 library.\n"
  )
  cat(
    sprintf(
      "Out of these, %d were modified to match.\n",
      aggregate_statistics$modified_count
    )
  )
  cat(
    sprintf(
      "There are %d codes that could not",
      aggregate_statistics$unmapped_icd_count
    ), "be mapped to the Thai ICD10 library:\n"
  )
  # print(head(aggregate_statistics$combined_unmapped_icds, rows_to_show))
}

print_summary_tables <- function(result, rows_to_show) {
  dt <- result$dt
  consolidated_summary <- result$consolidated_summary
  aggregate_statistics <- result$aggregate_statistics

  # Print the consolidated summary
  cat("Rename Success:\n", consolidated_summary$rename_success, "\n\n")

  if (nrow(consolidated_summary$ICD_replacements_1) > 0) {
    print(kable(head(consolidated_summary$ICD_replacements_1, rows_to_show),
      format = "markdown",
      caption = "ICD Replacements 1"
    ))
  } else {
    cat("\nNo ICD replacements found in the first set.\n\n")
  }

  if (nrow(consolidated_summary$ICD_replacements_2) > 0) {
    print(kable(head(consolidated_summary$ICD_replacements_2, rows_to_show),
      format = "markdown",
      caption = "ICD Replacements 2"
    ))
  } else {
    cat("\nNo ICD replacements found in the second set.\n\n")
  }

  if (is.null(consolidated_summary$pat_type_unmapped)) {
    cat("Patient Type Unmapped: NULL\n\n")
  } else {
    cat(
      "Patient Type Unmapped:\n",
      consolidated_summary$pat_type_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$memcat_parent_unmapped)) {
    cat("Memcat Parent Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Parent Unmapped:\n",
      consolidated_summary$memcat_parent_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$memcat_child_unmapped)) {
    cat("Memcat Child Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Child Unmapped:\n",
      consolidated_summary$memcat_child_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$discharge_unmapped)) {
    cat("Discharge Unmapped: NULL\n\n")
  } else {
    cat(
      "Discharge Unmapped:\n",
      consolidated_summary$discharge_unmapped, "\n\n"
    )
  }

  if (nrow(consolidated_summary$discard_rvs_one) > 0) {
    print(kable(head(consolidated_summary$discard_rvs_one, rows_to_show),
      format = "markdown",
      caption = "Discarded RVS Codes One"
    ))
  } else {
    cat("\nNo RVS codes discarded in the first set.\n\n")
  }

  if (nrow(consolidated_summary$discard_rvs_two) > 0) {
    print(kable(head(consolidated_summary$discard_rvs_two, rows_to_show),
      format = "markdown",
      caption = "Discarded RVS Codes Two"
    ))
  } else {
    cat("\nNo RVS codes discarded in the second set.\n\n")
  }

  if (nrow(consolidated_summary$empty_strings_replaced_1) > 0) {
    print(kable(
      head(
        consolidated_summary$empty_strings_replaced_1, rows_to_show
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(consolidated_summary$empty_strings_replaced_2) > 0) {
    print(kable(
      head(
        consolidated_summary$empty_strings_replaced_2, rows_to_show
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }

  # Print aggregate summary statistics
  print_aggregate_summary_stats(aggregate_statistics, rows_to_show)

  # Print comparison table and invalid
  # ICDs table within the consolidated summary
  if (nrow(consolidated_summary$icd_comparison_table) > 0) {
    print(kable(head(consolidated_summary$icd_comparison_table, rows_to_show),
      format = "markdown",
      caption = "Comparison of ICD Codes Before and After Mapping"
    ))
  } else {
    cat("\nNo ICD codes changed during mapping.\n\n")
  }

  if (nrow(consolidated_summary$invalid_icds_table) > 0) {
    print(kable(head(consolidated_summary$invalid_icds_table, rows_to_show),
      format = "markdown",
      caption = "Invalid ICD Codes Not Found in Thai or PhilHealth Libraries"
    ))
  } else {
    cat("\nAll resulting ICD codes are valid and present in the libraries.\n\n")
  }
}

split_and_save_chunks <- function() {
  if (to_split && !is_partial_file(part)) {
    rows_per_part <- ceiling(total_rows / split_chunks)
    for (part in 1:split_chunks) {
      chunk_file <- if (to_sample) {
        sampled_claims_file(part)
      } else {
        full_claims_file(part)
      }

      if (!file.exists(chunk_file)) {
        start_row <- (part - 1) * rows_per_part + 1
        end_row <- min(part * rows_per_part, total_rows)
        read_and_save_partial(start_row, end_row, part)
      } else {
        print(paste("File already exists:", chunk_file))
      }
    }
  }
}


read_and_process_chunk <- function(part) {
  # Determine the correct chunk file based on sampling condition
  if (to_sample) {
    # Ensure the correct, existing sampled file is used
    chunk_file <- sampled_claims_file(part)
  } else {
    chunk_file <- full_claims_file(part)
  }

  # Check if the chunk file exists
  if (!file_exists(chunk_file)) {
    stop(paste("File does not exist:", chunk_file))
  }

  # Read the chunk file into a data table
  dt <- fread(chunk_file, na.strings = na_values, colClasses = "character")

  # Check if dt is empty and provide informative messages
  if (is.null(dt) || nrow(dt) == 0) {
    stop(paste("Data table is empty for part:", part, "file:", chunk_file))
  }

  return(dt)
}

parallelize_and_summarize_data <- function(part, dt) {
  if (to_chunk) {
    num_cores <- max(1, availableCores() - 1)
    suppress_interim_output({
      result <- parallelize_and_summarize(
        dt, num_cores,
        to_view_checks = to_view_checks_parallelized, global_seed,
        rows_to_show, rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize
      )
    })

    dt <- result$dt
    all_parts_summaries[[part]] <<- result$consolidated_summary
    all_parts_statistics[[part]] <<- result$aggregate_statistics
  } else {
    stop("Error: to_chunk must be TRUE; not chunking is deprecated.")
  }
  return(dt)
}

group_data <- function(part, dt) {
  if (to_group) {
    export_for_batch_grouper(dt, year_to_load, output_txt_file(part))
    for_batch_grouping <- fread(output_txt_file(part), sep = "|", na.strings = "--")
    if (file.exists(grouper_result_file(part))) {
      batch_grouping_result <- fread(grouper_result_file(part), sep = "|", na.strings = "--")
    }
  }
}

write_intermediate_file <- function(part, dt) {
  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE))
  }
}

combine_and_print_summaries <- function() {
  final_combined_summary <- combine_all_parts_summaries(
    all_parts_summaries, rows_to_show
  )
  final_combined_statistics <- combine_all_parts_statistics(
    all_parts_statistics
  )

  print_summary_tables(
    list(
      dt = NULL,
      consolidated_summary = final_combined_summary,
      aggregate_statistics = final_combined_statistics
    ),
    rows_to_show
  )
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

handle_sampling <- function(dt = NULL) {
  sampled_file <- sampled_claims_file(part)

  if (file.exists(sampled_file)) {
    if (to_view_checks) {
      print("Sampled file exists. Reading the sampled file...")
    }
    dt <- read_sampled_file(sampled_file)
    if (nrow(dt) != sample_size) {
      if (to_view_checks) {
        print(paste(
          "Sampled file does not match sample size. Expected:",
          sample_size, "Found:", nrow(dt), "Re-sampling..."
        ))
      }
      dt <- resample_data()
    } else if (to_view_checks) {
      print("Sampled file matches sample size.")
    }
  } else {
    if (to_view_checks) {
      print("Sampled file does not exist. Creating new sample...")
    }
    dt <- resample_data()
  }

  return(dt)
}


# Function to resample data with initial read logic
resample_data <- function() {
  dt <- read_entire_file(full_claims_file(part), initial_read = TRUE)
  dt <- sample_data(dt)
  if (to_write) {
    if (to_view_checks) {
      print(paste(
        "to_write is TRUE. Writing the new sample data to file:",
        sampled_claims_file(part)
      ))
    }
    fwrite(dt, sampled_claims_file(part))
  } else if (to_view_checks) {
    print("to_write is FALSE. Not writing the sample data to file.")
  }
  return(dt)
}

# Function to read the entire file or a specific chunk with initial reading logic
read_entire_file <- function(file, initial_read = TRUE) {
  header <- fread(file, nrows = 1)
  if (initial_read) {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      skip = 1
    )
    setnames(dt, names(header))
  } else {
    dt <- fread(file,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      skip = 1
    )
    setnames(dt, names(header))
    dt <- dt[, (drop_cols) := NULL]
    for (col in names(col_classes_after_drop)) {
      dt[[col]] <- switch(col_classes_after_drop[[col]],
        "character" = as.character(dt[[col]]),
        "factor" = as.factor(dt[[col]]),
        "integer" = as.integer(dt[[col]]),
        "numeric" = as.numeric(dt[[col]]),
        dt[[col]]
      )
    }
  }
  return(dt)
}

# Function to read a sampled file
read_sampled_file <- function(file) {
  header <- fread(file, nrows = 1)
  dt <- fread(file,
    na.strings = na_values,
    colClasses = "character",
    header = FALSE,
    skip = 1
  )
  setnames(dt, names(header))
  dt <- dt[, (drop_cols) := NULL]
  for (col in names(col_classes_after_drop)) {
    dt[[col]] <- switch(col_classes_after_drop[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = as.factor(dt[[col]]),
      "integer" = as.integer(dt[[col]]),
      "numeric" = as.numeric(dt[[col]]),
      dt[[col]]
    )
  }
  return(dt)
}

# Function to sample data
sample_data <- function(dt) {
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

main_read_function <- function(file = NA) {
  if (is.na(file)) {
    if (to_read) {
      file <- full_claims_file(part)
      if (to_view_checks) {
        print("Reading the entire file...")
        print(paste("Full claims file path:", file))
      }
      dt <- read_entire_file(file, initial_read = TRUE)

      if (to_sample) {
        dt <- handle_sampling(dt)
      }
    } else if (to_sample) {
      dt <- handle_sampling()
    } else {
      stop("Cannot proceed: to_read is FALSE and to_sample is FALSE. At least one must be TRUE.")
    }
  } else {
    dt <- read_entire_file(file, initial_read = FALSE)
  }

  return(dt)
}


read_and_save_partial <- function(start_row, end_row, part_num) {
  partial_file_path <- full_claims_file(part = part_num, fileext = TRUE)
  header <- fread(full_claims_file(), nrows = 1, header = TRUE) # Always read the header

  if (!file_exists(partial_file_path)) {
    skip_rows <- if (part_num == 1) start_row else start_row - 1
    dt <- fread(full_claims_file(),
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = skip_rows,
      header = FALSE
    )
    setnames(dt, colnames(header))
    if (nrow(dt) > 0) {
      print(paste("Saving partial file:", partial_file_path))
      fwrite(dt, partial_file_path, quote = TRUE)
    } else {
      print(paste("No rows to save for part:", part_num))
    }
  } else {
    print(paste("File already exists, skipping creation:", partial_file_path))
  }

  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part_num)
    if (!file_exists(sampled_file_path)) {
      print(paste("Creating sampled file:", sampled_file_path))
      if (exists("dt") && nrow(dt) > 0) {
        sampled_dt <- dt[sample(.N, min(sample_size, .N))]
      } else {
        # Read the partial file again if dt doesn't exist
        dt <- fread(partial_file_path, na.strings = na_values, colClasses = "character")
        sampled_dt <- dt[sample(.N, min(sample_size, .N))]
      }
      setnames(sampled_dt, colnames(header))
      fwrite(sampled_dt, sampled_file_path, quote = TRUE)
    } else {
      print(paste("Sampled file already exists, skipping creation:", sampled_file_path))
    }
  }
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

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
apply_icd10_mapping_to_columns <- function(
    clin_c1, clin_c2, clin_icd, icd10_env) {
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

implement_icd10_mapping <- function(
    clin_c1, clin_c2, clin_icd, tdrg_icd10, rows_to_show = Inf) {
  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      length(icds), length(direct_match_codes),
      length(direct_match_codes) * 100 / length(icds)
    ),
    " are directly in the Thai ICD-10 library\n"
  )

  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env
  )
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf(
    "The modifications led to a total of %d",
    length(icd_mapping)
  ), " codes being mapped to an equivalent in the Thai ICD10 library.\n")
  cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf(
      "There are %d codes that could not",
      length(unmatched_icds)
    ), "be mapped to the Thai ICD10 library:\n")
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
  fwrite(icd10_map, paste0(
    "cache/icd10_map_file_",
    year_to_load, ".csv"
  ))
  icd10_env <- list2env(setNames(
    as.list(icd10_map$tdrg_icd10),
    icd10_map$phl_icd10
  ))

  mapped_columns <- apply_icd10_mapping_to_columns(
    clin_c1, clin_c2, clin_icd, icd10_env
  )

  # Generate comparison table
  original_data <- list(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2, clin_icd = clin_icd
  )
  modified_data <- list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd
  )

  padded_data <- lapply(
    names(original_data),
    function(name) {
      pad_list_elements(
        original_data[[name]],
        modified_data[[name]]
      )
    }
  )

  comparison_table <- rbind(
    generate_comparison_table(padded_data[[1]][[1]], padded_data[[1]][[2]]),
    generate_comparison_table(padded_data[[2]][[1]], padded_data[[2]][[2]]),
    generate_comparison_table(padded_data[[3]][[1]], padded_data[[3]][[2]])
  )

  comparison_table <- comparison_table[order(-count)]

  # Check if all resulting ICD codes are in either the
  # Thai library or the PhilHealth library
  all_icds <- unique(c(
    unlist(mapped_columns$clin_c1),
    unlist(mapped_columns$clin_c2), unlist(mapped_columns$clin_icd)
  ))
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
            unlist(mapped_columns$clin_c1),
            unlist(mapped_columns$clin_c2),
            unlist(mapped_columns$clin_icd)
          ) == icd, na.rm = TRUE)
        }
      )
    )

    invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
    invalid_icds_table <- invalid_icds_table[order(-count)]
  } else {
    invalid_icds_table <- data.table()
  }

  return(list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd,
    var1 = length(icds),
    var2 = length(direct_match_codes),
    var4 = length(icd_mapping),
    var5 = modified_count,
    var6 = length(unmatched_icds),
    var7 = unmatched_sources,
    comparison_table = comparison_table,
    invalid_icds_table = invalid_icds_table
  ))
}



ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  # Convert lists to data.table for efficient processing
  dt <- data.table(clin_c1 = clin_c1, clin_c2 = clin_c2, clin_icd = clin_icd)

  # Deduplicate each column
  dt[, clin_c1 := lapply(clin_c1, unique)]
  dt[, clin_c2 := lapply(clin_c2, unique)]
  dt[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  dt[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  dt[, clin_c1 := Map(function(c1, c2) {
    setdiff(c1, c2)
  }, clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  dt[, clin_c2 := Map(function(c1, c2) {
    setdiff(c2, c1)
  }, clin_c1, clin_c2)]

  return(list(clin_c1 = dt$clin_c1, clin_c2 = dt$clin_c2, clin_icd = dt$clin_icd))
}

generate_comparison_table <- function(original, modified) {
  original_unlisted <- unlist(original, use.names = FALSE)
  modified_unlisted <- unlist(modified, use.names = FALSE)

  comparison <- data.table(
    old_code = original_unlisted,
    new_code = modified_unlisted
  )

  comparison <- comparison[old_code != new_code,
    .(count = .N),
    by = .(old_code, new_code)
  ]

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

aggregate_icd10_stats <- function(summaries) {
  # Check the structure of summaries
  if (length(summaries) == 0) {
    stop("The summaries list is empty.")
  }
  if (!all(sapply(summaries, is.list))) {
    stop("All elements in summaries should be lists.")
  }

  required_fields <- c(
    "total_unique_icd_count", "direct_match_count",
    "modified_count", "total_mapped_count",
    "unmapped_icd_count", "unmapped_icds"
  )
  for (i in seq_along(summaries)) {
    summary <- summaries[[i]]
    missing_fields <- setdiff(required_fields, names(summary))
    if (length(missing_fields) > 0) {
      stop(sprintf(
        "Summary %d is missing fields: %s", i,
        paste(missing_fields, collapse = ", ")
      ))
    }
  }

  total_unique_icd_count <- sum(sapply(
    summaries,
    function(res) res$total_unique_icd_count
  ))
  direct_match_count <- sum(sapply(
    summaries,
    function(res) res$direct_match_count
  ))
  modified_count <- sum(sapply(
    summaries,
    function(res) res$modified_count
  ))
  total_mapped_count <- sum(sapply(
    summaries,
    function(res) res$total_mapped_count
  ))
  unmapped_icd_count <- sum(sapply(
    summaries,
    function(res) res$unmapped_icd_count
  ))

  unmapped_icds_list <- lapply(
    summaries,
    function(res) res$unmapped_icds
  )
  combined_unmapped_icds <- rbindlist(unmapped_icds_list, fill = TRUE)
  combined_unmapped_icds <- combined_unmapped_icds[,
    .(count = sum(count)),
    by = code
  ][order(-count)]

  return(list(
    total_unique_icd_count = total_unique_icd_count,
    direct_match_count = direct_match_count,
    direct_match_percentage = ifelse(total_unique_icd_count > 0,
      (direct_match_count / total_unique_icd_count) * 100, 0
    ),
    total_mapped_count = total_mapped_count,
    modified_count = modified_count,
    unmapped_icd_count = unmapped_icd_count,
    combined_unmapped_icds = combined_unmapped_icds
  ))
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

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

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

  return(list(icd9_list = icd9_list, rvs_map_list = rvs_maps$rvs_map_list))
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

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  invalid_matches <- lapply(matches, function(x) x[!x %in% valid_rvs_codes])
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(
      CODE = discarded_codes
    )[, .N, by = CODE][order(-N)]
    # Change column names here
    names(discarded_table) <- c("CODE", "count")
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  dt <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(dt, valid_rvs_codes)
  dt[, col := remove_5_digit_codes(col)]
  discarded_rvs <- warn_invalid_rvs(dt$matches, valid_rvs_codes)

  return(
    list(
      clin_rvs = dt$clin_rvs,
      col = dt$col,
      discarded_rvs = discarded_rvs
    )
  )
}

compute_statistics <- function(dt, rvs_icd9, rvs_map_list) {
  with_thai <- rvs_icd9[is_thai == TRUE]
  without_thai <- rvs_icd9[!rvs %in% with_thai$rvs]

  cat(sprintf(
    "There are %d RVS codes without an",
    length(unique(without_thai$rvs))
  ), "ICD-9CM equivalent recognized by the TDRG ICD9CM\n")

  rvss <- unique(unlist(dt$clin_rvs))
  cat(sprintf(
    "There are %d unique RVS codes that appear in the claims.\n",
    length(rvss)
  ))

  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf(
    "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
    length(mappable_rvs), (length(mappable_rvs) * 100 / length(rvss))
  ))

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf(
    "Of these, there are %d (%.2f%%) with more than one ICD9",
    length(multi_mapped_rvs),
    (length(multi_mapped_rvs) * 100 / length(mappable_rvs))
  ), "equivalent recognized by the Thai ICD9 library.\n")

  unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
  cat(sprintf(
    "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n",
    length(unmappable_rvs), (length(unmappable_rvs) * 100 / length(rvss))
  ))
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

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
# source(here("data-cleaning", "r_scripts", "libraries.R"))

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  require(lubridate)

  # Ensure ages are numeric
  ages <- as.numeric(ages)

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
# Function to print time estimates
print_time_estimates <- function(
    split_chunk_to_process, dt, total_time, total_rows) {
  total_rows_dt <- nrow(dt)
  total_cells <- nrow(dt) * ncol(dt)
  time_per_cell <- total_time / total_cells
  time_per_row <- total_time / total_rows_dt
  time_estimate_total_rows <- time_per_row * total_rows

  # Format the row numbers
  formatted_total_rows_dt <- format_large_numbers(total_rows_dt)
  formatted_total_rows <- format_large_numbers(total_rows)

  if (!is.na(split_chunk_to_process)) {
    # Print the results for processing a specific chunk
    cat(sprintf(
      "Time spent (total) for %2s rows:  %1.2f sec  (actual)\n",
      formatted_total_rows_dt, total_time
    ))
    cat(sprintf(
      "Time spent (t/row) for %2s rows:  %1.2f msec (actual)\n",
      formatted_total_rows_dt, time_per_row * 1000
    ))
    cat(sprintf(
      "Time spent (total) for  %2s rows: %2.2f min  (estimate)\n",
      formatted_total_rows, time_estimate_total_rows / 60
    ))
  } else {
    # Print the results for processing the whole file
    cat(sprintf(
      "Time spent (total) for %2s rows:  %1.2f sec  (actual)\n",
      formatted_total_rows, total_time
    ))
    cat(sprintf(
      "Time spent (t/row) for %2s rows:  %1.2f msec (actual)\n",
      formatted_total_rows, time_per_row * 1000
    ))
  }
}
# source(here("data-cleaning", "r_scripts", "libraries.R"))

concatenate_r_files <- function(input_path, output_file) {
  # List all .R files in the directory
  r_files <- list.files(
    path = input_path,
    pattern = "\\.R$", full.names = TRUE
  )

  # Delete the existing output file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Read and concatenate contents
  file_contents <- lapply(r_files, readLines)
  concatenated_content <- unlist(file_contents)

  # Write concatenated content to the output file
  cat(concatenated_content, file = output_file, sep = "\n")
}
# # id_series:STRING,
# # id_pin:STRING,
# # date_adm:STRING,
# # time_adm:STRING,
# # date_dis:STRING,
# # time_dis:STRING,
# # date_rec:STRING,
# # date_ref:STRING,
# # date_check:STRING,
# # id_hci:STRING,
# # id_hcp:STRING,
# # clin_outpatient:STRING,
# # clin_emergency:STRING,
# # pat_type:STRING,
# # clin_acc:STRING,
# # pat_rel:STRING,
# # pat_bdate:STRING,
# # pat_age:STRING,
# # pat_sex:STRING,
# # pat_bwt:STRING,
# # pat_memcat_parent:STRING,
# # pat_memcat_child:STRING,
# # pat_memcat_subchild:STRING,
# # clin_discharge:STRING,
# # clin_c1:STRING,
# # clin_c2:STRING,
# # claim_status:STRING,
# # claim_payout:STRING,
# # claim_charge:STRING,
# # date_ext:STRING,
# # id_year:STRING,
# # clin_icd:STRING,
# # clin_rvs:STRING,
# # icd9_list:STRING,
# # rvs_unmap_list:STRING,
# # pdx:STRING,
# # pdx_code:STRING
# PSEUDO_CLAIMSERIES:STRING,
# PSEUDO_MEM_PIN:STRING,
# DATE_ADM:STRING,
# TIME_ADM:STRING,
# DATE_DIS:STRING,
# TIME_DIS:STRING,
# DATE_REC:STRING,
# DATE_REF:STRING,
# CHKDT:STRING,
# HCI_PMCC_NO:STRING,
# HCP_NO_LIST:STRING,
# OUT_PATIENT:STRING,
# EMERGENCY:STRING,
# PATIENT_TYPE:STRING,
# ROOM_TYPE:STRING,
# DEP_REL:STRING,
# PAT_BDAY:STRING,
# PATAGE:STRING,
# PATSEX:STRING,
# PAT_BWT_KG:STRING,
# MEMCAT_PARENT_DESC:STRING,
# MEMCAT_CHILD_DESC:STRING,
# MEMCAT_SUBCHILD_DESC:STRING,
# DISPOSITION:STRING,
# PRIMARY_ILLNESS:STRING,
# SECONDARY_ILLNESS:STRING,
# ICDCODE1:STRING,
# ICDCODE2:STRING,
# ICDCODE3:STRING,
# ICDCODE4:STRING,
# ICDCODE5:STRING,
# ICDCODE6:STRING,
# ICDCODE7:STRING,
# ICDCODE8:STRING,
# ICDCODE9:STRING,
# ICDCODE10:STRING,
# ICDCODE11:STRING,
# ICDCODE12:STRING,
# ICDCODE13:STRING,
# ICDCODE14:STRING,
# ICCODED15:STRING,
# ICDCODE16:STRING,
# ICDCODE17:STRING,
# ICDCODE18:STRING,
# ICDCODE19:STRING,
# ICDCODE20:STRING,
# ICDCODE21:STRING,
# ICDCODE22:STRING,
# ICDCODE23:STRING,
# ICDCODE24:STRING,
# ICDCODE25:STRING,
# ICDCODE26:STRING,
# ICDCODE27:STRING,
# ICDCODE28:STRING,
# ICDCODE29:STRING,
# ICDCODE30:STRING,
# ICDCODE31:STRING,
# ICDCODE32:STRING,
# ICDCODE33:STRING,
# ICDCODE34:STRING,
# ICDCODE35:STRING,
# ICDCODE36:STRING,
# ICDCODE37:STRING,
# ICDCODE38:STRING,
# ICDCODE39:STRING,
# ICDCODE40:STRING,
# ICDCODE41:STRING,
# ICDCODE42:STRING,
# ICDCODE43:STRING,
# ICDCODE44:STRING,
# ICDCODE45:STRING,
# ICDCODE46:STRING,
# ICDCODE47:STRING,
# ICDCODE48:STRING,
# ICDCODE49:STRING,
# ICDCODE50:STRING,
# ICDCODE51:STRING,
# ICDCODE52:STRING,
# ICDCODE53:STRING,
# ICDCODE54:STRING,
# ICDCODE55:STRING,
# ICDCODE56:STRING,
# ICDCODE57:STRING,
# ICDCODE58:STRING,
# ICDCODE59:STRING,
# ICDCODE60:STRING,
# ICDCODE61:STRING,
# ICDCODE62:STRING,
# ICDCODE63:STRING,
# ICDCODE64:STRING,
# ICDCODE65:STRING,
# ICDCODE66:STRING,
# ICDCODE67:STRING,
# ICDCODE68:STRING,
# ICDCODE69:STRING,
# ICDCODE70:STRING,
# ICDCODE71:STRING,
# ICDCODE72:STRING,
# ICDCODE73:STRING,
# ICDCODE74:STRING,
# ICDCODE75:STRING,
# ICDCODE76:STRING,
# ICDCODE77:STRING,
# ICDCODE78:STRING,
# ICDCODE79:STRING,
# ICDCODE80:STRING,
# ICDCODE81:STRING,
# ICDCODE82:STRING,
# ICDCODE83:STRING,
# ICDCODE84:STRING,
# ICDCODE85:STRING,
# ICDCODE86:STRING,
# ICDCODE87:STRING,
# ICDCODE88:STRING,
# ICDCODE89:STRING,
# ICDCODE90:STRING,
# ICDCODE91:STRING,
# ICDCODE92:STRING,
# ICDCODE93:STRING,
# ICDCODE94:STRING,
# ICDCODE95:STRING,
# ICDCODE96:STRING,
# ICDCODE97:STRING,
# ICDCODE98:STRING,
# ICDCODE99:STRING,
# ICDCODE100:STRING,
# ICDCODE101:STRING,
# ICDCODE102:STRING,
# ICDCODE103:STRING,
# ICDCODE104:STRING,
# ICDCODE105:STRING,
# ICDCODE106:STRING,
# ICDCODE107:STRING,
# ICDCODE108:STRING,
# ICDCODE109:STRING,
# ICDCODE110:STRING,
# ICDCODE111:STRING,
# ICDCODE112:STRING,
# ICDCODE113:STRING,
# ICDCODE114:STRING,
# ICDCODE115:STRING,
# ICDCODE116:STRING,
# ICDCODE117:STRING,
# ICDCODE118:STRING,
# ICDCODE119:STRING,
# ICDCODE120:STRING,
# ICDCODE121:STRING,
# ICDCODE122:STRING,
# ICDCODE123:STRING,
# ICDCODE124:STRING,
# ICDCODE125:STRING,
# ICDCODE126:STRING,
# ICDCODE127:STRING,
# ICDCODE128:STRING,
# ICDCODE129:STRING,
# ICDCODE130:STRING,
# ICDCODE131:STRING,
# ICDCODE132:STRING,
# ICDCODE133:STRING,
# ICDCODE134:STRING,
# ICDCODE135:STRING,
# ICDCODE136:STRING,
# ICDCODE137:STRING,
# ICDCODE138:STRING,
# ICDCODE139:STRING,
# ICDCODE140:STRING,
# ICDCODE141:STRING,
# ICDCODE142:STRING,
# ICDCODE143:STRING,
# ICDCODE144:STRING,
# ICDCODE145:STRING,
# ICDCODE146:STRING,
# ICDCODE147:STRING,
# ICDCODE148:STRING,
# ICDCODE149:STRING,
# ICDCODE150:STRING,
# ICDCODE151:STRING,
# ICDCODE152:STRING,
# ICDCODE153:STRING,
# ICDCODE154:STRING,
# ICDCODE155:STRING,
# ICDCODE156:STRING,
# ICDCODE157:STRING,
# ICDCODE158:STRING,
# ICDCODE159:STRING,
# ICDCODE160:STRING,
# ICDCODE161:STRING,
# ICDCODE162:STRING,
# ICDCODE163:STRING,
# ICDCODE164:STRING,
# ICDCODE165:STRING,
# ICDCODE166:STRING,
# ICDCODE167:STRING,
# ICDCODE168:STRING,
# ICDCODE169:STRING,
# ICDCODE170:STRING,
# RVSCODE1:STRING,
# RVSCODE2:STRING,
# RVSCODE3:STRING,
# RVSCODE4:STRING,
# RVSCODE5:STRING,
# RVSCODE6:STRING,
# RVSCODE7:STRING,
# RVSCODE8:STRING,
# RVSCODE9:STRING,
# RVSCODE10:STRING,
# RVSCODE11:STRING,
# RVSCODE12:STRING,
# RVSCODE13:STRING,
# RVSCODE14:STRING,
# RVSCODE15:STRING,
# RVSCODE16:STRING,
# RVSCODE17:STRING,
# RVSCODE18:STRING,
# RVSCODE19:STRING,
# RVSCODE20:STRING,
# CLAIMS_STATUS:STRING,
# CLAIMS_PAID_AMT:STRING,
# ACR_AMOUNT_ACTUAL:STRING,
# EXTRACTION_DATE:STRING
