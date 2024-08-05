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
  "stringdist"
)

# Function to install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Install and load required packages
lapply(required_packages, install_and_load)

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})
to_read <- FALSE # TODO: Deprecated, used to be whether to forcibly read the whole file again instead of using the split parts created even if available
to_split <- TRUE # TODO: Deprecated, only used when to_sample is TRUE # Whether to split into split_parts parts (i.e. to fit in 32gb RAM).
tic("Time spent (total)               ") # Start total execution timer

na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

all_na_values <- unique(c(na_values, na_like_strings))

integer_cols <- c("OUT_PATIENT", "EMERGENCY")

factor_cols <- c(
  "PATIENT_TYPE", "ROOM_TYPE", "DEP_REL", "PATSEX", "MEMCAT_PARENT_DESC",
  "MEMCAT_CHILD_DESC",
  # "MEMCAT_SUBCHILD_DESC",
  "DISPOSITION", "CLAIMS_STATUS"
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

# Define column classes
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
  # "MEMCAT_SUBCHILD_DESC",
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
  # "pat_memcat_subchild",
  "clin_outpatient", "clin_emergency", "clin_acc",
  "clin_discharge", "clin_c1", "clin_c2", paste0("clin_icd", 1:12),
  paste0("clin_rvs", 1:20), "claim_status", "claim_charge", "claim_payout"
)
# Paths to various directories for intermediate files,
# cache, auxiliary files, etc.
intermediate_path <- "data-cleaning/data-claims/intermediate"
# Create the directory if it does not exist
if (!dir.exists(here(intermediate_path))) {
  dir.create(here(intermediate_path), recursive = TRUE)
  cat("Directory created:", intermediate_path, "\n")
} else {
  cat("Directory already exists:", intermediate_path, "\n")
}

cache_path <- "data-cleaning/cache"
# Create the directory if it does not exist
if (!dir.exists(here(cache_path))) {
  dir.create(here(cache_path), recursive = TRUE)
  cat("Directory created:", cache_path, "\n")
} else {
  cat("Directory already exists:", cache_path, "\n")
}

aux_path <- "data-cleaning/data-aux-files"
# Create the directory if it does not exist
if (!dir.exists(here(aux_path))) {
  dir.create(here(aux_path), recursive = TRUE)
  cat("Directory created:", aux_path, "\n")
} else {
  cat("Directory already exists:", aux_path, "\n")
}

excel_path <- "data-cleaning/data-excel"
# Create the directory if it does not exist
if (!dir.exists(here(excel_path))) {
  dir.create(here(excel_path), recursive = TRUE)
  cat("Directory created:", excel_path, "\n")
} else {
  cat("Directory already exists:", excel_path, "\n")
}

cleaned_claims_path <- "data-cleaning/data-claims/cleaned"
# Create the directory if it does not exist
if (!dir.exists(here(cleaned_claims_path))) {
  dir.create(here(cleaned_claims_path), recursive = TRUE)
  cat("Directory created:", cleaned_claims_path, "\n")
} else {
  cat("Directory already exists:", cleaned_claims_path, "\n")
}

grouper_output_path <- "data-cleaning/data-grouper-output"
# Create the directory if it does not exist
if (!dir.exists(here(grouper_output_path))) {
  dir.create(here(grouper_output_path), recursive = TRUE)
  cat("Directory created:", grouper_output_path, "\n")
} else {
  cat("Directory already exists:", grouper_output_path, "\n")
}

chunks_path <- "data-cleaning/data-claims/chunked"
# Create the directory if it does not exist
if (!dir.exists(here(chunks_path))) {
  dir.create(here(chunks_path), recursive = TRUE)
  cat("Directory created:", chunks_path, "\n")
} else {
  cat("Directory already exists:", chunks_path, "\n")
}

raw_claims_parts_path <- "data-cleaning/data-claims/raw/parts"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_parts_path))) {
  dir.create(here(raw_claims_parts_path), recursive = TRUE)
  cat("Directory created:", raw_claims_parts_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_parts_path, "\n")
}

raw_claims_samples_path <- "data-cleaning/data-claims/raw/samples"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_samples_path))) {
  dir.create(here(raw_claims_samples_path), recursive = TRUE)
  cat("Directory created:", raw_claims_samples_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_samples_path, "\n")
}

raw_claims_path <- "data-cleaning/data-claims/raw"
# Create the directory if it does not exist
if (!dir.exists(here(raw_claims_path))) {
  dir.create(here(raw_claims_path), recursive = TRUE)
  cat("Directory created:", raw_claims_path, "\n")
} else {
  cat("Directory already exists:", raw_claims_path, "\n")
}

profvis_path <- "data-cleaning/profvis/profvis.html"
# Create the directory if it does not exist
if (!dir.exists(here("data-cleaning/profvis"))) {
  dir.create(here("data-cleaning/profvis"), recursive = TRUE)
  cat("Directory created:", "data-cleaning/profvis", "\n")
} else {
  cat("Directory already exists:", "data-cleaning/profvis", "\n")
}

full_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the full claims file
  #'
  #' @description This function generates the file path for the
  #' full claims file, based on the year and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("claims_extract_CLAIMS_", year_to_load, "_", ver_to_use)
  } else {
    paste0("claims_extract_CLAIMS_", year_to_load, "_", ver_to_use, "_part_", sprintf("%02d", part), "_of_", split_parts)
  }
  if (fileext) filename <- paste0(filename, ".csv")
  if (is.null(part)) {
    return(here(raw_claims_path, filename))
  } else {
    return(here(raw_claims_parts_path, filename))
  }
}

total_rows_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for total rows file
  #'
  #' @description This function generates the file path for storing/retrieving
  #' the total number of rows in a claims file, based on the year and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("total_rows_", year_to_load)
  } else {
    paste0(
      "total_rows_", year_to_load, "_part_",
      sprintf("%02d", part), "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".rds")
  }
  return(here(cache_path, filename))
}

# Load cached total rows file if available, saves ~10 seconds of runtime
if (file.exists(total_rows_file())) {
  total_rows <- readRDS(total_rows_file())
  cat(paste("Total Rows via cached object:", total_rows))
} else {
  total_rows <- fread(full_claims_file(), select = 1L, header = TRUE)[, .N]
  saveRDS(total_rows, file = total_rows_file())
  cat(paste("Total Rows via fread:", total_rows))
}

# Compute sample size when splitting and when not,
# only relevant when sampling
if (to_split) {
  sample_size <- ceiling(total_rows / split_parts / sample_size_divisor)
} else {
  sample_size <- ceiling(total_rows / sample_size_divisor)
}

suffix <- paste0(
  ifelse(to_sample, paste0("_sampled_", sample_size, "_"), "_full_")
)

sampled_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the sampled claims file
  #'
  #' @description This function generates the file path for the sampled
  #' claims file, based on the year, sample size, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("sampled_claims_", year_to_load, "_", ver_to_use, "_", sample_size)
  } else {
    paste0(
      "sampled_claims_", year_to_load, "_", ver_to_use, "_", sample_size,
      "_part_", sprintf("%02d", part), "_of_", split_parts
    )
  }
  if (fileext) filename <- paste0(filename, ".csv")
  return(here(raw_claims_samples_path, filename))
}

intermediate_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the intermediate claims file
  #'
  #' @description This function generates the file path for the
  #' intermediate claims file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("intermediate_claims_", year_to_load, "_", ver_to_use, suffix)
  } else {
    paste0(
      "intermediate_claims_", year_to_load, "_", ver_to_use, suffix,
      "part_", sprintf("%02d", part), "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(intermediate_path, filename))
}

cleaned_claims_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the cleaned claims file
  #'
  #' @description This function generates the file path for the cleaned
  #' claims file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("cleaned_claims_", year_to_load, "_", ver_to_use, suffix)
  } else {
    paste0(
      "cleaned_claims_", year_to_load, "_", ver_to_use, suffix,
      "part_", sprintf("%02d", part), "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".csv")
  }
  return(here(cleaned_claims_path, filename))
}

output_txt_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the output text file for DRG grouping
  #'
  #' @description This function generates the file path for the output text file
  #' for DRG grouping, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    paste0("DRG_Grouped", "_", year_to_load, "_", ver_to_use, suffix)
  } else {
    paste0(
      "DRG_Grouped", "_", year_to_load, "_", ver_to_use, suffix,
      "part_", sprintf("%02d", part), "_of_", split_parts
    )
  }
  if (fileext) {
    filename <- paste0(filename, ".txt")
  }
  return(here(grouper_output_path, filename))
}

grouper_result_file <- function(part = NULL, fileext = TRUE) {
  #' @title Generate the file path for the grouper result file
  #'
  #' @description This function generates the file path for the
  #' grouper result file, based on the year, suffix, and part.
  #'
  #' @param part Integer. The part number of the file.
  #' Default is NULL.
  #' @param fileext Logical. Whether to include the file extension.
  #' Default is TRUE.
  #'
  #' @return Character. The generated file path.
  filename <- if (is.null(part)) {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load, "_", ver_to_use, suffix,
      "Res"
    ))
  } else {
    toupper(paste0(
      "DRG_Grouped", "_", year_to_load, "_", ver_to_use, suffix,
      "Res_", sprintf("%02d", part), "_of_", split_parts
    ))
  }
  if (fileext) {
    filename <- paste0(filename, ".TXT")
  }
  return(here(grouper_output_path, filename))
}
clean_column <- function(column_to_clean, na_like_strings) {
  #' @title Clean a column
  #'
  #' @description This function cleans a column by converting it to UTF-8,
  #' making it uppercase, removing specific characters, and replacing
  #' NA-like strings with NA.
  #'
  #' @param column_to_clean character. The column to be cleaned.
  #' @param na_like_strings character. A vector of strings considered as NA.
  #'
  #' @return character. The cleaned column.
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- iconv(column_to_clean, to = "UTF-8", sub = "byte")
  cleaned_col <- toupper(cleaned_col)
  # cleaned_col <- stri_replace_all_regex(cleaned_col, "[\\s]", "")
  # cleaned_col <- stri_replace_all_regex(cleaned_col, "[\n]", "")
  # cleaned_col <- stri_replace_all_regex(cleaned_col, "[\\]", "")
  # cleaned_col <- stri_replace_all_regex(cleaned_col, "[/]", "")
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d]+", "")
  cleaned_col <- stri_trim_both(cleaned_col)
  cleaned_col <- ifelse(cleaned_col %in% na_like_strings,
    NA_character_, cleaned_col
  )

  return(cleaned_col)
}

collapse_columns <- function(cols_to_process, na_like_strings) {
  #' @title Collapse multiple columns into a single column
  #'
  #' @description This function collapses multiple columns into a single
  #' column by concatenating their values, cleaning them, and replacing
  #' NA-like strings with NA.
  #'
  #' @param cols_to_process list. A list of columns to be collapsed.
  #' @param na_like_strings character. A vector of strings considered as NA.
  #'
  #' @return character. The collapsed and cleaned column.
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
  #' @title Replace empty strings with NA
  #'
  #' @description This function replaces empty strings, "NA", and "character(0)"
  #' with NA in character, factor, and list columns of the data table.
  #' Optionally provides a summary of replacements.
  #'
  #' @param dt data.table. The data table to be processed.
  #' @param to_view_checks logical. Whether to provide a
  #' summary of replacements.
  #'
  #' @return list. A list containing the processed data table
  #' and the replacement summary.

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
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA_character_
    ]

    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
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
  }

  return(list(data = dt, replacement_summary = replacement_summary))
}

split_to_vector <- function(column) {
  #' @title Split a column into a vector
  #'
  #' @description This function splits the elements of a column by "||"
  #' and returns a list of vectors.
  #'
  #' @param column character. The column to be split.
  #'
  #' @return list. A list of vectors obtained by splitting the column.
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
  #' @title Remap patient type
  #'
  #' @description This function remaps patient types to standardized codes
  #' and identifies any unknown types.
  #'
  #' @param pat_type character. The patient type column.
  #'
  #' @return list. A list containing the remapped patient types
  #' and the unknown types.
  known_types <- c("MEMBER", "DEPENDENT")
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "M",
    pat_type == "DEPENDENT", "D"
  )
  unknown_types <- setdiff(
    pat_type[!is.na(pat_type)],
    known_types
  )
  list(remapped = remapped_pat_type, unmapped = unknown_types)
}

remap_claim_status <- function(claim_status) {
  #' @title Remap patient type
  #'
  #' @description This function remaps patient types to standardized codes
  #' and identifies any unknown types.
  #'
  #' @param pat_type character. The patient type column.
  #'
  #' @return list. A list containing the remapped patient types
  #' and the unknown types.
  known_types <- c("DENIED", "IN-PROCESS", "PAID", "RTH", "APRV4PAYMENT")
  remapped_claim_status <- fcase(
    claim_status == "DENIED", "D",
    claim_status == "IN-PROCESS", "I",
    claim_status == "PAID", "G",
    claim_status == "RTH", "R",
    claim_status == "APRV4PAYMENT", "G"
  )
  unknown_types <- setdiff(
    claim_status[!is.na(claim_status)],
    known_types
  )
  list(remapped = remapped_claim_status, unmapped = unknown_types)
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  #' @title Remap member category parent description
  #'
  #' @description This function remaps member category parent descriptions
  #' to standardized codes and identifies any unknown parents.
  #'
  #' @param pat_memcat_parent character. The member category parent
  #' description column.
  #'
  #' @return list. A list containing the remapped parent descriptions
  #' and the unknown parents.
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
  #' @title Remap member category child description
  #'
  #' @description This function remaps member category child descriptions
  #' to standardized codes and identifies any unknown children.
  #'
  #' @param pat_memcat_child character. The member category child
  #' description column.
  #'
  #' @return list. A list containing the remapped child descriptions
  #' and the unknown children.
  known_children <- c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER", "FORMAL ECONOMY",
    "PROFESSIONAL PRACTITIONER"
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
    pat_memcat_child == "FAMILY DRIVER", "FORMAL",
    # added this myself
    pat_memcat_child == "FORMAL ECONOMY", "FORMAL",
    # added this myself
    pat_memcat_child == "PROFESSIONAL PRACTITIONER", "INFORMAL"
  )
  unknown_children <- setdiff(
    pat_memcat_child[!is.na(pat_memcat_child)],
    known_children
  )
  list(remapped = remapped_memcat_child, unmapped = unknown_children)
}

remap_disposition <- function(clin_discharge) {
  #' @title Remap clinical discharge disposition
  #'
  #' @description This function remaps clinical discharge dispositions to
  #' standardized codes and identifies any unknown dispositions.
  #'
  #' @param clin_discharge character. The clinical discharge disposition column.
  #'
  #' @return list. A list containing the remapped discharge dispositions
  #' and the unknown dispositions.
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

is_partial_file <- function(filename) {
  #' @title Check if a file is a partial file
  #'
  #' @description This function checks if a given filename indicates
  #' a partial file.
  #'
  #' @param filename character. The name of the file.
  #'
  #' @return logical. TRUE if the file is a partial file, otherwise FALSE.
  return(grepl("part", filename, ignore.case = TRUE))
}

suppress_interim_output <- function(expr) {
  #' @title Suppress interim output
  #'
  #' @description This function suppresses interim messages and warnings
  #' generated during the evaluation of an expression.
  #'
  #' @param expr expression. The expression whose output is to be suppressed.
  #'
  #' @return NULL. The function is used for its side effect of
  #' suppressing output.
  suppressMessages(suppressWarnings(capture.output(expr, file = NULL)))
}
clean_data <- function(dt) {
  #' @title Clean and preprocess data
  #' @description This function cleans and preprocesses
  #' the given data.table by performing tasks like renaming
  #' columns, collapsing and cleaning ICD and RVS columns,
  #' replacing empty strings, and remapping patient data.
  #' @param dt data.table. The data table to be cleaned.
  #' @return list. A list containing the cleaned data and
  #' various summaries.

  # Convert source year to integer
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if renaming was successful
  rename_success <- all(new_colnames %in% colnames(dt))

  # Collapse and clean ICD and RVS columns
  dt <- collapse_and_clean_icd_rvs(dt)

  # Clean clin_c1 column
  dt[, clin_c1_orig := dt$clin_c1]
  dt[, clin_c1 := clean_column(clin_c1, na_like_strings)]
  dt[, clin_c1_orig := sapply(clin_c1_orig, toString)]
  dt[, clin_c1 := sapply(clin_c1, toString)]

  # Compare cleaning results for clin_c1
  clin_c1_cleaning_comparison <- dt[
    !is.na(clin_c1_orig) & clin_c1 != clin_c1_orig,
    .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
    by = .(clin_c1_orig, clin_c1)
  ]

  # Clean clin_c2 column
  dt[, clin_c2_orig := dt$clin_c2]
  dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]
  dt[, clin_c2_orig := sapply(clin_c2_orig, toString)]
  dt[, clin_c2 := sapply(clin_c2, toString)]

  # Compare cleaning results for clin_c2
  clin_c2_cleaning_comparison <- dt[
    !is.na(clin_c2_orig) & clin_c2 != clin_c2_orig,
    .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
    by = .(clin_c2_orig, clin_c2)
  ]

  manual_multi_replace <- function(code, replacements) {
    # Iterate over each pattern and its corresponding replacement in the list
    for (pattern in names(replacements)) {
      replacement <- replacements[[pattern]]
      code <- gsub(paste0("\\b", pattern, "\\b"), replacement, code)
    }
    return(code)
  }

  # Apply the multi-replacement function using the named list
  dt[, clin_icd := lapply(clin_icd, manual_multi_replace, replacements = manual_code_replacements)]
  dt[, clin_c1 := lapply(clin_c1, manual_multi_replace, replacements = manual_code_replacements)]
  dt[, clin_c2 := lapply(clin_c2, manual_multi_replace, replacements = manual_code_replacements)]

  # Remove lumped ICD codes
  dt[, clin_c1 := remove_lumped_icd_codes(clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(clin_c2)]

  # Clean clinical columns
  clean_clin_col_res <- clean_clinical_columns(dt)
  dt <- clean_clin_col_res$dt
  discard_rvs_one <- clean_clin_col_res$discard_rvs_one
  discard_rvs_two <- clean_clin_col_res$discard_rvs_two

  # Replace empty strings with NA
  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  empty_strings_replaced_1 <- replace_result$replacement_summary

  # Remap patient data
  remapping_results <- remap_patient_data(dt, to_view_checks)
  dt <- remapping_results$data

  # Return the cleaned data and summaries
  return(list(
    data = dt,
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_unmapped = remapping_results$pat_type_unmapped,
    memcat_parent_unmapped = remapping_results$memcat_parent_unmapped,
    memcat_child_unmapped = remapping_results$memcat_child_unmapped,
    discharge_unmapped = remapping_results$discharge_unmapped,
    claim_status_unmapped = remapping_results$claim_status_unmapped,
    discard_rvs_one = discard_rvs_one,
    discard_rvs_two = discard_rvs_two,
    empty_strings_replaced_1 = empty_strings_replaced_1
  ))
}


collapse_and_clean_icd_rvs <- function(dt) {
  #' @title Collapse and clean ICD and RVS columns
  #' @description This function collapses and cleans the ICD
  #' and RVS columns in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dt[, clin_icd := collapse_columns(mget(paste0("clin_icd", 1:12)), na_like_strings)]
  dt[, paste0("clin_icd", 1:12) := NULL]
  dt[, clin_rvs := collapse_columns(mget(paste0("clin_rvs", 1:20)), na_like_strings)]
  dt[, paste0("clin_rvs", 1:20) := NULL]
  dt[, clin_icd := remove_lumped_icd_codes(clin_icd)]
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]
  return(dt)
}

clean_clinical_columns <- function(dt) {
  #' @title Clean clinical columns
  #' @description This function cleans the clinical columns
  #' in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.

  dt <- transfer_icd_codes(dt)
  dt <- deduplicate_icd_codes(dt)

  clin_c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c1, rvs_icd9
  )
  dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
  dt[, clin_c1 := clin_c1_rvs_results$col]
  clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

  # cat(clin_c1_discarded_rvs)

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]
  clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

  # cat(clin_c2_discarded_rvs)

  dt[, clin_rvs := lapply(clin_rvs, unique)]

  return_list <- list(
    dt = dt,
    discard_rvs_one = clin_c1_discarded_rvs,
    discard_rvs_two = clin_c2_discarded_rvs
  )

  # str(return_list)

  return(return_list)
}

transfer_icd_codes <- function(dt) {
  #' @title Transfer ICD codes
  #' @description This function transfers extra ICD-10 codes
  #' to clinical ICD in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dt[, clin_c1 := split_to_vector(clin_c1)]
  clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c1
  )
  dt[, clin_icd := clin_c1_result$clin_icd]
  dt[, clin_c1 := clin_c1_result$col_first]

  dt[, clin_c2 := split_to_vector(clin_c2)]
  clin_c2_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c2
  )
  dt[, clin_icd := clin_c2_result$clin_icd]
  dt[, clin_c2 := clin_c2_result$col_first]

  return(dt)
}

deduplicate_icd_codes <- function(dt) {
  #' @title Deduplicate ICD codes
  #' @description This function ensures unique ICD codes
  #' within and across clinical columns in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dedup_result <- ensure_unique_icd_codes(dt$clin_c1, dt$clin_c2, dt$clin_icd)
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]
  return(dt)
}

remap_patient_data <- function(dt, to_view_checks) {
  #' @title Remap patient data
  #' @description This function remaps patient data such as
  #' patient type, member category, and discharge disposition
  #' in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @param to_view_checks logical. Whether to view checks.
  #' @return list. A list containing the processed data and summaries.

  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL
  claim_status_unmap <- NULL


  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    pat_unmap <- result$unmapped
  }

  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    parent_unmap <- result$unmapped
  }

  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    child_unmap <- result$unmapped
  }

  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    discharge_unmap <- result$unmapped
  }

  result <- remap_claim_status(dt$claim_status)
  dt$claim_status <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    claim_status_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    claim_status_unmapped = claim_status_unmap
  ))
}
process_chunk <- function(
    chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  #' @title Process and map clinical data chunk
  #'
  #' @description This function processes a data chunk by cleaning
  #' the data, mapping ICD codes, replacing empty strings with NA,
  #' and applying primary diagnosis logic. It returns the processed
  #' chunk along with a summary of the processing steps.
  #'
  #' @param chunk data.table The input data chunk containing clinical
  #' data to be processed.
  #' @param to_view_checks logical If TRUE, enables viewing checks for
  #' debugging. If FALSE, suppresses output.
  #' @param rvs_icd9 data.frame Mapping data for RVS to ICD-9 codes.
  #' @param tdrg_icd10 data.frame Mapping data for ICD-10 codes.
  #' @param acc_pdx data.frame Data for primary diagnosis (PDX) application.
  #'
  #' @return list A list containing the processed data chunk and a
  #' summary of the processing steps.

  if (to_view_checks) {
    # cat("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  clean_result <- clean_data(chunk)
  chunk <- clean_result$data

  summary <- list(
    rename_success = clean_result$rename_success,
    ICD_replacements_1 = clean_result$ICD_replacements_1,
    ICD_replacements_2 = clean_result$ICD_replacements_2,
    pat_type_unmapped = clean_result$pat_type_unmapped,
    memcat_parent_unmapped = clean_result$memcat_parent_unmapped,
    memcat_child_unmapped = clean_result$memcat_child_unmapped,
    discharge_unmapped = clean_result$discharge_unmapped,
    claim_status_unmapped = clean_result$claim_status_unmapped,
    discard_rvs_one = clean_result$discard_rvs_one,
    discard_rvs_two = clean_result$discard_rvs_two,
    empty_strings_replaced_1 = clean_result$empty_strings_replaced_1
  )

  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  icd10_mapping_result <- implement_icd10_mapping(
    clin_c1, clin_c2, clin_icd, tdrg_icd10
  )
  chunk[, clin_c1 := icd10_mapping_result$clin_c1]
  chunk[, clin_c2 := icd10_mapping_result$clin_c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
  chunk <- chunk_replace_result$data
  summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

  pdx_result <- apply_find_pdx(
    chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Ensure consistent lengths of clin_rvs and icd9_list
  clin_rvs_len <- lengths(chunk$clin_rvs)
  icd9_list_len <- lengths(chunk$icd9_list)

  max_len <- max(c(clin_rvs_len, icd9_list_len))
  chunk$clin_rvs <- lapply(chunk$clin_rvs, function(x) {
    length(x) <- max_len
    x
  })
  chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
    length(x) <- max_len
    x
  })

  summary$unique_icds <- icd10_mapping_result$unique_icds
  summary$direct_matches <- icd10_mapping_result$direct_matches
  summary$unmatched <- icd10_mapping_result$unmatched
  summary$unmatched_sources <- icd10_mapping_result$unmatched_sources
  summary$icd10_map_dt <- icd10_mapping_result$icd10_map_dt

  summary$rvss <- rvs_mapping_result$rvss
  summary$mappable_rvs <- rvs_mapping_result$mappable_rvs
  summary$unmappable_rvs <- rvs_mapping_result$unmappable_rvs
  summary$multi_mapped_rvs <- rvs_mapping_result$multi_mapped_rvs
  summary$without_drg <- rvs_mapping_result$without_drg

  gc() # debug
  return(list(chunk = chunk, summary = summary))
}

parallelize_and_summarize_data <- function(
    dt, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, diff_chars) {
  #' @title Parallelize and summarize data processing
  #'
  #' @description This function parallelizes the data processing
  #' across multiple cores and summarizes the results.
  #'
  #' Main processing step; calls process_chunk
  #' with or without parallelization
  #' Process chunk does (per chunk):
  #' 1. Clean data
  #' 2. Maps RVS
  #' 3. Maps ICD
  #' 4. Replaces empty strings
  #' 5. Finds PDXs
  #' 6. Returns chunk and chunk summaries

  chunk_size <- ceiling(nrow(dt) / ncores)
  chunks <- split(dt, rep(1:ncores, each = chunk_size, length.out = nrow(dt)))

  if (to_parallel) {
    parallel_results <- future_lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx,
      future.seed = global_seed
    )
  } else {
    parallel_results <- lapply(
      chunks, process_chunk,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx
    )
  }

  processed_chunks <- lapply(parallel_results, function(res) res$chunk)

  dt <- rbindlist(processed_chunks)

  rm(processed_chunks) # debug

  combined_summary <- combine_chunk_summaries(parallel_results, tmp_nrow, diff_chars)

  rm(parallel_results) # debug
  gc() # debug

  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  invalid_pdx_indices <- which(
    !is.na(dt$pdx) & dt$pdx != "" & !sapply(dt$pdx, function(x) exists(x, acc_pdx_env))
  )

  if (length(invalid_pdx_indices) > 0) {
    cat(paste("Invalid PDx found:", dt$pdx[invalid_pdx_indices]))
    combined_summary$pdx_success <- FALSE
  } else {
    combined_summary$pdx_success <- TRUE
  }

  gc() # debug

  return(list(
    dt = dt,
    combined_summary = combined_summary
  ))
}
process_part <- function(
    part, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, to_write,
    to_group, to_sample, diff_chars) {
  #' @title Process Part
  #' @description Process a single part of the data, including reading, processing, and summarizing.
  #' @param part integer. The part number to process.
  #' @param ncores integer. Number of cores to use for parallel processing.
  #' @param to_view_checks logical. Whether to view checks.
  #' @param global_seed integer. Global seed for random operations.
  #' @param tmp_nrow integer. Number of intermediate rows to show.
  #' @param rvs_icd9 character. RVS ICD9 codes.
  #' @param tdrg_icd10 character. TDRG ICD10 codes.
  #' @param acc_pdx character. Accepted PDX codes.
  #' @param para logical. Whether to parallelize the process.
  #' @param to_write logical. Whether to write intermediate files.
  #' @param to_group logical. Whether to group data for batch processing.
  #' @param to_sample logical. Whether to read sample files instead of full partial files.
  #' @return list. A list containing the processed data and summary.

  start_time <- Sys.time()

  # Ensure partial and sample files exist
  ensure_partial_files_exist(part)
  if (to_sample) ensure_sample_files_exist(part)

  # Read the appropriate file
  read_result <- read_appropriate_file(part, to_sample)
  dt <- read_result$dt
  replacement_sumamry <- read_result$replacement_summary

  result <- parallelize_and_summarize_data(
    dt, ncores, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, diff_chars
  )
  dt <- result$dt
  combined_summary <- result$combined_summary

  combined_summary$replacement_summary <- replacement_sumamry

  if (to_write) write_intermediate_file(to_write, part, dt)

  if (to_group) export_for_batch_grouper(dt, year_to_load, output_txt_file(part))

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  return(list(dt = dt, combined_summary = combined_summary, processing_time = processing_time))
}
split_and_save_parts <- function() {
  #' @title Split and save parts of the data
  #' @description This function splits the data into parts and saves them as separate files.
  #' @return NULL. The function is used for its side effect of splitting and saving the data.

  if (to_split) {
    rows_per_part <- ceiling(total_rows / split_parts)
    header <- fread(full_claims_file(), nrows = 1, colClasses = "character", header = TRUE, encoding = encode, sep = sep)
    if (to_split_read) {
      # Read the entire file in one go
      files_exist <- sapply(1:split_parts, function(part) file.exists(full_claims_file(part)))
      if (any(!files_exist)) {
        full_data <- fread(full_claims_file(), na.strings = na_values, colClasses = "character", header = TRUE, encoding = encode, sep = sep)
      }
      split_and_save <- function(part) {
        chunk_file <- full_claims_file(part)
        if (!file.exists(chunk_file)) {
          start_row <- (part - 1) * rows_per_part + 1
          end_row <- min(part * rows_per_part, total_rows)
          dt <- full_data[start_row:end_row]
          setnames(dt, colnames(header))
          if (to_debug) print(head(dt), 2) # debug
          fwrite(dt, chunk_file, quote = TRUE)
          rm(dt)
          gc()
        }
      }
      lapply(1:split_parts, split_and_save)
      # Clean up the full data from memory
      if (exists("full_data")) {
        if (to_debug) print(head(full_data), 2) # debug
        rm(full_data)
      }
      gc()
    } else {
      split_and_save <- function(part) {
        chunk_file <- full_claims_file(part)
        if (!file.exists(chunk_file)) {
          start_row <- (part - 1) * rows_per_part + 1
          end_row <- min(part * rows_per_part, total_rows)
          dt <- fread(
            full_claims_file(),
            skip = start_row,
            nrows = end_row - start_row + 1,
            na.strings = na_values,
            colClasses = "character",
            header = FALSE,
            encoding = encode,
            sep = sep
          )
          setnames(dt, colnames(header))
          if (to_debug) print(head(dt), 2) # debug
          fwrite(dt, chunk_file, quote = TRUE)
          rm(dt)
          gc()
        }
      }
      lapply(1:split_parts, split_and_save)
    }
  }
}

ensure_partial_files_exist <- function(part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- full_claims_file(part)
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (part - 1) * rows_per_part + 1
    end_row <- min(part * rows_per_part, total_rows)
    header <- fread(full_claims_file(), nrows = 1, colClasses = "character", header = TRUE, encoding = encode, sep = sep)
    dt <- fread(
      full_claims_file(),
      skip = start_row,
      nrows = end_row - start_row + 1,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(header))
    fwrite(dt, chunk_file, quote = TRUE)
  }
}

ensure_sample_files_exist <- function(part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  sampled_file <- sampled_claims_file(part)
  if (!file.exists(sampled_file)) {
    header <- fread(full_claims_file(), nrows = 1, colClasses = "character", header = TRUE, encoding = encode, sep = sep)
    dt <- fread(full_claims_file(part), skip = 1, na.strings = na_values, colClasses = "character", header = FALSE, encoding = encode, sep = sep)
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(header))
    if (to_write) fwrite(dt, sampled_file, quote = TRUE)
  }
}

read_appropriate_file <- function(part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample) for a given part, drops specified columns, and casts column types.
  #' @param part integer. The part number to process.
  #' @param to_sample logical. Whether to read the sample file or the full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file(part)
  } else {
    full_claims_file(part)
  }

  dt <- fread(chunk_file, na.strings = na_values, colClasses = "character", header = TRUE, encoding = encode, sep = sep)

  if (to_debug) print(head(dt), 2) # debug

  # Drop columns
  if (any(drop_cols %in% colnames(dt))) {
    dt <- dt[, (drop_cols) := NULL]
  }

  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  replacement_summary <- replace_result$replacement_summary

  if (to_debug) print(head(dt), 2) # debug

  # Cast column types with checks
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
      dt[[col]]
    )

    # Check for NA coercion
    coerced_to_na <- which(is.na(dt[[col]]) & !is.na(original_values))
    if (length(coerced_to_na) > 0) {
      cat(sprintf(
        "Column '%s' coerced %d values to NA. First few original values: %s\n",
        col, length(coerced_to_na), paste(original_values[coerced_to_na][1:5], collapse = ", ")
      ))
    }
  }

  return(list(dt = dt, replacement_summary = replacement_summary))
}

# Function to suppress warnings for integer and numeric conversions
suppressedWarnings <- function(expr) {
  suppressWarnings({
    res <- eval(expr)
  })
  return(res)
}

read_and_save_partial <- function(start_row, end_row, part) {
  #' @title Read and Save Partial Files
  #' @description This function reads and saves partial files from the full claims file.
  #' @param start_row integer. The starting row number.
  #' @param end_row integer. The ending row number.
  #' @param part integer. The part number of the file.
  #' @return NULL. The function is used for its side effect of reading and saving partial files.
  partial_file_path <- full_claims_file(part, fileext = TRUE)
  header <- fread(full_claims_file(), nrows = 1, colClasses = "character", header = TRUE, encoding = encode, sep = sep)

  cat(paste("Reading header from:", full_claims_file()))
  cat(paste("Partial file path:", partial_file_path))
  cat(paste("Start row:", start_row, "End row:", end_row))

  dt <- NULL
  if (!file.exists(partial_file_path)) {
    cat("Partial file does not exist. Creating partial file...")
    dt <- fread(full_claims_file(),
      na.strings = na_values,
      colClasses = "character",
      nrows = end_row - start_row + 1,
      skip = start_row,
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(header))
    cat(paste("Number of rows read:", nrow(dt)))
    if (nrow(dt) > 0) {
      cat(paste("Writing partial file to:", partial_file_path))
      fwrite(dt, partial_file_path, quote = TRUE)
    } else {
      cat("No rows to save")
    }
  } else {
    cat(paste(
      "Partial file already exists. Skipping creation:",
      partial_file_path
    ))
    dt <- fread(partial_file_path, na.strings = na_values, colClasses = "character", encoding = encode, sep = sep)
  }

  if (to_sample) {
    sampled_file_path <- sampled_claims_file(part)
    cat(paste("Sampled file path:", sampled_file_path))
    if (!file.exists(sampled_file_path)) {
      cat("Sampled file does not exist. Creating new sample...")
      if (!is.null(dt) && nrow(dt) > 0) {
        sampled_dt <- sample_data(dt)
        setnames(sampled_dt, colnames(header))
        cat(paste("Writing sampled file to:", sampled_file_path))
        fwrite(sampled_dt, sampled_file_path, quote = TRUE)
      } else {
        stop("Failed to read partial file or no rows available for sampling")
      }
    } else {
      cat(paste(
        "Sampled file already exists. Skipping creation:",
        sampled_file_path
      ))
    }
  }
}

write_intermediate_file <- function(to_write, part, dt) {
  #' @title Write Intermediate File
  #' @description This function writes the intermediate data table to a file.
  #' @param part integer. The part number of the data being processed.
  #' @param dt data.table. The data table to be written.
  #' @return NULL. The function is used for its side effect of writing the data table to a file.
  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE), quote = TRUE)
  }
}
# Function to remove lumped ICD codes
remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #'
  #' @description This function removes lumped ICD codes by adding a separator
  #' between numeric and alphabetic characters.
  #'
  #' @param column character. The column to be processed.
  #'
  #' @return character. The modified column with lumped ICD codes separated.

  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

# Function to transfer extra ICD-10 codes to clinical ICD
transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to Clinical ICD
  #'
  #' @description This function transfers extra ICD-10 codes from
  #' a column to the clinical ICD.
  #'
  #' @param clin_icd list. The clinical ICD codes.
  #' @param col list. The column containing extra ICD-10 codes.
  #'
  #' @return list. A list containing updated clinical ICD and the
  #' first code of the column.

  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

# Function to get unique ICD codes
get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Get Unique ICD Codes
  #'
  #' @description This function retrieves unique ICD codes from
  #' the given columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return character. The unique ICD codes.

  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Function to create a Thai ICD-10 environment
create_thai_icd10_environment <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #'
  #' @description This function creates an environment for Thai ICD-10 codes.
  #'
  #' @param thai_icd10_codes character. The Thai ICD-10 codes.
  #'
  #' @return environment. The environment with Thai ICD-10 codes.

  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

# Function to find direct ICD matches
find_direct_icd_matches <- function(icds, thai_icd10_env) {
  #' @title Find Direct ICD Matches
  #'
  #' @description This function finds direct matches for ICD codes
  #' in the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be matched.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #'
  #' @return character. The ICD codes that have direct matches.

  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

# Function to generate ICD-10 mapping
generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  #' @title Generate ICD-10 Mapping
  #'
  #' @description This function generates a mapping of ICD-10 codes
  #' based on the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be mapped.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #' @param neoplasms_env environment. The environment with neoplasm ICD codes.
  #'
  #' @return list. A list containing the ICD mapping and the count of
  #' modified codes.

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
  #' @title Apply ICD-10 Mapping to Columns
  #'
  #' @description This function maps ICD-10 codes to the given columns
  #' using the provided environment.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #' @param icd10_env environment. The environment with ICD-10 codes.
  #'
  #' @return list. A list containing the mapped clinical columns.

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

# Function to implement ICD-10 mapping
implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10) {
  #' @title Implement ICD-10 Mapping
  #'
  #' @description This function implements the ICD-10 mapping
  #' for the given clinical columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #' @param tdrg_icd10 data.table. The table with Thai ICD-10 codes.
  #'
  #' @return list. A list containing the mapped clinical columns
  #' and related information.

  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  direct_match_codes <- find_direct_icd_matches(
    icds, thai_icd10_env
  )

  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env
  )
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count

  unmatched_icds <- setdiff(icds, names(icd_mapping))

  if (length(unmatched_icds) > 0) {
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
  fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  icd10_env <- list2env(
    setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10)
  )

  mapped_columns <- apply_icd10_mapping_to_columns(
    clin_c1, clin_c2, clin_icd, icd10_env
  )

  return(list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    direct_matches = direct_match_codes,
    unmatched = unmatched_icds,
    unmatched_sources = unmatched_sources
  ))
}

# Function to ensure unique ICD codes
ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Ensure Unique ICD Codes
  #'
  #' @description This function ensures that ICD codes are unique
  #' within and across clinical columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return list. A list containing the deduplicated clinical columns.

  # Convert lists to data.table for efficient processing
  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Deduplicate each column
  datatable[, clin_c1 := lapply(clin_c1, unique)]
  datatable[, clin_c2 := lapply(clin_c2, unique)]
  datatable[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  datatable[, clin_c1 := Map(function(c1, c2) {
    setdiff(c1, c2)
  }, clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  datatable[, clin_c2 := Map(function(c1, c2) {
    setdiff(c2, c1)
  }, clin_c1, clin_c2)]

  return(
    list(
      clin_c1 = datatable$clin_c1,
      clin_c2 = datatable$clin_c2,
      clin_icd = datatable$clin_icd
    )
  )
}
# Function to split RVS codes
split_rvs_codes <- function(rvs_icd9) {
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Function to create RVS map lists
create_rvs_map_lists <- function(with_drg) {
  setorder(with_drg, rvs, -is_drg)
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

# Function to get ICD-9 codes from clinical RVS
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

# Function to map RVS to ICD-9
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

  rvss <- unique(unlist(clin_rvs))
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  without_drg <- unique(rvs_icd9[!rvs %in% names(rvs_map_list)]$rvs)

  return_list <- list(
    icd9_list = icd9_list,
    rvs_map_list = rvs_maps$rvs_map_list,
    rvss = rvss,
    mappable_rvs = mappable_rvs,
    unmappable_rvs = unmappable_rvs,
    multi_mapped_rvs = multi_mapped_rvs,
    without_drg = without_drg
  )

  return(return_list)
}

# Function to find and append valid RVS codes
find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  datatable[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]
  datatable[, clin_rvs := mapply(
    function(rvs, matches) unique(c(rvs, matches)),
    clin_rvs, valid_matches,
    SIMPLIFY = FALSE
  )]
}

# Function to remove 5-digit codes
remove_5_digit_codes <- function(col) {
  regex_5_digit <- "\\b\\d{5}\\b"
  gsub(regex_5_digit, "", col)
}

# Function to warn about invalid RVS codes
warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  invalid_matches <- lapply(
    matches,
    function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
  )
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(
      CODE = discarded_codes
    )[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

# Function to append and remove RVS codes
append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  datatable <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(datatable, valid_rvs_codes)
  datatable[, col := remove_5_digit_codes(col)]
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  return(
    list(
      clin_rvs = datatable$clin_rvs,
      col = datatable$col,
      discarded_rvs = discarded_rvs
    )
  )
}
# Function to find the primary diagnosis (PDX) based on the provided logic
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_env) {
  #' @title Find primary diagnosis (PDX)
  #'
  #' @description This function finds the primary diagnosis (PDX) based
  #' on the provided clinical codes and acceptable PDX environment.
  #'
  #' @param clin_c1 character The first clinical code.
  #' @param clin_c2 character The second clinical code.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx_env environment The environment containing
  #' acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX code.

  check_similarity <- function(x, y) {
    score <- 0
    min_len <- min(nchar(x), nchar(y))
    for (i in 1:min_len) {
      if (substr(x, i, i) == substr(y, i, i)) {
        score <- score + 1
      }
    }
    return(score)
  }

  clin_icd <- unlist(clin_icd)

  # Get a list of all SDx that may be chosen as PDx
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, acc_pdx_env))]

  # For those with no acceptable PDx or only 1 acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # If there are multiple eligible PDx,
  # see if any are related to the starting letters
  for (cr in c(clin_c1, clin_c2)) {
    if (!is.na(cr)) {
      if (exists(cr, acc_pdx_env)) { # If clin_c* is a valid ICD-10
        # Get starting letter of clin_c*
        starting_letter <- substr(cr, 1, 1)
        # List all valid ICD-10 codes with same starting letter
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
        # If there's only one similar eligible PDx, choose that
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        }
        # If there are multiple similar eligible PDx
        if (length(starting_codes) > 1) {
          # Obtain the one that most resembles the case rate
          starting_codes <- starting_codes[
            order(sapply(
              starting_codes,
              function(x) check_similarity(cr, x)
            ), decreasing = TRUE)
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # If there is no related starting letter, choose randomly
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  #' @title Apply find PDX
  #'
  #' @description This function applies the find PDX logic
  #' to a set of clinical codes and acceptable PDX codes.
  #'
  #' @param clin_c1 list The list of first clinical codes.
  #' @param clin_c2 list The list of second clinical codes.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx character The list of acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX codes
  #' for the input clinical codes.

  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Vectorized application of find_pdx function
  find_pdx_vectorized <- function(clin_c1, clin_c2, clin_icd) {
    # Convert lists to characters for easy handling
    clin_c1_char <- sapply(
      clin_c1, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_c2_char <- sapply(
      clin_c2, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_icd_char <- sapply(
      clin_icd, function(x) paste(x, collapse = ",")
    )

    # Initialize result vectors
    pdx <- rep(NA_character_, length(clin_c1))
    pdx_code <- rep(NA_integer_, length(clin_c1))

    # Batch check clin_c1 and clin_c2
    clin_c1_check <- sapply(clin_c1_char, function(x) exists(x, acc_pdx_env))
    clin_c2_check <- sapply(clin_c2_char, function(x) exists(x, acc_pdx_env))

    pdx[clin_c1_check] <- clin_c1_char[clin_c1_check]
    pdx_code[clin_c1_check] <- 1

    clin_c2_only_check <- !clin_c1_check & clin_c2_check
    pdx[clin_c2_only_check] <- clin_c2_char[clin_c2_only_check]
    pdx_code[clin_c2_only_check] <- 2

    # Apply find_pdx function to remaining rows
    remaining_indices <- which(is.na(pdx))
    for (i in remaining_indices) {
      result <- find_pdx(
        clin_c1_char[i], clin_c2_char[i], clin_icd_char[i], acc_pdx_env
      )
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }

    return(list(pdx = pdx, pdx_code = pdx_code))
  }

  pdx_results <- find_pdx_vectorized(datatable$clin_c1, datatable$clin_c2, datatable$clin_icd)
  datatable[, pdx := pdx_results$pdx]
  datatable[, pdx_code := pdx_results$pdx_code]

  return(list(pdx = datatable$pdx, pdx_code = datatable$pdx_code))
}
# Function to generate date of birth (DOB) vectorized
generate_dob <- function(bdays, ages, date_adms) {
  #' @title Generate Date of Birth Vectorized
  #'
  #' @description This function generates a vector of dates of birth
  #' (DOB) based on birthdates, ages, and admission dates.
  #'
  #' @param bdays character. A vector of birthdates in string format.
  #' @param ages numeric. A vector of ages.
  #' @param date_adms character. A vector of admission dates in string format.
  #'
  #' @return character. A vector of dates of birth in "dd/mm/yyyy" format.

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

  # Handle cases where ages are zero:
  # For age 0, generate a random date within the past 27 days
  # from the admission date.
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

  # Handle cases where ages are positive:
  # For positive ages, subtract the truncated age in years and a random
  # number of days (up to 170) from the admission date.
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

  # Check that all years for dates are above 1900
  # years <- year(mdy(dob))
  # if (any(years < 1900)) {
  #   stop("Generated dates have years below 1900")
  # }

  return(dob)
}

# Function to prepare and write output
prepare_and_write_output <- function(output_dt, output_txt_file) {
  #' @title Prepare and Write Output
  #'
  #' @description This function prepares and writes a data table to a file,
  #' converting list columns to comma-separated strings.
  #'
  #' @param output_dt data.table. The output data table.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

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

# Function to export data for batch grouper
export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param year_to_load integer. The year to load.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob(dt$pat_bdate, dt$pat_age, dt$date_adm)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format(mdy(dt$date_adm), "%d/%m/%Y")]
  output_dt[, TimeAdm := gsub(":", "", dt$time_adm)]
  output_dt[, DateDsc := format(mdy(dt$date_dis), "%d/%m/%Y")]
  output_dt[, TimeDsc := gsub(":", "", dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]

  icd_codes_list <- lapply(dt$clin_icd, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  rvs_codes_list <- lapply(dt$icd9_list, function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20
    codes
  })
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  prepare_and_write_output(output_dt, output_txt_file)

  rm(output_dt) # debug
  gc() # debug
  if (to_debug) return(NULL) # debug
}

group_data <- function(to_group, part, dt) {
  #' @title Group data for batch processing
  #'
  #' @description This function groups the data for batch processing
  #' and exports it for the batch grouper.
  #'
  #' @param part integer. The part number of the data being processed.
  #' @param dt data.table. The data table to be grouped.
  #'
  #' @return NULL. The function is used for its side effect of
  #' grouping and exporting the data.

  if (to_group) {
    export_for_batch_grouper(
      dt, year_to_load,
      output_txt_file(part)
    )
    rm(dt) # debug
    gc() # debug
    for_batch_grouping <- fread(
      output_txt_file(part),
      sep = "|", na.strings = "--"
    )
    if (file.exists(grouper_result_file(part))) {
      batch_grouping_result <- fread(
        grouper_result_file(part),
        sep = "|", na.strings = "--"
      )
    }
  }
}
format_large_numbers <- function(x) {
  #' @title Format Large Numbers
  #'
  #' @description This function formats large numbers into a
  #' more readable string with units (k, m, b).
  #'
  #' @param x numeric. The number to be formatted.
  #'
  #' @return character. The formatted number as a string.

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

# Function to print time estimates
print_time_estimates <- function() {
  #' @title Print Time Estimates
  #'
  #' @description This function prints time estimates for
  #' processing rows in a data table.
  #'
  #' @return NULL.
  toc_data <- toc(log = TRUE)
  total_time <- toc_data$toc - toc_data$tic
  total_rows <- if (to_sample) dim_dt[1] * split_parts * sample_size_divisor else dim_dt[1] * split_parts

  total_rows_dt <- dim_dt[1] * split_parts
  total_cells <- dim_dt[1] * dim_dt[2]
  time_per_cell <- total_time / total_cells
  time_per_row <- total_time / total_rows_dt
  time_estimate_total_rows <- time_per_row * total_rows

  # Format the row numbers
  formatted_total_rows_dt <- format_large_numbers(total_rows_dt)
  formatted_total_rows <- format_large_numbers(total_rows)

  # Print the results for processing the whole file
  # cat(sprintf(
  #   "Time spent (total) for %s rows: %2.2f sec  (actual)\n",
  #   formatted_total_rows_dt, total_time
  # ))
  cat(sprintf(
    "Time spent (t/row) for %s rows: %2.2f msec\n",
    formatted_total_rows_dt, time_per_row * 1000
  ))
  cat(sprintf(
    "Time (est) (total) for %s rows: %2.2f min\n",
    formatted_total_rows, time_estimate_total_rows / 60
  ))
}

print_status_update <- function(part, split_parts, processing_times) { #
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param part integer. The current part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each part.
  elapsed_time <- sum(processing_times[1:part])
  avg_time_per_part <- elapsed_time / part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time
  cat(sprintf(
    "Status Update\nFinished: Part %d of %d\n",
    part, split_parts
  ))
  cat(sprintf("Elapsed: %d seconds\nETA: %d seconds\n", round(elapsed_time), round(estimated_remaining_time)))
}
concatenate_r_files <- function(input_path, output_file) {
  #' @title Concatenate R Files
  #'
  #' @description This function concatenates all .R files in 
  #' a specified directory into a single output file.
  #'
  #' @param input_path character. The directory containing the 
  #' .R files to concatenate.
  #' @param output_file character. The path to the output file 
  #' where the concatenated content will be written.
  #'
  #' @return NULL.

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
print_summary_tables <- function(final_combined_summaries, end_nrow) {
  #' @title Print Summary Tables
  #'
  #' @description This function prints summary tables for a given dataset.
  #'
  #' @param final_combined_summaries list. The final combined
  #' summaries to be printed.
  #' @param end_nrow integer. The number of rows to show
  #' in the summary tables.
  #'
  #' @return NULL. Prints the summary tables.

  summary <- final_combined_summaries

  cat("\n\nRename Success:\n", summary$final_rename_success, "\n\n")

  if (nrow(summary$final_ICD_replacements_1) > 0) {
    print(kable(head(summary$final_ICD_replacements_1, end_nrow),
      format = "markdown",
      caption = "ICD Text Normalization for clin_c1 Before Splitting"
    ))
  } else {
    cat(
      sprintf("\nNo ICD replacements found in clin_c1 with more than %d different characters.", diff_chars),
      "\nNote that commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }


  if (nrow(summary$final_ICD_replacements_2) > 0) {
    print(kable(head(summary$final_ICD_replacements_2, end_nrow),
      format = "markdown",
      caption = "ICD Text Normalization for clin_c2 Before Splitting"
    ))
  } else {
    cat(
      sprintf("\nNo ICD replacements found in clin_c2 with more than %d different characters.", diff_chars),
      "\nNote that commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }

  if (is.null(summary$final_pat_type_unmapped)) {
    cat("\n\nPatient Type Unmapped: NULL\n\n")
  } else {
    cat(
      "Patient Type Unmapped:\n",
      summary$final_pat_type_unmapped, "\n\n"
    )
  }

  if (is.null(summary$final_memcat_parent_unmapped)) {
    cat("Memcat Parent Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Parent Unmapped:\n",
      summary$final_memcat_parent_unmapped, "\n\n"
    )
  }

  if (is.null(summary$final_memcat_child_unmapped)) {
    cat("Memcat Child Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Child Unmapped:\n",
      summary$final_memcat_child_unmapped, "\n\n"
    )
  }

  if (is.null(summary$final_discharge_unmapped)) {
    cat("Discharge Unmapped: NULL\n\n")
  } else {
    cat(
      "Discharge Unmapped:\n",
      summary$final_discharge_unmapped, "\n\n"
    )
  }

  if (is.null(summary$final_claim_status_unmapped)) {
    cat("Claim Status Unmapped: NULL\n\n")
  } else {
    cat(
      "Claim Status Unmapped:\n",
      summary$final_claim_status_unmapped, "\n\n"
    )
  }

  if (nrow(summary$final_discard_rvs_one) > 0) {
    print(kable(head(summary$final_discard_rvs_one, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes One"
    ))
  } else {
    cat("\nNo RVS codes discarded in the first set.\n\n")
  }

  if (nrow(summary$final_discard_rvs_two) > 0) {
    print(kable(head(summary$final_discard_rvs_two, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes Two"
    ))
  } else {
    cat("\nNo RVS codes discarded in the second set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_0) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_0, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Zeroth Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the zeroth set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_1) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_1, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_2) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_2, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }

  cat(
    sprintf(
      "There are %d RVS codes without an ICD-9CM",
      summary$final_without_drg
    ), "equivalent recognized by the TDRG ICD9CM\n"
  )
  cat(
    sprintf(
      "There are %d unique RVS codes that appear in the claims.\n",
      summary$final_rvss
    )
  )
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      summary$final_mappable_rvs,
      (summary$final_mappable_rvs /
        summary$final_rvss) * 100
    )
  )
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      summary$final_multi_mapped_rvs,
      (summary$final_multi_mapped_rvs /
        summary$final_rvss) * 100
    ), "with more than one ICD9 equivalent",
    "recognized by the Thai ICD9 library.\n"
  )

  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      summary$final_unique_icds,
      summary$final_direct_matches,
      (summary$final_direct_matches /
        summary$final_unique_icds) * 100
    ), "are directly in the Thai ICD-10 library.\n"
  )

  cat(
    sprintf(
      "The modifications led to a total of %d codes",
      summary$final_unique_icds -
        summary$final_unmatched
    ), "being mapped to an equivalent in the Thai ICD10 library.\n"
  )

  cat(
    sprintf(
      "Out of these, %d were modified to match.\n",
      summary$final_unique_icds -
        summary$final_unmatched -
        summary$final_direct_matches
    )
  )

  cat(
    sprintf(
      "There are %d codes that could not",
      summary$final_unmatched
    ), "be mapped to the Thai ICD10 library.\n"
  )

  unique_icd10_map <- process_final_icd10_map(summary$final_icd10_map_dt, tmp_nrow)

  if (nrow(unique_icd10_map) > 0) {
    print(
      kable(
        head(
          unique_icd10_map,
          end_nrow
        ),
        format = "markdown",
        caption = "Modified ICD-10 codes ordered by descending Jaro-Winkler distance"
      )
    )
  } else {
    cat("\nNo modified ICD-10 codes found.\n\n")
  }

  if (nrow(summary$final_unmatched_sources) > 0) {
    print(
      kable(
        head(
          summary$final_unmatched_sources,
          end_nrow
        ),
        format = "markdown",
        caption = "Invalid ICD-10 Codes Not Found in Thai Library"
      )
    )
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }

  cat(
    "\n\nAll PDx's are in list of acceptable PDx's:\n",
    summary$final_rename_success, "\n\n"
  )
}

combine_comparison_tables <- function(
    summaries, comparison_field, tmp_nrow, diff_chars) {
  #' @title Combine Comparison Tables
  #'
  #' @description This function combines comparison tables from
  #' multiple summaries into one, and ranks rows by a custom fuzzy match score
  #' that prioritizes letter differences in ICD codes, ignoring '+', '*', ',', '.', and spaces.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param comparison_field character. The field in the summaries to compare.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #' @param diff_chars numeric. The minimum fuzzy match score to filter.
  #'
  #' @return data.table. The combined comparison table.

  # Helper function to clean strings by removing specified characters
  clean_string <- function(str) {
    gsub("[+*,.\\s]", "", str) # Remove '+', '*', ',', '.', and spaces
  }

  # Process each summary to extract comparison data
  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code, count)]
    }
    return(summary_data)
  })

  # Combine all comparison data into a single data.table
  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      count = integer(),
      differing_chars = numeric()
    ))
  }

  # Calculate Levenshtein distance (exact character differences) and add differing_chars column
  combined_comparison[, differing_chars := mapply(
    function(old, new) {
      clean_old <- clean_string(old)
      clean_new <- clean_string(new)
      # Calculate distance only if both cleaned strings are not empty
      if (nchar(clean_old) > 0 && nchar(clean_new) > 0) {
        stringdist(clean_old, clean_new, method = "lv") / max(nchar(clean_old), nchar(clean_new))
      } else {
        0 # Return 0 if either string is empty, meaning no difference
      }
    }, old_code, new_code
  )]

  # Filter rows based on diff_chars
  combined_comparison <- combined_comparison[differing_chars > diff_chars]

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      count = integer(),
      differing_chars = numeric()
    ))
  }

  # Sum counts, sort by differing_chars, and order by descending count
  combined_comparison <- combined_comparison[,
    .(count = sum(count, na.rm = TRUE), differing_chars = max(differing_chars, na.rm = TRUE)),
    by = .(old_code, new_code)
  ][order(-differing_chars, -count)]

  # Select the top rows based on tmp_nrow
  combined_comparison <- head(combined_comparison, tmp_nrow)

  return(combined_comparison)
}

# Function to filter and sort the final ICD-10 map
process_final_icd10_map <- function(icd10_map_dt, tmp_nrow = 10) {
  #' @title Process Final ICD-10 Map
  #'
  #' @description This function processes the final ICD-10 map data table
  #' by filtering out rows where the original and mapped codes are the same
  #' and sorts the result by descending Jaro-Winkler distance.
  #'
  #' @param icd10_map_dt data.table. The ICD-10 map data table.
  #' @param tmp_nrow integer. The number of rows to show in the final summary.
  #'
  #' @return data.table. The processed and sorted ICD-10 map.

  # Ensure the required package is available
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("The 'stringdist' package is required for Jaro-Winkler distance calculation.")
  }

  # Filter rows where phl_icd10 and tdrg_icd10 are different
  icd10_map_dt <- icd10_map_dt[phl_icd10 != tdrg_icd10]

  if (nrow(icd10_map_dt) == 0) {
    return(data.table(
      phl_icd10 = character(),
      tdrg_icd10 = character(),
      jw_distance = numeric()
    ))
  }

  # Calculate Jaro-Winkler distance and add jw_distance column
  icd10_map_dt[, jw_distance := stringdist::stringdist(
    phl_icd10, tdrg_icd10,
    method = "jw"
  )]

  # Sort by descending Jaro-Winkler distance
  icd10_map_dt <- icd10_map_dt[order(-jw_distance)]

  # Select the top rows based on tmp_nrow
  final_icd10_map <- head(icd10_map_dt, tmp_nrow)

  return(final_icd10_map)
}

combine_discarded_rvs_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Discarded RVS Tables
  #'
  #' @description This function combines discarded RVS tables
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains discarded RVS codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined discarded RVS table.

  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, tmp_nrow)

  return(combined_discarded)
}

combine_unmatched_icd10_codes <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Unmatched ICD-10 Codes
  #'
  #' @description This function combines unmatched ICD-10 codes
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains unmatched ICD-10 codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined unmatched ICD-10 codes table.

  combined_list <- lapply(summaries, function(summary) summary[[field]])
  combined_table <- rbindlist(combined_list, fill = TRUE)
  combined_table <- combined_table[, .(count = sum(count)),
    by = .(code, source)
  ]
  combined_table <- combined_table[order(-count)]
  return(combined_table)
}

combine_replace_empty_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Replace Empty Tables
  #'
  #' @description This function combines tables for replaced
  #' empty values from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains information on replaced empty values.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined replace empty tables.

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
  combined_replace_empty <- head(
    combined_replace_empty,
    tmp_nrow
  )

  return(combined_replace_empty)
}

combine_chunk_summaries <- function(
    parallel_results, tmp_nrow, diff_chars) {
  #' @title Combine Chunk Summaries
  #'
  #' @description This function combines summaries from
  #' multiple chunks into one summary.
  #'
  #' @param parallel_results list. A list of results from
  #' parallel processing.
  #' @param tmp_nrow integer. The number
  #' of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  summaries <- lapply(parallel_results, function(res) res$summary)
  combined_summary <- combine_summaries(summaries, tmp_nrow, diff_chars)
  return(combined_summary)
}

combine_summaries <- function(summaries, tmp_nrow, diff_chars) {
  #' @title Combine Summaries
  #'
  #' @description This function combines multiple summaries into one summary.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param tmp_nrow integer.
  #' The number of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- list(
    rename_success = all(unlist(sapply(
      summaries,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1", tmp_nrow, diff_chars
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", tmp_nrow, diff_chars
    ),
    pat_type_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$pat_type_unmapped
    ))),
    memcat_parent_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_parent_unmapped
    ))),
    memcat_child_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_child_unmapped
    ))),
    discharge_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$discharge_unmapped
    ))),
    claim_status_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$claim_status_unmapped
    ))),
    discard_rvs_one = combine_discarded_rvs_tables(
      summaries, "discard_rvs_one", tmp_nrow
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two", tmp_nrow
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1", tmp_nrow
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2", tmp_nrow
    ),
    unique_icds_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unique_icds
    ))),
    direct_matches_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$direct_matches
    ))),
    unmatched_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unmatched
    ))),
    unmatched_sources = combine_unmatched_icd10_codes(
      summaries, "unmatched_sources", tmp_nrow
    ),
    rvss = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$rvss
    )))),
    mappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$mappable_rvs
    )))),
    unmappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$unmappable_rvs
    )))),
    multi_mapped_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$multi_mapped_rvs
    )))),
    without_drg = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$without_drg
    )))),
    pdx_success = all(unlist(sapply(
      summaries,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    icd10_map_dt = rbindlist(lapply(
      summaries,
      function(summary) summary$icd10_map_dt
    ))
  )

  return(combined_summary)
}

combine_parts_summaries <- function(combined_summary, end_nrow) {
  #' @title Combine Parts Summaries
  #'
  #' @description This function combines summaries from multiple
  #' parts into one final summary.
  #'
  #' @param combined_summary list. A list of combined summaries.
  #' @param end_nrow integer. The number of rows to show in
  #' the final summary.
  #'
  #' @return list. The final combined summary.

  final_combined_summaries <- list(
    final_rename_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    final_ICD_replacements_1 = combine_comparison_tables(
      combined_summary, "ICD_replacements_1", end_nrow, diff_chars
    ),
    final_ICD_replacements_2 = combine_comparison_tables(
      combined_summary, "ICD_replacements_2", end_nrow, diff_chars
    ),
    final_pat_type_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_unmapped
    ))),
    final_memcat_parent_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_parent_unmapped
    ))),
    final_memcat_child_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_child_unmapped
    ))),
    final_discharge_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$discharge_unmapped
    ))),
    final_claim_status_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_unmapped
    ))),
    final_discard_rvs_one = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_one", end_nrow
    ),
    final_discard_rvs_two = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_two", end_nrow
    ),
    final_empty_strings_replaced_0 = combine_replace_empty_tables(
      combined_summary, "replacement_summary", end_nrow
    ),
    final_empty_strings_replaced_1 = combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_1", end_nrow
    ),
    final_empty_strings_replaced_2 = combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_2", end_nrow
    ),
    final_unique_icds = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unique_icds_count
    )))),
    final_direct_matches = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$direct_matches_count
    )))),
    final_unmatched = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unmatched_count
    )))),
    final_unmatched_sources = combine_unmatched_icd10_codes(
      combined_summary, "unmatched_sources", end_nrow
    ),
    final_rvss = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$rvss
    ))))),
    final_mappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$mappable_rvs
    ))))),
    final_unmappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$unmappable_rvs
    ))))),
    final_multi_mapped_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$multi_mapped_rvs
    ))))),
    final_without_drg = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$without_drg
    ))))),
    final_pdx_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    final_icd10_map_dt = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$icd10_map_dt
    )))
  )

  return(final_combined_summaries)
}
