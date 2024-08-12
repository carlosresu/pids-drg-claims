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
  "bigrquery"
)

# Install and load required packages
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

tic("Time spent (total)               ") # Start total execution timer

na_values <- c("NONE", "None", "-", "--", "---", "N/A", "n/a", "nan", "NAN")
na_like_strings <- c(
  "", " ", "  ", " ", "-", "none", "None", "NONE", "NA", "n/a",
  "N/A", "NaN", "'", "\t", "\n", "\r", "\f", "\v", "\u00A0",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005",
  "\u2006", "\u2007", "\u2008", "\u2009", "\u200A", "\u2028",
  "\u2029", "\u202F", "\u205F", "\u3000"
)

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
clean_column <- function(column_to_clean, na_like_strings, neoplasms_dt) {
  #' @title Clean a column
  #'
  #' @description This function cleans a column by converting it to UTF-8,
  #' making it uppercase, removing specific characters, and replacing
  #' NA-like strings with NA. It also restores slashes for codes
  #' matching patterns in a reference data table.
  #'
  #' @param column_to_clean character. The column to be cleaned.
  #' @param na_like_strings character. A vector of strings considered as NA.
  #' @param neoplasms_dt data.table. A table containing substrings where slashes should be preserved.
  #'
  #' @return character. The cleaned column with slashes restored as needed.

  # Step 1: Clean the column
  column_to_clean <- as.character(column_to_clean)

  # Use `stri_trans_general` for faster UTF-8 conversion
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Combine regex operations for efficiency
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d]+", "")

  # Use fast vectorized NA replacement
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # Step 2: Prepare the neoplasms_dt lookup table
  # Create a named vector directly for lookup
  lookup <- setNames(neoplasms_dt$icd10, gsub("/", "", neoplasms_dt$icd10))

  # Step 3: Restore slashes in the cleaned column using vectorization
  matched_indices <- match(cleaned_col, names(lookup))
  cleaned_col[!is.na(matched_indices)] <- lookup[matched_indices[!is.na(matched_indices)]]

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
    clean_column(col, na_like_strings, neoplasms_dt)
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

  return(
    list(
      return_data = dt,
      return_replacement_summary = replacement_summary
    )
  )
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

collapse_and_clean_icd_rvs <- function(dt) {
  #' @title Collapse and clean ICD and RVS columns
  #' @description This function collapses and cleans the ICD
  #' and RVS columns in the data.table.
  #' @param dt data.table. The data table to be processed.
  #' @return data.table. The processed data table.
  dt[, clin_icd := collapse_columns(
    mget(paste0("clin_icd", 1:12)), na_like_strings
  )]
  dt[, paste0("clin_icd", 1:12) := NULL]
  dt[, clin_rvs := collapse_columns(
    mget(paste0("clin_rvs", 1:20)), na_like_strings
  )]
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

  # Check for unmapped types and print a warning
  if (length(unknown_types) > 0) {
    warning(sprintf(
      "Unmapped Patient Types: %s",
      paste(unknown_types, collapse = ", ")
    ))
    # cat("Unmapped Patient Types:\n")
    # print(unknown_types)
  }

  list(
    original = pat_type,
    remapped = remapped_pat_type,
    unmapped = unknown_types
  )
}

remap_claim_status <- function(claim_status) {
  #' @title Remap claim status
  #'
  #' @description This function remaps claim statuses to standardized codes
  #' and identifies any unknown statuses.
  #'
  #' @param claim_status character. The claim status column.
  #'
  #' @return list. A list containing the remapped claim statuses
  #' and the unknown statuses.
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

  # Check for unmapped claim statuses and print a warning
  if (length(unknown_types) > 0) {
    warning(sprintf(
      "Unmapped Claim Statuses: %s",
      paste(unknown_types, collapse = ", ")
    ))
    # cat("Unmapped Claim Statuses:\n")
    # print(unknown_types)
  }

  list(
    original = claim_status,
    remapped = remapped_claim_status,
    unmapped = unknown_types
  )
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
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "D",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "I"
  )
  unknown_parents <- setdiff(
    pat_memcat_parent[!is.na(pat_memcat_parent)],
    known_parents
  )

  # Check for unmapped parent descriptions and print a warning
  if (length(unknown_parents) > 0) {
    warning(sprintf(
      "Unmapped Memcat Parent Descriptions: %s",
      paste(unknown_parents, collapse = ", ")
    ))
    # cat("Unmapped Memcat Parent Descriptions:\n")
    # print(unknown_parents)
  }

  list(
    original = pat_memcat_parent,
    remapped = remapped_memcat_parent,
    unmapped = unknown_parents
  )
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

  # Check for unmapped child descriptions and print a warning
  if (length(unknown_children) > 0) {
    warning(sprintf(
      "Unmapped Memcat Child Descriptions: %s",
      paste(unknown_children, collapse = ", ")
    ))
    # cat("Unmapped Memcat Child Descriptions:\n")
    # print(unknown_children)
  }

  list(
    original = pat_memcat_child,
    remapped = remapped_memcat_child,
    unmapped = unknown_children
  )
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

  # Check for unmapped discharge dispositions and print a warning
  if (length(unknown_dispositions) > 0) {
    warning(sprintf(
      "Unmapped Discharge Dispositions: %s",
      paste(unknown_dispositions, collapse = ", ")
    ))
    # cat("Unmapped Discharge Dispositions:\n")
    # print(unknown_dispositions)
  }

  list(
    original = clin_discharge,
    remapped = remapped_discharge,
    unmapped = unknown_dispositions
  )
}

remap_patient_data <- function(dt, to_view_checks) {
  #' @title Remap patient data
  #'
  #' @description This function remaps patient data such as
  #' patient type, member category, and discharge disposition
  #' in the data.table.
  #'
  #' @param dt data.table. The data table to be processed.
  #' @param to_view_checks logical. Whether to view checks.
  #'
  #' @return list. A list containing the processed data and summaries.

  # Load necessary library
  library(data.table)

  # Initialize lists for unmapped variables
  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL
  claim_status_unmap <- NULL

  # Initialize data tables for mapped variables
  pat_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  parent_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  child_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  discharge_mapped <- data.table(
    Original = character(), Mapped = character()
  )
  claim_status_mapped <- data.table(
    Original = character(), Mapped = character()
  )

  # Remap patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped

  # Create a data table for mapped patient types
  pat_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped patient types if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    pat_unmap <- result$unmapped
  }

  # Remap member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped

  # Create a data table for mapped member category parents
  parent_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped member category parents if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    parent_unmap <- result$unmapped
  }

  # Remap member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped

  # Create a data table for mapped member category children
  child_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped member category children if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    child_unmap <- result$unmapped
  }

  # Remap discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped

  # Create a data table for mapped discharge dispositions
  discharge_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped discharge dispositions if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    discharge_unmap <- result$unmapped
  }

  # Remap claim status
  result <- remap_claim_status(dt$claim_status)
  dt$claim_status <- result$remapped

  # Create a data table for mapped claim statuses
  claim_status_mapped <- unique(
    data.table(Original = result$original, Mapped = result$remapped)
  )

  # Capture unmapped claim statuses if needed
  if (length(result$unmapped) > 0 && to_view_checks) {
    claim_status_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    pat_type_mapped = pat_mapped,
    pat_memcat_parent_mapped = parent_mapped,
    pat_memcat_child_mapped = child_mapped,
    clin_discharge_mapped = discharge_mapped,
    claim_status_mapped = claim_status_mapped,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    claim_status_unmapped = claim_status_unmap
  ))
}

remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #'
  #' @description This function removes lumped ICD codes by adding a separator
  #' between numeric and alphabetic characters.
  #'
  #' @param column character. The column to be processed.
  #'
  #' @return character. The modified column with lumped ICD codes separated.
  # Use stri_replace_all_regex with a regex pattern for the desired replacement
  modified_column <- stri_replace_all_regex(
    column,
    "(?<=\\d)(?=[A-Za-z])",
    "||",
    opts_regex = stri_opts_regex()
  )
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

# Function to find and append valid RVS codes
find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  datatable[, valid_matches := lapply(
    matches,
    function(x) x[x %in% valid_rvs_codes]
  )]
  datatable[, clin_rvs := mapply(
    function(rvs, matches) unique(c(rvs, matches)),
    clin_rvs, valid_matches,
    SIMPLIFY = FALSE
  )]
}

# Function to remove 5-digit codes
remove_5_digit_codes <- function(col) {
  # Ensure input is a character vector
  col <- as.character(col) # Convert to character if not already

  # Define regex pattern for 5-digit codes
  regex_5_digit <- "\\b\\d{5}\\b"

  # Use stri_replace_all_regex to remove 5-digit codes
  modified_col <- stri_replace_all_regex(
    col,
    regex_5_digit,
    "",
    vectorize_all = FALSE # Apply replacement across all elements
  )

  return(modified_col)
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

  pdx_results <- find_pdx_vectorized(
    datatable$clin_c1,
    datatable$clin_c2,
    datatable$clin_icd
  )
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

  # # Check that all years for dates are above 1900
  # years <- year(mdy(dob))
  # if (any(years < 1900)) {
  #   stop("Generated dates have years below 1900")
  # }

  return(dob)
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
  total_rows <- if (to_sample) {
    dim_dt[1] * split_parts * sample_size_divisor
  } else {
    dim_dt[1] * split_parts
  }

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

# Function to print status updates using lubridate
print_status_update <- function(status_part, split_parts, processing_times) {
  #' @title Print Status Update
  #' @description Print the status update and estimated time remaining.
  #' @param status_part integer. The current status_part number.
  #' @param split_parts integer. Total number of parts.
  #' @param processing_times numeric. Array of processing times for each
  #' status_part.

  # Calculate elapsed time and averages
  elapsed_time <- sum(processing_times[1:status_part])
  avg_time_per_part <- elapsed_time / status_part
  estimated_total_time <- avg_time_per_part * split_parts
  estimated_remaining_time <- estimated_total_time - elapsed_time

  # Convert time to period (using lubridate)
  convert_to_hr_min_sec <- function(seconds) {
    # Round seconds to the nearest whole number
    period <- seconds_to_period(round(seconds))
    return(period)
  }

  # Calculate elapsed and remaining time
  elapsed <- convert_to_hr_min_sec(elapsed_time)
  remaining <- convert_to_hr_min_sec(estimated_remaining_time)

  # Format period to string
  format_time <- function(period) {
    # Extract components
    h <- hour(period)
    m <- minute(period)
    s <- second(period)

    # Construct time string with labels
    time_components <- c()
    if (h > 0) time_components <- c(time_components, paste0(h, "h"))
    if (m > 0 || h > 0) time_components <- c(time_components, paste0(m, "m"))
    time_components <- c(time_components, paste0(s, "s"))

    # Join components and return
    time_str <- paste(time_components, collapse = " ")
    return(trimws(time_str))
  }

  elapsed_str <- format_time(elapsed)
  remaining_str <- format_time(remaining)

  # Determine when to print the status update
  if (avg_time_per_part >= 4) {
    # Print status updates for every status_part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      status_part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  } else if (avg_time_per_part < 4 && status_part %% 5 == 0) {
    # Print status updates for every 5th, 10th, 15th status_part
    cat(sprintf(
      "\rFinished %d of %d parts in %s (ETA %s)       ",
      status_part, split_parts, elapsed_str, remaining_str
    ))
    flush.console()
  }
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

  cat("\nRename Success:\n", summary$final_rename_success, "")

  # if (nrow(summary$final_ICD_replacements_1) > 0) {
  #   print(kable(head(summary$final_ICD_replacements_1, end_nrow),
  #     format = "markdown",
  #     caption = "ICD Text Normalization for clin_c1 Before Splitting"
  #   ))
  # } else {
  #   cat(
  #     sprintf(
  #       "\nNo ICD replacements found in clin_c1 with more than %d diff. chars.",
  #       diff_chars
  #     ),
  #     "\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n"
  #   )
  # }


  # if (nrow(summary$final_ICD_replacements_2) > 0) {
  #   print(kable(head(summary$final_ICD_replacements_2, end_nrow),
  #     format = "markdown",
  #     caption = "ICD Text Normalization for clin_c2 Before Splitting"
  #   ))
  # } else {
  #   cat(
  #     sprintf(
  #       "\nNo ICD replacements found in clin_c2 with more than %d diff. chars.",
  #       diff_chars
  #     ),
  #     "\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n"
  #   )
  # }

  final_icd_replacements <- unique(rbind(
    summary$final_ICD_replacements_1,
    summary$final_ICD_replacements_2
  ))

  if (nrow(final_icd_replacements) > 0) {
    print(kable(head(final_icd_replacements, end_nrow),
      format = "markdown",
      caption = "ICD Normalized Text for clin c1 & c2 Before Splitting"
    ))
  } else {
    cat(
      sprintf(
        "\nNo ICD replacements found in clin c1 & c2 with diff chars > %d",
        diff_chars
      ),
      "\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }

  # Display unique before and after mappings for each categorical variable
  display_unique_mappings <- function(mapped_data, mapping_name, tmp_nrow) {
    #' @title Display Unique Mappings
    #'
    #' @description Displays the unique before-and-after mappings
    #' for a given dataset.
    #'
    #' @param mapped_data data.table. The data table with Original
    #' and Mapped columns.
    #' @param mapping_name character. The name of the mapping being displayed.
    #' @param tmp_nrow integer. Number of rows to display in the output.
    #'
    #' @return NULL. Prints the unique mappings.

    # Ensure the data has the correct columns
    if (
      !("Original" %in% names(mapped_data)) ||
        !("Mapped" %in% names(mapped_data))) {
      stop("The data table must contain 'Original' and 'Mapped' columns.")
    }

    # Create a data table to display unique before and after mappings
    unique_mappings <- unique(mapped_data)

    # Print the mappings using kable
    print(kable(head(unique_mappings, tmp_nrow),
      format = "markdown",
      caption = sprintf("Unique Before and After Mappings for %s", mapping_name)
    ))
  }

  display_unique_mappings(
    summary$final_pat_type_mapped, "Patient Type", tmp_nrow
  )
  display_unique_mappings(
    summary$final_memcat_parent_mapped, "Memcat Parent", tmp_nrow
  )
  display_unique_mappings(
    summary$final_memcat_child_mapped, "Memcat Child", tmp_nrow
  )
  display_unique_mappings(
    summary$final_clin_discharge_mapped, "Discharge", tmp_nrow
  )
  display_unique_mappings(
    summary$final_claim_status_mapped, "Claim Status", tmp_nrow
  )

  # if (nrow(summary$final_discard_rvs_one) > 0) {
  #   print(kable(head(summary$final_discard_rvs_one, tmp_nrow),
  #     format = "markdown",
  #     caption = "Discarded RVS Codes One"
  #   ))
  # } else {
  #   cat("\nNo RVS codes discarded in the first set.\n\n")
  # }

  # if (nrow(summary$final_discard_rvs_two) > 0) {
  #   print(kable(head(summary$final_discard_rvs_two, tmp_nrow),
  #     format = "markdown",
  #     caption = "Discarded RVS Codes Two"
  #   ))
  # } else {
  #   cat("\nNo RVS codes discarded in the second set.\n\n")
  # }

  final_discard_rvs <- rbind(
    summary$final_discard_rvs_one,
    summary$final_discard_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]

  if (nrow(final_discard_rvs) > 0) {
    print(kable(head(final_discard_rvs, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
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

  # Count the number of rows with phl_icd10 length > 5
  num_long_phl_icd10 <- sum(nchar(unique_icd10_map$phl_icd10) > 5)

  # Print the count
  cat(
    "\nNumber of rows with phl_icd10 length greater than 5:",
    num_long_phl_icd10, "of", nrow(unique_icd10_map), "rows"
  )

  # Check if there are any rows and print the table
  if (nrow(unique_icd10_map) > 0) {
    print(
      kable(
        head(
          unique_icd10_map,
          end_nrow
        ),
        format = "markdown",
        caption = "Modified ICD-10 codes ordered by descending NChar distance"
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
    "\nAll PDx's are in list of acceptable PDx's:\n",
    summary$final_rename_success, ""
  )
}

# combine_comparison_tables <- function(
#     summaries, comparison_field, tmp_nrow, diff_chars) {
#   #' @title Combine Comparison Tables
#   #'
#   #' @description This function combines comparison tables from
#   #' multiple summaries into one, and ranks rows by a custom fuzzy match score
#   #' that prioritizes letter differences in ICD codes,
#   #' ignoring '+', '*', ',', '.', and spaces.
#   #'
#   #' @param summaries list. A list of summary tables.
#   #' @param comparison_field character. The field in the summaries to compare.
#   #' @param tmp_nrow integer. The number of
#   #' rows to show in the intermediate summary.
#   #' @param diff_chars numeric. The minimum fuzzy match score to filter.
#   #'
#   #' @return data.table. The combined comparison table.

#   # Helper function to clean strings by removing specified characters
#   clean_string <- function(strings) {
#     # Remove '+', '*', ',', '.', and spaces
#     cleaned_strings <- gsub("[+*,.\\s]", "", strings)
#     return(cleaned_strings)
#   }

#   # Process each summary to extract comparison data
#   comparison_list <- lapply(summaries, function(summary) {
#     summary_data <- summary[[comparison_field]]
#     if (!is.null(summary_data) && nrow(summary_data) > 0) {
#       summary_data <- summary_data[, .(old_code, new_code, count)]
#     }
#     return(summary_data)
#   })

#   # Combine all comparison data into a single data.table
#   combined_comparison <- rbindlist(comparison_list, fill = TRUE)

#   if (nrow(combined_comparison) == 0) {
#     return(data.table(
#       old_code = character(),
#       new_code = character(),
#       count = integer(),
#       differing_chars = numeric()
#     ))
#   }

#   # Clean both old_code and new_code columns
#   combined_comparison[, `:=`(
#     clean_old = clean_string(old_code),
#     clean_new = clean_string(new_code)
#   )]

#   # Calculate the difference in number of characters between clean_old and clean_new
#   combined_comparison[, differing_chars := abs(nchar(clean_old) - nchar(clean_new))]

#   # Filter rows based on diff_chars
#   combined_comparison <- combined_comparison[differing_chars > diff_chars]

#   if (nrow(combined_comparison) == 0) {
#     return(data.table(
#       old_code = character(),
#       new_code = character(),
#       count = integer(),
#       differing_chars = numeric()
#     ))
#   }

#   # Sum counts, sort by differing_chars, and order by descending count
#   combined_comparison <- combined_comparison[,
#     .(count = sum(count, na.rm = TRUE), differing_chars = max(differing_chars, na.rm = TRUE)),
#     by = .(old_code, new_code)
#   ][order(-differing_chars, -count)]

#   # Select the top rows based on tmp_nrow
#   combined_comparison <- head(combined_comparison, tmp_nrow)

#   return(combined_comparison)
# }

combine_comparison_tables <- function(
    summaries, comparison_field, tmp_nrow = 10) {
  #' @title Combine Comparison Tables
  #'
  #' @description This function combines comparison tables from
  #' multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param comparison_field character. The field in the summaries to compare.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined comparison table with absolute differences in character lengths.

  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code)]
    }
    return(summary_data)
  })

  # Combine all the data
  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      diff_chars = integer()
    ))
  }

  # Trim whitespaces and calculate the absolute difference in character lengths for unique pairs
  combined_comparison[, `:=`(
    old_code = gsub("\\s", "", old_code),
    new_code = gsub("\\s", "", new_code)
  )]
  combined_comparison[, diff_chars := abs(nchar(old_code) - nchar(new_code))]

  # Keep only unique old_code to new_code combinations
  unique_combinations <- unique(combined_comparison)

  # Order by the absolute character difference and limit the number of rows
  unique_combinations <- unique_combinations[order(-diff_chars)]
  unique_combinations <- head(unique_combinations, tmp_nrow)

  return(unique_combinations)
}


# Function to filter and sort the final ICD-10 map
process_final_icd10_map <- function(icd10_map_dt, tmp_nrow = 10) {
  #' @title Process Final ICD-10 Map
  #'
  #' @description This function processes the final ICD-10 map data table
  #' by filtering out rows where the original and mapped codes are the same
  #' and sorts the result by descending absolute difference in the number
  #' of characters between the codes.
  #'
  #' @param icd10_map_dt data.table. The ICD-10 map data table.
  #' @param tmp_nrow integer. The number of rows to show in the final summary.
  #'
  #' @return data.table. The processed and sorted ICD-10 map.

  # Filter rows where phl_icd10 and tdrg_icd10 are different
  icd10_map_dt <- icd10_map_dt[phl_icd10 != tdrg_icd10]

  if (nrow(icd10_map_dt) == 0) {
    return(data.table(
      phl_icd10 = character(),
      tdrg_icd10 = character(),
      char_diff = numeric()
    ))
  }

  # Calculate the absolute difference in character length and add char_diff column
  icd10_map_dt[, char_diff := abs(nchar(phl_icd10) - nchar(tdrg_icd10))]

  # Sort by descending absolute difference in character length
  icd10_map_dt <- icd10_map_dt[order(-char_diff)]

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

final_combine_replace_empty_tables <- function(
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
      Total_Empty_Replaced = integer(),
      Total_NA_Replaced = integer(),
      Total_Character0_Replaced = integer(),
      Total_Elements = integer(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE),
    Total_Elements = if (to_sample) sample_size * split_parts else total_rows
  ), by = Column]

  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = (Total_Empty_Replaced / Total_Elements) * 100,
    NA_Replaced_Percentage = (Total_NA_Replaced / Total_Elements) * 100,
    Character0_Replaced_Percentage = (Total_Character0_Replaced / Total_Elements) * 100
  )]

  # Format percentages as "XX.X%"
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    NA_Replaced_Percentage = sprintf("%.2f%%", NA_Replaced_Percentage),
    Character0_Replaced_Percentage = sprintf("%.2f%%", Character0_Replaced_Percentage)
  )]

  combined_replace_empty <- combined_replace_empty[
    order(
      -as.numeric(gsub("%", "", Empty_Replaced_Percentage)),
      -as.numeric(gsub("%", "", NA_Replaced_Percentage)),
      -as.numeric(gsub("%", "", Character0_Replaced_Percentage))
    )
  ]

  combined_replace_empty <- head(combined_replace_empty, tmp_nrow)

  return(combined_replace_empty[, .(
    Column,
    Empty_Replaced_Percentage,
    NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}


combine_chunk_summaries <- function(
    summaries, tmp_nrow) {
  #' @title Combine Chunk Summaries
  #'
  #' @description This function combines summaries from
  #' multiple chunks into one summary.
  #'
  #' @param summaries list. A list of results from
  #' summaries from parallel processing.
  #' @param tmp_nrow integer. The number
  #' of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- combine_summaries(summaries, tmp_nrow)
  return(combined_summary)
}

combine_summaries <- function(summaries, tmp_nrow) {
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
      summaries, "ICD_replacements_1", tmp_nrow
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", tmp_nrow
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
    )),
    pat_type_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_type_mapped
    )),
    pat_memcat_parent_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_parent_mapped
    )),
    pat_memcat_child_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_child_mapped
    )),
    clin_discharge_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$clin_discharge_mapped
    )),
    claim_status_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$claim_status_mapped
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
      combined_summary, "ICD_replacements_1", end_nrow
    ),
    final_ICD_replacements_2 = combine_comparison_tables(
      combined_summary, "ICD_replacements_2", end_nrow
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
      combined_summary, "discard_rvs_one", tmp_nrow
    ),
    final_discard_rvs_two = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_two", tmp_nrow
    ),
    final_empty_strings_replaced_0 = final_combine_replace_empty_tables(
      combined_summary, "replacement_summary", end_nrow
    ),
    final_empty_strings_replaced_1 = final_combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_1", end_nrow
    ),
    final_empty_strings_replaced_2 = final_combine_replace_empty_tables(
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
    ))),
    final_pat_type_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_mapped
    ))),
    final_memcat_parent_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_parent_mapped
    ))),
    final_memcat_child_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_child_mapped
    ))),
    final_clin_discharge_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$clin_discharge_mapped
    ))),
    final_claim_status_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_mapped
    )))
  )

  return(final_combined_summaries)
}
ensure_partial_files_exist <- function(partial_part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a
  #' given partial_part and creates them if they don't.
  #' @param partial_part integer. The partial_part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- partial_claims_file
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (partial_part - 1) * rows_per_part + 1
    end_row <- min(partial_part * rows_per_part, total_rows)
    dt <- fread(
      file = full_claims_file,
      skip = start_row,
      nrows = end_row - start_row + 1,
      na.strings = na_values,
      colClasses = "character",
      header = FALSE,
      encoding = encode,
      sep = sep
    )
    setnames(dt, colnames(full_header))
    fwrite(dt, chunk_file, quote = TRUE)
  }
}

ensure_sample_files_exist <- function(sample_part) {
  #' @title Ensure Sample Files Exist
  #' @description This function checks if sample files exist for a given sample_part and creates them if they don't.
  #' @param sample_part integer. The sample_part number to process.
  #' @return NULL. Creates sample files as a side effect if they do not exist.
  if (!file.exists(sampled_claims_file)) {
    dt <- fread(
      here(raw_claims_parts_path, paste0(
        full_claims_prefix, year_to_load,
        "_part_", sprintf("%02d", sample_part), "_of_", split_parts, ".csv"
      )),
      skip = 1, na.strings = na_values,
      colClasses = "character", header = FALSE, encoding = encode, sep = sep
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(full_header))
    fwrite(dt, sampled_claims_file, quote = TRUE)
  }
}

read_appropriate_file <- function(read_part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample)
  #' for a given read_part, drops specified columns, and casts column types.
  #' @param read_part integer. The read_part number to process.
  #' @param to_sample logical. Whether to read the sample file or the
  #' full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year_to_load,
      "_part_", sprintf("%02d", read_part), "_of_", split_parts, ".csv"
    ))
  }

  dt <- fread(chunk_file,
    na.strings = na_values, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

  if (to_debug) print(head(dt), 2) # debug

  # Drop columns
  if (any(drop_cols %in% colnames(dt))) {
    dt <- dt[, (drop_cols) := NULL]
  }

  replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
  dt <- replace_result$return_data
  replacement_summary <- replace_result$return_replacement_summary

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
        col, length(coerced_to_na), paste(original_values[coerced_to_na][1:5],
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

# Function to export data for batch grouper
export_for_grouper <- function(dt, year_to_load, output_txt_file, loop_part) {
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

  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }

  # Write the data.table to a file
  if (to_combine) master_grouper_input_list[[loop_part]] <<- output_dt
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)

  if (to_dec_mem_usage) rm(output_dt) # debug
  if (to_dec_mem_usage) gc() # debug
  if (to_debug) {
    return(NULL)
  } # debug
}
