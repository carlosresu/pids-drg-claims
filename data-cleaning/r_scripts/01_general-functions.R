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
  dt[, clin_icd := lapply(clin_icd, manual_multi_replace,
    replacements = manual_code_replacements
  )]
  dt[, clin_c1 := lapply(clin_c1, manual_multi_replace,
    replacements = manual_code_replacements
  )]
  dt[, clin_c2 := lapply(clin_c2, manual_multi_replace,
    replacements = manual_code_replacements
  )]

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
    pat_type_mapped = remapping_results$pat_type_mapped,
    pat_memcat_parent_mapped = remapping_results$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = remapping_results$pat_memcat_child_mapped,
    clin_discharge_mapped = remapping_results$clin_discharge_mapped,
    claim_status_mapped = remapping_results$claim_status_mapped,
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
    pat_type_mapped = clean_result$pat_type_mapped,
    pat_memcat_parent_mapped = clean_result$pat_memcat_parent_mapped,
    pat_memcat_child_mapped = clean_result$pat_memcat_child_mapped,
    clin_discharge_mapped = clean_result$clin_discharge_mapped,
    claim_status_mapped = clean_result$claim_status_mapped,
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

  if (to_dec_mem_usage) gc() # debug
  return(list(chunk = chunk, summary = summary))
}

parallelize_and_summarize_data <- function(
    dt, nthreads, to_view_checks, global_seed, tmp_nrow,
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

  chunk_size <- ceiling(nrow(dt) / nthreads)
  chunks <- split(dt, rep(1:nthreads, each = chunk_size, length.out = nrow(dt)))

  if (to_parallel && is_unix) {
    parallel_results <- mclapply(
      chunks, process_chunk,
      mc.cores = nthreads,
      to_view_checks = to_view_checks,
      rvs_icd9 = rvs_icd9,
      tdrg_icd10 = tdrg_icd10,
      acc_pdx = acc_pdx
    )
  } else if (to_parallel && !is_unix) {
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

  if (to_dec_mem_usage) rm(processed_chunks) # debug

  combined_summary <- combine_chunk_summaries(parallel_results, tmp_nrow, diff_chars)

  if (to_dec_mem_usage) rm(parallel_results) # debug
  if (to_dec_mem_usage) gc() # debug

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

  if (to_dec_mem_usage) gc() # debug

  return(list(
    dt = dt,
    combined_summary = combined_summary
  ))
}

process_part <- function(
    part, nthreads, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, to_write,
    to_group, to_sample, diff_chars) {
  #' @title Process Part
  #' @description Process a single part of the data, including reading,
  #' processing, and summarizing.
  #' @param part integer. The part number to process.
  #' @param nthreads integer. Number of cores to use for parallel processing.
  #' @param to_view_checks logical. Whether to view checks.
  #' @param global_seed integer. Global seed for random operations.
  #' @param tmp_nrow integer. Number of intermediate rows to show.
  #' @param rvs_icd9 character. RVS ICD9 codes.
  #' @param tdrg_icd10 character. TDRG ICD10 codes.
  #' @param acc_pdx character. Accepted PDX codes.
  #' @param para logical. Whether to parallelize the process.
  #' @param to_write logical. Whether to write intermediate files.
  #' @param to_group logical. Whether to group data for batch processing.
  #' @param to_sample logical. Whether to read sample files instead of
  #' full partial files.
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
    dt, nthreads, to_view_checks, global_seed, tmp_nrow,
    rvs_icd9, tdrg_icd10, acc_pdx, to_parallel, diff_chars
  )
  dt <- result$dt
  combined_summary <- result$combined_summary

  combined_summary$replacement_summary <- replacement_sumamry

  if (to_write) write_intermediate_file(to_write, part, dt)

  if (to_group) export_for_grouper(dt, year_to_load, output_txt_file(part))

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  return(list(
    dt = dt,
    combined_summary = combined_summary,
    processing_time = processing_time
  ))
}

split_and_save_parts <- function() {
  #' @title Split and save parts of the data
  #' @description This function splits the data into parts and
  #' saves them as separate files.
  #' @return NULL. The function is used for its side effect of
  #' splitting and saving the data.

  if (to_split) {
    rows_per_part <- ceiling(total_rows / split_parts)

    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
    if (to_split_read) {
      # Read the entire file in one go
      files_exist <- sapply(1:split_parts, function(part) {
        file.exists(full_claims_file(part))
      })
      if (any(!files_exist)) {
        full_data <- fread(full_claims_file(),
          na.strings = na_values,
          colClasses = "character", header = TRUE, encoding = encode, sep = sep
        )
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
          if (to_dec_mem_usage) rm(dt)
          if (to_dec_mem_usage) gc()
        }
      }
      lapply(1:split_parts, split_and_save)
      # Clean up the full data from memory
      if (exists("full_data")) {
        if (to_debug) print(head(full_data), 2) # debug
        if (to_dec_mem_usage) rm(full_data)
      }
      if (to_dec_mem_usage) gc()
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
          if (to_dec_mem_usage) rm(dt)
          if (to_dec_mem_usage) gc()
        }
      }
      lapply(1:split_parts, split_and_save)
    }
  } else {
    stop("Error: to_split = FALSE is deprecated")
  }
}

ensure_partial_files_exist <- function(part) {
  #' @title Ensure Partial Files Exist
  #' @description This function checks if partial files exist for a
  #' given part and creates them if they don't.
  #' @param part integer. The part number to process.
  #' @return NULL. Creates partial files as a side effect if they do not exist.
  chunk_file <- full_claims_file(part)
  if (!file.exists(chunk_file)) {
    rows_per_part <- ceiling(total_rows / split_parts)
    start_row <- (part - 1) * rows_per_part + 1
    end_row <- min(part * rows_per_part, total_rows)
    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
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
    header <- fread(full_claims_file(),
      nrows = 1, colClasses = "character",
      header = TRUE, encoding = encode, sep = sep
    )
    dt <- fread(full_claims_file(part),
      skip = 1, na.strings = na_values,
      colClasses = "character", header = FALSE, encoding = encode, sep = sep
    )
    dt <- dt[sample(.N, min(sample_size, .N))]
    setnames(dt, colnames(header))
    fwrite(dt, sampled_file, quote = TRUE)
  }
}

read_appropriate_file <- function(part, to_sample) {
  #' @title Read Appropriate File
  #' @description This function reads the appropriate file (partial or sample)
  #' for a given part, drops specified columns, and casts column types.
  #' @param part integer. The part number to process.
  #' @param to_sample logical. Whether to read the sample file or the
  #' full partial file.
  #' @return data.table. The processed data table.

  chunk_file <- if (to_sample) {
    sampled_claims_file(part)
  } else {
    full_claims_file(part)
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
        col, length(coerced_to_na), paste(original_values[coerced_to_na][1:5],
          collapse = ", "
        )
      ))
    }
  }

  nrow_start[[part]] <<- nrow(dt)

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
  #' @description This function reads and saves partial files from the
  #' full claims file.
  #' @param start_row integer. The starting row number.
  #' @param end_row integer. The ending row number.
  #' @param part integer. The part number of the file.
  #' @return NULL. The function is used for its side effect of reading and
  #' saving partial files.
  partial_file_path <- full_claims_file(part, fileext = TRUE)
  header <- fread(full_claims_file(),
    nrows = 1, colClasses = "character",
    header = TRUE, encoding = encode, sep = sep
  )

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
    dt <- fread(partial_file_path,
      na.strings = na_values, colClasses = "character",
      encoding = encode, sep = sep
    )
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
  #' @return NULL. The function is used for its side effect of writing the
  #' data table to a file.
  if (to_write) {
    fwrite(dt, intermediate_file(part, fileext = TRUE), quote = TRUE)
  }
}
