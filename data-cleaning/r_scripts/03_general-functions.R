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
    warning(sprintf("Unmapped Patient Types: %s", paste(unknown_types, collapse = ", ")))
    cat("Unmapped Patient Types:\n")
    print(unknown_types)
  }

  list(original = pat_type, remapped = remapped_pat_type, unmapped = unknown_types)
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
    warning(sprintf("Unmapped Claim Statuses: %s", paste(unknown_types, collapse = ", ")))
    cat("Unmapped Claim Statuses:\n")
    print(unknown_types)
  }

  list(original = claim_status, remapped = remapped_claim_status, unmapped = unknown_types)
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
    warning(sprintf("Unmapped Memcat Parent Descriptions: %s", paste(unknown_parents, collapse = ", ")))
    cat("Unmapped Memcat Parent Descriptions:\n")
    print(unknown_parents)
  }

  list(original = pat_memcat_parent, remapped = remapped_memcat_parent, unmapped = unknown_parents)
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
    warning(sprintf("Unmapped Memcat Child Descriptions: %s", paste(unknown_children, collapse = ", ")))
    cat("Unmapped Memcat Child Descriptions:\n")
    print(unknown_children)
  }

  list(original = pat_memcat_child, remapped = remapped_memcat_child, unmapped = unknown_children)
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
    warning(sprintf("Unmapped Discharge Dispositions: %s", paste(unknown_dispositions, collapse = ", ")))
    cat("Unmapped Discharge Dispositions:\n")
    print(unknown_dispositions)
  }

  list(original = clin_discharge, remapped = remapped_discharge, unmapped = unknown_dispositions)
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
