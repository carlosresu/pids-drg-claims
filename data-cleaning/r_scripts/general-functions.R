read_entire_file <- function(drop_cols) {
  #' @title Read Entire Claims Data File
  #' @description Reads the entire claims data file, dropping specified columns.
  #' @param drop_cols A vector of column names to drop.
  #' @return A data.table containing the claims data.
  dt <- fread(full_claims,
    na.strings = na_values, drop = drop_cols,
    colClasses = col_classes
  )
  return(dt)
}

read_sampled_file <- function() {
  #' @title Read Sampled Claims Data File
  #' @description Reads the sampled claims data file.
  #' @return A data.table containing the sampled claims data.
  dt <- fread(sampled_claims, na.strings = na_values, colClasses = col_classes)
  return(dt)
}

sample_data <- function(dt) {
  #' @title Sample Data from a Data Table
  #' @description Samples a given data.table.
  #' @param dt A data.table to sample from.
  #' @return A sampled data.table.
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

rename_columns <- function(dt) {
  #' @title Rename Columns in Data Table
  #' @description Renames columns in a data.table.
  #' @param dt A data.table with columns to rename.
  #' @return The modified data.table with renamed columns.
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_column <- function(column_to_clean, na_like_strings) {
  #' @title Clean a Specified Column
  #' @description Cleans a specified column by converting to UTF-8,
  #' removing spaces, and setting NA values.
  #' @param column_to_clean A character vector representing the column to clean.
  #' @param na_like_strings A vector of strings to be treated as NA values.
  #' @return The cleaned column as a character vector.
  #'
  #' @details
  #' This function performs the following operations on the specified column:
  #' - Converts text to UTF-8 encoding.
  #' - Converts text to uppercase.
  #' - Removes spaces and newlines.
  #' - Removes non-alphanumeric characters, except for slashes and spaces.
  #' - Trims leading and trailing whitespace.
  #' - Sets values in `na_like_strings` to `NA_character_`.
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
  #' @title Collapse Columns
  #' @description Collapses specified columns into a new column.
  #' @param cols_to_process A list of character vectors representing
  #' the columns to process.
  #' @param na_like_strings A vector of strings to be treated as NA values.
  #' @return The new collapsed column as a character vector.
  #' @details
  #' This function performs the following operations:
  #' - Cleans the specified columns using `clean_column`.
  #' - Collapses the cleaned columns into a single new column,
  #' separated by "||".
  #' - Removes any "||NA" and "NA||" patterns from the new column.
  #' - Removes trailing "||" from the new column.
  #' - Sets values in the new column that match `na_like_strings`
  #' to `NA_character_`.
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
  #' @title Replace Empty Strings with NA
  #' @description Replaces empty strings, "NA" strings, and "character(0)"
  #' with NA values in character, factor, and list columns of a data.table.
  #' @param dt A data.table to process.
  #' @param to_view_checks A logical value to determine if the summary of
  #' replacements should be printed.
  #' @return The modified data.table with empty strings, "NA" strings, and
  #' "character(0)" replaced by NA values.
  #'
  #' @details
  #' This function processes all character, factor, and list columns in the
  #' data.table, replacing empty strings, "NA" strings, and "character(0)"
  #' with actual NA values.
  #'

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
    print(kable(replacement_summary,
      format = "markdown",
      col.names = c(
        "Column", "\"\" Replaced",
        "\"NA\" Replaced", "\"character(0)\" Replaced"
      )
    ))
  }

  return(dt)
}

split_to_vector <- function(column) {
  #' @title Split Column to Vector
  #' @description Splits strings in a column by "||" and handles NA values.
  #' @param column A column to split.
  #' @return A list of vectors resulting from the split.
  #'
  #' @details
  #' This function splits each string in the column by the delimiter "||"
  #' and converts the result into a list of vectors. NA values are
  #' handled appropriately.
  #'
  #' @examples
  #' column <- c("A||B||C", "D||E", NA)
  #' result <- split_to_vector(column)
  #' print(result)  # Should print list of vectors

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
  #' @title Remap Patient Type
  #' @description Remaps patient type.
  #' @param pat_type A character vector representing the patient type column.
  #' @return The remapped patient type column with warnings for unmapped
  #' entries.
  known_types <- c("MEMBER", "DEPENDENT")
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )
  unknown_types <- setdiff(
    pat_type[!is.na(pat_type)],
    known_types
  )
  if (length(unknown_types) > 0) {
    warning(
      "Unmapped patient types found: ",
      paste(unknown_types, collapse = ", ")
    )
  }
  return(remapped_pat_type)
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  #' @title Remap Member Category Parent Description
  #' @description Remaps member category parent description.
  #' @param pat_memcat_parent A character vector representing
  #' the member category parent description column.
  #' @return The remapped member category parent description column
  #' with warnings for unmapped entries.
  known_parents <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR")
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )
  unknown_parents <- setdiff(
    pat_memcat_parent[!is.na(pat_memcat_parent)],
    known_parents
  )
  if (length(unknown_parents) > 0) {
    warning(
      "Unmapped member category parents found: ",
      paste(unknown_parents, collapse = ", ")
    )
  }
  return(remapped_memcat_parent)
}

remap_memcat_child_desc <- function(pat_memcat_child) {
  #' @title Remap Member Category Child Description
  #' @description Remaps member category child description.
  #' @param pat_memcat_child A character vector representing the member category
  #' child description column.
  #' @return The remapped member category child description column with warnings
  #' for unmapped entries.
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
  if (length(unknown_children) > 0) {
    warning(
      "Unmapped member category children found: ",
      paste(unknown_children, collapse = ", ")
    )
  }
  return(remapped_memcat_child)
}

remap_disposition <- function(clin_discharge) {
  #' @title Remap Clinical Discharge Disposition
  #' @description Remaps clinical discharge disposition.
  #' @param clin_discharge A character vector representing the clinical
  #' discharge disposition column.
  #' @return The remapped clinical discharge disposition column with
  #' warnings for unmapped entries.
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
  if (length(unknown_dispositions) > 0) {
    warning(
      "Unmapped clinical discharge dispositions found: ",
      paste(unknown_dispositions, collapse = ", ")
    )
  }
  return(remapped_discharge)
}

format_large_numbers <- function(x) {
  #' @title Format Large Numbers
  #' @description Formats large numbers with appropriate suffixes
  #' (e.g., k for thousands, m for millions, b for billions).
  #' @param x A numeric value to format.
  #' @return A formatted string representing the large number.
  #'
  #' @details
  #' This function takes a numeric value and formats it with appropriate
  #' suffixes based on its magnitude:
  #' - Adds 'b' for billions.
  #' - Adds 'm' for millions.
  #' - Adds 'k' for thousands.
  #' If the number is less than 1,000, it returns the number as a string without
  #' any suffix.
  #'
  #' @examples
  #' format_large_numbers(1234567890)  # Should return "1.2b"
  #' format_large_numbers(1234567)     # Should return "1.2m"
  #' format_large_numbers(1234)        # Should return "1.2k"
  #' format_large_numbers(123)         # Should return "123"

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
