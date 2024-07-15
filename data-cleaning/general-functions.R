read_entire_file <- function(drop_cols) {
  #' @title Read Entire Claims Data File
  #' @description Reads the entire claims data file, dropping specified columns.
  #' @param drop_cols A vector of column names to drop.
  #' @return A data.table containing the claims data.
  dt <- fread(full_claims, na.strings = na_values, drop = drop_cols, colClasses = col_classes)
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
  #' @description Cleans a specified column by converting to UTF-8, removing spaces, and setting NA values.
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
  #'
  #' @examples
  #' column_to_clean <- c("text with spaces", "text/with/symbols!", "    trim   ", "Na-like-value")
  #' na_like_strings <- c("Na-like-value")
  #' cleaned_column <- clean_column(column_to_clean, na_like_strings)
  #' print(cleaned_column)
  
  # Ensure the input is a character vector
  column_to_clean <- as.character(column_to_clean)
  
  # Apply cleaning operations to the specified column
  cleaned_col <- iconv(column_to_clean, to = "UTF-8", sub = "byte")
  cleaned_col <- toupper(cleaned_col)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[ \n]", "")
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d\\/\\s]+", "")
  cleaned_col <- stri_trim_both(cleaned_col)
  cleaned_col <- ifelse(cleaned_col %in% na_like_strings, NA_character_, cleaned_col)
  
  return(cleaned_col)
}

collapse_columns <- function(cols_to_process, na_like_strings) {
  #' @title Collapse Columns
  #' @description Collapses specified columns into a new column.
  #' @param cols_to_process A list of character vectors representing the columns to process.
  #' @param na_like_strings A vector of strings to be treated as NA values.
  #' @return The new collapsed column as a character vector.
  #'
  #' @details
  #' This function performs the following operations:
  #' - Cleans the specified columns using `clean_column`.
  #' - Collapses the cleaned columns into a single new column, separated by "||".
  #' - Removes any "||NA" and "NA||" patterns from the new column.
  #' - Removes trailing "||" from the new column.
  #' - Sets values in the new column that match `na_like_strings` to `NA_character_`.
  #'
  #' @examples
  #' cols_to_process <- list(c("A", "B"), c("1", "2"))
  #' na_like_strings <- c("NA", "N/A")
  #' collapsed_column <- process_and_collapse_columns(cols_to_process, na_like_strings)
  #' print(collapsed_column)
  
  # Clean each column in cols_to_process
  cleaned_columns <- lapply(cols_to_process, function(col) clean_column(col, na_like_strings))
  
  # Collapse the cleaned columns into a single new column
  collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
  collapsed_column <- ifelse(collapsed_column %in% na_like_strings, NA_character_, collapsed_column)
  
  return(collapsed_column)
}

replace_empty_with_na <- function(dt) {
  #' @title Replace Empty Strings with NA
  #' @description Replaces empty strings, "NA" strings, and "character(0)" with NA values in character, factor, and list columns of a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with empty strings, "NA" strings, and "character(0)" replaced by NA values.
  #'
  #' @details
  #' This function processes all character, factor, and list columns in the data.table, replacing empty strings, "NA" strings, and "character(0)" with actual NA values.
  #'
  #' @examples
  #' library(data.table)
  #' dt <- data.table(col1 = c("A", "", "C"), col2 = factor(c("X", "", "Z")), col3 = list("NA", "", "B"))
  #' dt <- replace_empty_with_na(dt)
  #' print(dt)  # Should print modified data.table with NA values
  
  char_factor_cols <- names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col) || is.list(col))]
  dt[, (char_factor_cols) := lapply(.SD, function(x) {
    x[x == "" | x == "NA" | x == "character(0)"] <- NA_character_
    if (is.factor(x)) {
      levels(x) <- c(levels(x), NA)
    }
    return(x)
  }), .SDcols = char_factor_cols]
  return(dt)
}

split_to_vector <- function(column) {
  #' @title Split Column to Vector
  #' @description Splits strings in a column by "||" and handles NA values.
  #' @param column A column to split.
  #' @return A list of vectors resulting from the split.
  #'
  #' @details
  #' This function splits each string in the column by the delimiter "||" and converts the result into a list of vectors. NA values are handled appropriately.
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
  #' @return The remapped patient type column.
  #'
  #' @details
  #' This function remaps the `pat_type` column by mapping:
  #' - "MEMBER" to "MEM"
  #' - "DEPENDENT" to "DEP"
  #'
  #' @examples
  #' pat_type <- c("MEMBER", "DEPENDENT", "OTHER")
  #' remapped_pat_type <- remap_patient_type(pat_type)
  #' print(remapped_pat_type)  # Should print remapped patient type
  
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )
  return(remapped_pat_type)
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  #' @title Remap Member Category Parent Description
  #' @description Remaps member category parent description.
  #' @param pat_memcat_parent A character vector representing the member category parent description column.
  #' @return The remapped member category parent description column.
  #'
  #' @details
  #' This function remaps the `pat_memcat_parent` column by mapping:
  #' - "DIRECT CONTRIBUTOR" to "DIRECT"
  #' - "INDIRECT CONTRIBUTOR" to "INDIRECT"
  #'
  #' @examples
  #' pat_memcat_parent <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR", "OTHER")
  #' remapped_memcat_parent <- remap_memcat_parent_desc(pat_memcat_parent)
  #' print(remapped_memcat_parent)  # Should print remapped member category parent description
  
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )
  return(remapped_memcat_parent)
}

remap_memcat_child_desc <- function(pat_memcat_child) {
  #' @title Remap Member Category Child Description
  #' @description Remaps member category child description.
  #' @param pat_memcat_child A character vector representing the member category child description column.
  #' @return The remapped member category child description column.
  #'
  #' @details
  #' This function remaps the `pat_memcat_child` column by mapping various descriptions to their corresponding codes.
  #'
  #' @examples
  #' pat_memcat_child <- c("EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "OTHER")
  #' remapped_memcat_child <- remap_memcat_child_desc(pat_memcat_child)
  #' print(remapped_memcat_child)  # Should print remapped member category child description
  
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
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD", "INFORMAL",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL"
  )
  return(remapped_memcat_child)
}


remap_disposition <- function(clin_discharge) {
  #' @title Remap Clinical Discharge Disposition
  #' @description Remaps clinical discharge disposition.
  #' @param clin_discharge A character vector representing the clinical discharge disposition column.
  #' @return The remapped clinical discharge disposition column.
  #'
  #' @details
  #' This function remaps the `clin_discharge` column by mapping various descriptions to their corresponding integer codes:
  #' - "IMPROVED" and "RECOVERED" to 1
  #' - "HOME/DISCHARGED AGAINST MEDICAL ADVICE" to 2
  #' - "ABSCONDED" to 3
  #' - "TRANSFERRED/REFERRED" to 4
  #' - "EXPIRED" to 9
  #' - "UNDEFINED" to NA
  #'
  #' @examples
  #' clin_discharge <- c("IMPROVED", "RECOVERED", "EXPIRED", "OTHER")
  #' remapped_discharge <- remap_disposition(clin_discharge)
  #' print(remapped_discharge)  # Should print remapped clinical discharge disposition
  
  remapped_discharge <- fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )
  return(remapped_discharge)
}

format_large_numbers <- function(x) {
  #' @title Format Large Numbers
  #' @description Formats large numbers with appropriate suffixes (e.g., k for thousands, m for millions, b for billions).
  #' @param x A numeric value to format.
  #' @return A formatted string representing the large number.
  #'
  #' @details
  #' This function takes a numeric value and formats it with appropriate suffixes based on its magnitude:
  #' - Adds 'b' for billions.
  #' - Adds 'm' for millions.
  #' - Adds 'k' for thousands.
  #' If the number is less than 1,000, it returns the number as a string without any suffix.
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