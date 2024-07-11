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

#' write_data <- function(dt, path) {
#'   #' @title Write Data Table to File
#'   #' @description Writes a data.table to a specified path.
#'   #' @param dt A data.table to write.
#'   #' @param path The file path where the data.table should be written.
#'   fwrite(dt, path)
#' }

#' add_year_column <- function(dt, year_to_load) {
#'   #' @title Add Year Column to Data Table
#'   #' @description Adds a year column to a data.table.
#'   #' @param dt A data.table to modify.
#'   #' @param year_to_load The year to add as a new column.
#'   #' @return The modified data.table with the added year column.
#'   dt[, SRC_YR := as.integer(year_to_load)]
#'   return(dt)
#' }

rename_columns <- function(dt) {
  #' @title Rename Columns in Data Table
  #' @description Renames columns in a data.table.
  #' @param dt A data.table with columns to rename.
  #' @return The modified data.table with renamed columns.
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

#' clean_columns <- function(dt) {
#'   #' @title Clean Columns in a Data Table
#'   #' @description Cleans columns in a data.table by converting to UTF-8, removing spaces, and setting NA values.
#'   #' @param dt A data.table to clean.
#'   #' @return The cleaned data.table.
#'   #' 
#'   #' @details
#'   #' This function performs the following operations on each column in the data.table:
#'   #' - Converts text to UTF-8 encoding.
#'   #' - Converts text to uppercase.
#'   #' - Removes spaces and newlines.
#'   #' - Removes non-alphanumeric characters, except for slashes and spaces.
#'   #' - Trims leading and trailing whitespace.
#'   #' - Sets values in `na_like_strings` to `NA_character_`.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(column1 = c("text with spaces", "text/with/symbols!"),
#'   #'                  column2 = c("    trim   ", "Na-like-value"))
#'   #' na_like_strings <- c("Na-like-value") # Define na_like_strings before calling the function
#'   #' cleaned_dt <- clean_columns(dt)
#'   #' print(cleaned_dt)
#'   
#'   # Ensure the input is a data.table
#'   if (!is.data.table(dt)) {
#'     dt <- as.data.table(dt)
#'   }
#'   
#'   # Convert all columns to character type
#'   dt[] <- lapply(dt, as.character)
#'   
#'   # Apply cleaning operations to each column
#'   dt[] <- lapply(dt, function(col) {
#'     col <- iconv(col, to = "UTF-8", sub = "byte")
#'     col <- toupper(col)
#'     col <- stri_replace_all_regex(col, "[ \n]", "")
#'     col <- stri_replace_all_regex(col, "[^\\w\\d\\/\\s]+", "")
#'     col <- stri_trim_both(col)
#'     col <- ifelse(col %in% na_like_strings, NA_character_, col)
#'     return(col)
#'   })
#'   
#'   return(dt)
#' }
#' 
#' 
#' clean_columns_in_dt <- function(dt, cols_to_clean) {
#'   #' @title Clean Specified Columns in Data Table
#'   #' @description Cleans specified columns in a data.table.
#'   #' @param dt A data.table to clean.
#'   #' @param cols_to_clean A vector of column names to clean.
#'   #' @return The cleaned data.table.
#'   #'
#'   #' @details
#'   #' This function uses the `clean_columns` function to clean specified columns in the data.table.
#'   #' It applies the cleaning operations such as converting to UTF-8, removing spaces, and setting NA values.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(column1 = c("text with spaces", "text/with/symbols!"),
#'   #'                  column2 = c("    trim   ", "Na-like-value"))
#'   #' cols_to_clean <- c("column1", "column2")
#'   #' cleaned_dt <- clean_columns_in_dt(dt, cols_to_clean)
#'   #' print(cleaned_dt)
#'   
#'   dt[, (cols_to_clean) := clean_columns(.SD), .SDcols = cols_to_clean]
#'   return(dt)
#' }

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

#' process_and_collapse_columns <- function(dt, cols_to_process, new_col_name) {
#'   #' @title Process and Collapse Columns in Data Table
#'   #' @description Processes and collapses specified columns in a data.table into a new column.
#'   #' @param dt A data.table to process.
#'   #' @param cols_to_process A vector of column names to process.
#'   #' @param new_col_name The name of the new column to create.
#'   #' @return The modified data.table with the new collapsed column.
#'   #'
#'   #' @details
#'   #' This function performs the following operations:
#'   #' - Cleans the specified columns using `clean_columns`.
#'   #' - Collapses the cleaned columns into a single new column, separated by "||".
#'   #' - Removes any "||NA" and "NA||" patterns from the new column.
#'   #' - Removes trailing "||" from the new column.
#'   #' - Sets values in the new column that match `na_like_strings` to `NA_character_`.
#'   #' - Removes the original columns that were processed.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(column1 = c("A", "B"), column2 = c("1", "2"))
#'   #' cols_to_process <- c("column1", "column2")
#'   #' new_col_name <- "collapsed_column"
#'   #' processed_dt <- process_and_collapse_columns(dt, cols_to_process, new_col_name)
#'   #' print(processed_dt)
#'   
#'   dt[, (cols_to_process) := clean_columns(.SD), .SDcols = cols_to_process]
#'   dt[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
#'   dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|NA", "")]
#'   dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "NA\\|\\|", "")]
#'   dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|$", "")]
#'   dt[, (new_col_name) := ifelse(get(new_col_name) %in% na_like_strings, NA_character_, get(new_col_name))]
#'   dt[, (cols_to_process) := NULL]
#'   return(dt)
#' }

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

#' find_lumped_codes <- function(codes) {
#'   #' @title Identify Lumped Codes in a Vector
#'   #' @description Identifies lumped codes in a vector of codes.
#'   #' @param codes A vector of codes to check.
#'   #' @return A logical vector indicating which codes are lumped.
#'   #'
#'   #' @details
#'   #' This function checks each code in the vector to see if it meets the criteria for being considered "lumped":
#'   #' - The code has more than 4 characters.
#'   #' - The code contains more than one alphabetic character.
#'   #' - The code contains more than one numeric character.
#'   #'
#'   #' @examples
#'   #' codes <- c("A12", "B1234", "C12D3", NA, "ABCDE")
#'   #' lumped <- find_lumped_codes(codes)
#'   #' print(lumped)  # Should print logical vector indicating lumped codes
#'   
#'   sapply(codes, function(code) {
#'     if (is.na(code)) {
#'       return(FALSE)
#'     }
#'     nchar(code) > 4 &&
#'       str_count(code, "[A-Za-z]") > 1 &&
#'       str_count(code, "[0-9]") > 1
#'   })
#' }


#' replace_NA_as_char <- function(result) {
#'   #' @title Replace "NA" Strings with NA Values
#'   #' @description Replaces "NA" strings and empty strings with NA values in a vector.
#'   #' @param result A vector to process.
#'   #' @return The modified vector with "NA" strings and empty strings replaced by NA values.
#'   #'
#'   #' @details
#'   #' This function processes a given vector and replaces all occurrences of the string "NA" and empty strings with actual NA values.
#'   #'
#'   #' @examples
#'   #' result <- c("A", "NA", "", "B", "C")
#'   #' modified_result <- replace_NA_as_char(result)
#'   #' print(modified_result)  # Should print c("A", NA, NA, "B", "C")
#'   
#'   result[result == "NA" | result == ""] <- NA_character_
#'   return(result)
#' }


#' remove_lumped_icd_codes <- function(dt, column) {
#'   #' @title Remove Lumped ICD Codes
#'   #' @description Removes lumped ICD codes from a specified column in a data.table.
#'   #' @param dt A data.table to process.
#'   #' @param column The name of the column to process.
#'   #' @return The modified data.table with lumped ICD codes removed.
#'   #'
#'   #' @details
#'   #' This function processes the specified column in the data.table to remove lumped ICD codes by adding "||" between numeric and alphabetic characters.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(icd_codes = c("A1234B123", "C568D1234", "E901F117"))
#'   #' dt <- remove_lumped_icd_codes(dt, "icd_codes")
#'   #' print(dt)  # Should print modified ICD codes with "||" inserted
#'   
#'   dt[, (column) := gsub("(?<=\\d)(?=[A-Za-z])", "||", get(column), perl = TRUE)]
#'   return(dt)
#' }

remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #' @description Removes lumped ICD codes from a specified column.
  #' @param column A character vector representing the column to process.
  #' @return The modified column with lumped ICD codes removed.
  #'
  #' @details
  #' This function processes the specified column to remove lumped ICD codes by adding "||" between numeric and alphabetic characters.
  #'
  #' @examples
  #' icd_codes <- c("A1234B123", "C568D1234", "E901F117")
  #' modified_icd_codes <- remove_lumped_icd_codes(icd_codes)
  #' print(modified_icd_codes)  # Should print modified ICD codes with "||" inserted
  
  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
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

transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to clin_icd
  #' @description Transfers Extra ICD-10 codes in a specified column of a data.table to the appropriate list column.
  #' @param clin_icd A list of character vectors representing the clin_icd column.
  #' @param col A list of character vectors representing the column to process.
  #' @return A list containing the modified clin_icd and the first elements of col.
  #'
  #' @details
  #' This function processes the specified column by performing the following operations:
  #' - Ensures the column is a list of characters.
  #' - For rows with more than one element, splits and assigns the first element to the specified column.
  #' - Returns the modified clin_icd and the first elements of col.
  #'
  #' @examples
  #' clin_icd <- list(c("A123", "B456"), c("C789", "D012"))
  #' col <- list(c("X1", "Y2"), c("Z3", "W4"))
  #' result <- transfer_extra_icd10s_to_clin_icd(clin_icd, col)
  #' print(result$clin_icd)  # Should print modified clin_icd
  #' print(result$col_first)  # Should print the first elements of col
  
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])
  
  clin_icd <- mapply(function(icd, c1) c(icd, c1[-1]), clin_icd, col, SIMPLIFY = FALSE)
  
  return(list(clin_icd = clin_icd, col_first = col_first))
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

# Helper function to split RVS codes into with and without DRG
split_rvs_codes <- function(rvs_icd9) {
  #' @title Split RVS Codes
  #' @description Splits RVS codes into those with and without DRG.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return A list containing two data.tables: with_drg and without_drg.
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Helper function to create RVS to ICD-9-CM mapping lists
create_rvs_map_lists <- function(with_drg) {
  #' @title Create RVS Map Lists
  #' @description Creates mapping lists for RVS to ICD-9-CM codes.
  #' @param with_drg A data.table containing RVS codes with DRG.
  #' @return A list containing two lists: rvs_map_list and rvs_map_solo.
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

# Helper function to print summary statistics
print_summary_statistics <- function(rvss, rvs_icd9, rvs_map_list) {
  #' @title Print Summary Statistics
  #' @description Prints summary statistics for RVS to ICD-9-CM mappings.
  #' @param rvss A vector of unique RVS codes that appear in the claims.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @param rvs_map_list A list of RVS codes with multiple ICD-9-CM mappings.
  without_drg <- rvs_icd9[!rvs %in% names(rvs_map_list)]
  cat(sprintf('There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n', length(unique(without_drg$rvs))))
  
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf('Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n', length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))
  
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf('Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n', length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))
  
  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf('There are %d (%.2f%%) with no ICD-9-CM equivalents.\n', length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))
}

# Main function to process RVS code mappings
process_rvs_code_mapping <- function(dt, rvs_icd9) {
  #' @title Process RVS Code Mappings
  #' @description Processes RVS code mappings to ICD-9-CM codes in a data.table.
  #' @param dt A data.table to process.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return The modified data.table with processed RVS to ICD-9-CM code mappings.
  
  # Split RVS codes into with and without DRG
  split_codes <- split_rvs_codes(rvs_icd9)
  with_drg <- split_codes$with_drg
  without_drg <- split_codes$without_drg
  
  # Create RVS to ICD-9-CM mapping lists
  rvs_maps <- create_rvs_map_lists(with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list
  rvs_map_solo <- rvs_maps$rvs_map_solo
  
  # Get unique RVS codes from claims
  rvss <- unique(unlist(dt$clin_rvs))
  rvss <- intersect(rvss, rvs_icd9$rvs)
  
  # Print summary statistics
  print_summary_statistics(rvss, rvs_icd9, rvs_map_list)
  
  # Process claims data
  dt2 <- dt[lengths(clin_rvs) > 0]
  dt2 <- dt2[, .(clin_rvs), by = .(id_series)]
  
  rvs_map_solo_env <- as.environment(rvs_map_solo)
  rvs_map_list_env <- as.environment(rvs_map_list)
  
  dt2[, icd9_list := lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[!is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))]
    if (length(mappable) > 0) {
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })]
  
  dt2[, rvs_unmap_list := lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    unmappable <- codes[is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA)) & is.na(mget(codes, envir = rvs_map_list_env, ifnotfound = NA))]
    if (length(unmappable) > 0) {
      unique(unmappable)
    } else {
      NA_character_
    }
  })]
  
  # Merge results back into the original data.table
  setkey(dt, id_series)
  setkey(dt2, id_series)
  dt <- merge(dt, dt2[, .(id_series, icd9_list, rvs_unmap_list)], by = "id_series", all.x = TRUE)
  
  return(dt)
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

# Helper function to extract unique ICD codes from a data.table
extract_unique_icd_codes <- function(dt) {
  #' @title Extract Unique ICD Codes
  #' @description Extracts unique ICD codes from a data.table.
  #' @param dt A data.table containing ICD codes.
  #' @return A unique vector of ICD codes.
  icds <- unique(c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Helper function to create an environment for Thai ICD-10 codes
create_thai_icd10_env <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #' @description Creates an environment for Thai ICD-10 codes.
  #' @param thai_icd10_codes A vector of Thai ICD-10 codes.
  #' @return An environment with Thai ICD-10 codes as keys.
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes))
  return(thai_icd10_env)
}

# Helper function to identify direct matches of ICD codes in the Thai ICD-10 library
identify_direct_matches <- function(icds, thai_icd10_env) {
  #' @title Identify Direct Matches
  #' @description Identifies direct matches of ICD codes in the Thai ICD-10 library.
  #' @param icds A vector of ICD codes.
  #' @param thai_icd10_env An environment with Thai ICD-10 codes.
  #' @return A vector of directly matched ICD codes.
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))
  return(direct_match_codes)
}

# Helper function to create ICD-10 mapping
create_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  #' @title Create ICD-10 Mapping
  #' @description Creates a mapping of ICD-10 codes to Thai ICD-10 equivalents.
  #' @param icds A vector of ICD codes.
  #' @param thai_icd10_env An environment with Thai ICD-10 codes.
  #' @param neoplasms_env An environment with neoplasm codes.
  #' @return A list containing the ICD-10 mapping and the count of modified codes.
  icd_mapping <- list()
  modified_count <- 0
  for (d in icds) {
    d <- str_trim(d)
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
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

# Helper function to map ICD-10 codes in a data.table
map_icd10_codes <- function(dt, icd10_env) {
  #' @title Map ICD-10 Codes
  #' @description Maps ICD-10 codes in a data.table.
  #' @param dt A data.table containing ICD codes to map.
  #' @param icd10_env An environment with ICD-10 mappings.
  map_icd10 <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }
  dt[, clin_c1 := lapply(clin_c1, map_icd10)]
  dt[, clin_c2 := lapply(clin_c2, map_icd10)]
  dt[, clin_icd := lapply(clin_icd, map_icd10)]
  return(dt)
}

# Main function to process ICD-10 mappings
process_icd10_mapping <- function(dt) {
  #' @title Process ICD-10 Mappings
  #' @description Processes ICD-10 mappings in a data.table using the Thai ICD-10 library.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed ICD-10 mappings.
  
  # Extract unique ICD codes
  icds <- extract_unique_icd_codes(dt)
  thai_icd10 <- unique(tdrg_icd10$CODE)
  
  # Create environments for Thai ICD-10 codes and neoplasms
  thai_icd10_env <- create_thai_icd10_env(thai_icd10)
  neoplasms <- unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  neoplasms_env <- create_thai_icd10_env(neoplasms)
  
  # Identify direct matches
  direct_match_codes <- identify_direct_matches(icds, thai_icd10_env)
  cat(sprintf("There are %d unique entries for ICD-10 codes, of which %d (%.2f%%) are directly in the Thai ICD-10 library\n", length(icds), length(direct_match_codes), length(direct_match_codes) * 100 / length(icds)))
  
  # Create ICD-10 mapping
  icd_mapping_info <- create_icd10_mapping(icds, thai_icd10_env, neoplasms_env)
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf('The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n', length(icd_mapping)))
  cat(sprintf('Out of these, %d were modified to match.\n', modified_count))
  
  # Identify unmatched ICD codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf('There are %d codes that could not be mapped to the Thai ICD10 library:\n', length(unmatched_icds)))
    unmatched_sources <- data.table(
      code = unmatched_icds,
      source = NA_character_
    )
    for (col in c("clin_c1", "clin_c2", "clin_icd")) {
      unmatched_sources[code %in% unlist(dt[[col]]), source := col]
    }
    print(unmatched_sources)
  }
  
  # Create ICD-10 mapping data.table and environment
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  fwrite(icd10_map, here(path_to_cache, paste0("icd10_map_file_", year_to_load, ".csv")))
  icd10_env <- list2env(setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10))
  
  # Map ICD-10 codes in the data.table
  dt <- map_icd10_codes(dt, icd10_env)
  return(dt)
}


#' # Helper function to initialize clin_rvs column
#' initialize_clin_rvs <- function(dt) {
#'   #' @title Initialize clin_rvs Column
#'   #' @description Ensures the clin_rvs column in the data.table is a list of characters.
#'   #' @param dt A data.table to process.
#'   #' @return The modified data.table with initialized clin_rvs column.
#'   dt[, clin_rvs := lapply(clin_rvs, function(x) if (is.null(x)) character() else x)]
#'   return(dt)
#' }

#' # Helper function to find and append 5-digit numeric codes
#' find_and_append_codes <- function(rvs, col_value, regex_5_digit) {
#'   #' @title Find and Append 5-Digit Codes
#'   #' @description Finds and appends 5-digit numeric codes from the column value to the rvs list.
#'   #' @param rvs A list of existing RVS codes.
#'   #' @param col_value The value from the specified column.
#'   #' @param regex_5_digit The regular expression to match 5-digit numeric codes.
#'   #' @return The modified list of RVS codes with 5-digit codes appended.
#'   matches <- unlist(regmatches(col_value, gregexpr(regex_5_digit, col_value)))
#'   if (length(matches) > 0) {
#'     rvs <- c(rvs, matches)
#'   }
#'   return(rvs)
#' }
#' 
#' # Helper function to remove 5-digit numeric codes from a column value
#' remove_5_digit_codes <- function(col_value, regex_5_digit) {
#'   #' @title Remove 5-Digit Codes
#'   #' @description Removes 5-digit numeric codes from the column value.
#'   #' @param col_value The value from the specified column.
#'   #' @param regex_5_digit The regular expression to match 5-digit numeric codes.
#'   #' @return The modified column value with 5-digit codes removed.
#'   gsub(regex_5_digit, "", col_value)
#' }

# Helper function to find and append 5-digit codes
find_and_append_codes <- function(clin_rvs, col, regex_5_digit) {
  codes_to_append <- regmatches(col, gregexpr(regex_5_digit, col))[[1]]
  clin_rvs <- c(clin_rvs, codes_to_append)
  return(clin_rvs)
}

# Helper function to remove 5-digit codes
remove_5_digit_codes <- function(col, regex_5_digit) {
  col <- gsub(regex_5_digit, "", col)
  return(col)
}

#' # Main function to append and remove 5-digit numeric codes
#' append_and_remove_rvs <- function(dt, col) {
#'   #' @title Append and Remove 5-Digit Codes (i.e. 5-digit RVS procedure codes)
#'   #' @description Appends and removes 5-digit numeric codes (i.e. 5-digit RVS procedure codes) from a specified column in a data.table.
#'   #' @param dt A data.table to process.
#'   #' @param col The name of the column to process.
#'   #' @return The modified data.table with 5-digit numeric codes appended and removed.
#'   
#'   # Initialize clin_rvs column
#'   # dt <- initialize_clin_rvs(dt)  # unwrapped/deprecated function
#'   dt[, clin_rvs := lapply(clin_rvs, function(x) if (is.null(x)) character() else x)]
#'   
#'   # Regular expression to match 5-digit numeric codes
#'   regex_5_digit <- "\\b\\d{5}\\b" 
#'   
#'   # Append and remove 5-digit numeric codes
#'   dt[, `:=` (
#'     clin_rvs = mapply(find_and_append_codes, clin_rvs, get(col), MoreArgs = list(regex_5_digit = regex_5_digit), SIMPLIFY = FALSE),
#'     tmp_col = lapply(get(col), remove_5_digit_codes, regex_5_digit = regex_5_digit)
#'   )]
#'   
#'   # Update the specified column and remove the temporary column
#'   dt[, (col) := tmp_col]
#'   dt[, tmp_col := NULL]
#'   
#'   return(dt)
#' }

append_and_remove_rvs <- function(clin_rvs, col) {
  #' @title Append and Remove 5-Digit Codes
  #' @description Appends and removes 5-digit numeric codes (i.e. 5-digit RVS procedure codes) from a specified column.
  #' @param clin_rvs A list of character vectors representing the clin_rvs column.
  #' @param col A character vector representing the column to process.
  #' @return A list containing the modified clin_rvs and the modified col.
  #'
  #' @details
  #' This function performs the following operations:
  #' - Ensures the clin_rvs column is a list of characters.
  #' - Finds and appends 5-digit numeric codes from the specified column to the clin_rvs column.
  #' - Removes 5-digit numeric codes from the specified column.
  #'
  #' @examples
  #' clin_rvs <- list(c("A", "B"), c("C", "D"))
  #' col <- c("12345 E", "67890 F")
  #' result <- append_and_remove_rvs(clin_rvs, col)
  #' print(result$clin_rvs)  # Should print modified clin_rvs with 5-digit codes appended
  #' print(result$col)  # Should print the modified col with 5-digit codes removed
  
  # Ensure the clin_rvs column is a list of characters
  clin_rvs <- lapply(clin_rvs, function(x) if (is.null(x)) character() else x)
  
  # Regular expression to match 5-digit numeric codes
  regex_5_digit <- "\\b\\d{5}\\b" 
  
  # Find and append 5-digit codes, and remove them from the specified column
  modified_clin_rvs <- mapply(find_and_append_codes, clin_rvs, col, MoreArgs = list(regex_5_digit = regex_5_digit), SIMPLIFY = FALSE)
  modified_col <- lapply(col, remove_5_digit_codes, regex_5_digit = regex_5_digit)
  
  return(list(clin_rvs = modified_clin_rvs, col = unlist(modified_col)))
}

#' deduplicate_columns <- function(dt, columns) {
#'   #' @title Deduplicate Columns
#'   #' @description Deduplicates specified columns in a data.table.
#'   #' @param dt A data.table to process.
#'   #' @param columns A vector of column names to deduplicate.
#'   #' @return The modified data.table with deduplicated columns.
#'   #'
#'   #' @details
#'   #' This function processes each specified column in the data.table and removes duplicate entries within each column.
#'   #' The columns to be deduplicated are specified in the `columns` parameter.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(col1 = list(c(1, 1, 2), c(2, 3)), col2 = list(c("a", "a", "b"), c("b", "c")))
#'   #' deduplicated_dt <- deduplicate_columns(dt, c("col1", "col2"))
#'   #' print(deduplicated_dt)  # Should print the data.table with deduplicated columns
#'   
#'   for (col in columns) {
#'     dt[, (col) := lapply(get(col), unique)]
#'   }
#'   return(dt)
#' }

deduplicate_and_ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Deduplicate and Ensure Unique Entries Across Columns
  #' @description Deduplicates and ensures unique entries across clin_c1, clin_c2, and clin_icd columns within each row.
  #' @param clin_c1 A list of vectors representing the clin_c1 column.
  #' @param clin_c2 A list of vectors representing the clin_c2 column.
  #' @param clin_icd A list of vectors representing the clin_icd column.
  #' @return A list containing the modified clin_c1, clin_c2, and clin_icd columns.
  #'
  #' @details
  #' This function performs the following operations:
  #' - Deduplicates each specified column within each row.
  #' - Ensures that entries in clin_c1 are not found in clin_c2 or clin_icd within each row.
  #' - Ensures that entries in clin_c2 are not found in clin_c1 or clin_icd within each row.
  #' - Ensures that entries in clin_icd are not found in clin_c1 or clin_c2 within each row.
  #'
  #' @examples
  #' clin_c1 <- list(c("A", "B", "A", "C"), c("C", "D"))
  #' clin_c2 <- list(c("B", "E", "C"), c("D", "F"))
  #' clin_icd <- list(c("A", "G"), c("E", "H"))
  #' result <- deduplicate_and_ensure_unique(clin_c1, clin_c2, clin_icd)
  #' print(result$clin_c1)  # Should print modified clin_c1
  #' print(result$clin_c2)  # Should print modified clin_c2
  #' print(result$clin_icd)  # Should print modified clin_icd
  
  # Deduplicate each column within each row
  clin_c1 <- lapply(clin_c1, unique)
  clin_c2 <- lapply(clin_c2, unique)
  clin_icd <- lapply(clin_icd, unique)
  
  # Ensure unique entries across columns within each row
  unique_clin_icd <- mapply(function(c1, c2, icd) setdiff(icd, union(c1, c2)), clin_c1, clin_c2, clin_icd, SIMPLIFY = FALSE)
  
  # Ensure that clin_c1 and clin_c2 are unique within their columns
  unique_clin_c1 <- mapply(function(c1, c2, icd) setdiff(c1, c2), clin_c1, clin_c2, SIMPLIFY = FALSE)
  unique_clin_c2 <- mapply(function(c1, c2, icd) setdiff(c2, c1), clin_c1, clin_c2, SIMPLIFY = FALSE)
  
  return(list(clin_c1 = unique_clin_c1, clin_c2 = unique_clin_c2, clin_icd = unique_clin_icd))
}

# Helper function to check if a clinical code is an acceptable PDX
check_pdx_code <- function(code, acc_pdx, code_num) {
  #' @title Check PDX Code
  #' @description Checks if a clinical code is an acceptable PDX.
  #' @param code A clinical code to check.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @param code_num The code number to return if the code is acceptable.
  #' @return A list containing the PDX and its code number, or NULL if not acceptable.
  if (!is.null(code) && code %in% acc_pdx) {
    return(list(pdx = code, pdx_code = code_num))
  }
  return(NULL)
}

# Helper function to find PDX from clinical ICD codes
find_pdx_from_icd <- function(clin_icd, acc_pdx) {
  #' @title Find PDX from ICD Codes
  #' @description Finds the PDX from a list of clinical ICD codes.
  #' @param clin_icd A list of clinical ICD codes.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @return A list containing the PDX and its code number, or NULL if not found.
  pdxs <- intersect(clin_icd, acc_pdx)
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }
  return(pdxs)
}

# Helper function to compute similarities between a code and candidates
compute_similarity <- function(code, candidates) {
  #' @title Compute Similarity
  #' @description Computes similarities between a code and a list of candidate codes.
  #' @param code A clinical code to compare.
  #' @param candidates A list of candidate codes to compare against.
  #' @return A numeric vector representing the similarity scores.
  sapply(candidates, function(candidate) {
    sum(substr(code, 1, nchar(candidate)) == substr(candidate, 1, nchar(candidate)))
  })
}

# Helper function to find the most similar PDX
find_most_similar_pdx <- function(code, pdxs) {
  #' @title Find Most Similar PDX
  #' @description Finds the most similar PDX from a list of PDX codes based on a given code.
  #' @param code A clinical code to compare.
  #' @param pdxs A list of PDX codes to compare against.
  #' @return A list containing the most similar PDX and its code number.
  starting_letter <- substr(code, 1, 1)
  starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
  if (length(starting_codes) == 1) {
    return(list(pdx = starting_codes[1], pdx_code = 4))
  } else if (length(starting_codes) > 1) {
    similarities <- compute_similarity(code, starting_codes)
    most_similar_pdx <- starting_codes[which.max(similarities)]
    return(list(pdx = most_similar_pdx, pdx_code = 5))
  }
  return(NULL)
}

# Main function to find the primary diagnosis (PDX)
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  #' @title Find Primary Diagnosis (PDX)
  #' @description Finds the primary diagnosis (PDX) in a given set of clinical codes.
  #' @param clin_c1 The first clinical code.
  #' @param clin_c2 The second clinical code.
  #' @param clin_icd A list of clinical ICD codes.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @return A list containing the PDX and its code number.
  
  clin_icd <- unlist(clin_icd)
  
  # Check if clin_c1 or clin_c2 is an acceptable PDX
  pdx_check <- check_pdx_code(clin_c1, acc_pdx, 1)
  if (!is.null(pdx_check)) return(pdx_check)
  
  pdx_check <- check_pdx_code(clin_c2, acc_pdx, 2)
  if (!is.null(pdx_check)) return(pdx_check)
  
  # Find PDX from clinical ICD codes
  pdxs <- find_pdx_from_icd(clin_icd, acc_pdx)
  if (is.list(pdxs)) return(pdxs)
  
  # Find the most similar PDX based on clin_c1 or clin_c2
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {
      most_similar_pdx <- find_most_similar_pdx(cr, pdxs)
      if (!is.null(most_similar_pdx)) return(most_similar_pdx)
    }
  }
  
  # If no specific match, return a random PDX from the list
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }
  
  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(dt, acc_pdx) {
  #' @title Apply Find PDX to Data Table
  #' @description Applies the find_pdx function to each row of a data.table.
  #' @param dt A data.table to process.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @return The modified data.table with PDX information added.
  #'
  #' @details
  #' This function processes each row in the data.table to identify the primary diagnosis (PDX).
  #' It first attempts to assign PDX based on clin_c1 and clin_c2 columns.
  #' If no PDX is found, it uses the find_pdx function to determine the PDX from the clin_icd column.
  #'
  #' @examples
  #' library(data.table)
  #' dt <- data.table(
  #'   clin_c1 = c("A123", "B456", NA),
  #'   clin_c2 = c(NA, "C789", "D012"),
  #'   clin_icd = list(c("A123", "B456"), c("C789"), c("D012", "E345"))
  #' )
  #' acc_pdx <- c("A123", "C789", "E345")
  #' modified_dt <- apply_find_pdx(dt, acc_pdx)
  #' print(modified_dt)  # Should print the data.table with PDX information added
  
  # Assign PDX based on clin_c1 and clin_c2
  dt[, pdx := ifelse(clin_c1 %in% acc_pdx, clin_c1, ifelse(clin_c2 %in% acc_pdx, clin_c2, NA))]
  dt[, pdx_code := ifelse(clin_c1 %in% acc_pdx, 1, ifelse(clin_c2 %in% acc_pdx, 2, 99))]
  
  # Identify rows without a PDX
  missing_pdx_indices <- which(is.na(dt$pdx))
  
  if (length(missing_pdx_indices) > 0) {
    clin_icd_list <- dt$clin_icd[missing_pdx_indices]
    
    result_list <- lapply(clin_icd_list, function(icd_list) {
      find_pdx(NA, NA, icd_list, acc_pdx)
    })
    
    # Update dt with results
    dt$pdx[missing_pdx_indices] <- sapply(result_list, `[[`, "pdx")
    dt$pdx_code[missing_pdx_indices] <- sapply(result_list, `[[`, "pdx_code")
  }
  
  return(dt)
}

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  #' @title Generate Date of Birth (DOB) Vectorized
  #' @description Generates date of birth (DOB) values vectorized from birthdates, ages, and admission dates.
  #' @param bdays A vector of birthdates.
  #' @param ages A vector of ages.
  #' @param date_adms A vector of admission dates.
  #' @return A vector of generated DOB values.
  require(lubridate)
  
  dob <- rep(NA_character_, length(ages))
  
  # Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(mdy(bdays[valid_bdays_indices]), "%d/%m/%Y")
  
  # Identify indices where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- mdy(date_adms[missing_bday_indices])
  
  # Handle cases where ages are zero
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)
  dob[missing_bday_indices[zero_age_indices]] <- format(ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  # Handle cases where ages are positive
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)
  truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
  dob[missing_bday_indices[positive_age_indices]] <- format(ref_dates[positive_age_indices] - years(truncated_ages) - days(sample(1:170, length(positive_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  return(dob)
}



# Helper function to generate date of birth (DOB) values
generate_dob_column <- function(dt) {
  #' @title Generate DOB Column
  #' @description Generates date of birth (DOB) values for a data.table.
  #' @param dt A data.table containing birthdates, ages, and admission dates.
  #' @return A vector of generated DOB values.
  generate_dob_vectorized(dt$pat_bdate, dt$pat_age, dt$date_adm)
}

# Helper function to format dates
format_dates <- function(date_vector) {
  #' @title Format Dates
  #' @description Formats dates in a vector to "dd/mm/yyyy" format.
  #' @param date_vector A vector of dates to format.
  #' @return A vector of formatted dates.
  format(mdy(date_vector), "%d/%m/%Y")
}

# Helper function to format times
format_times <- function(time_vector) {
  #' @title Format Times
  #' @description Formats times in a vector by removing colons.
  #' @param time_vector A vector of times to format.
  #' @return A vector of formatted times.
  gsub(":", "", time_vector)
}

# Helper function to split ICD codes
split_icd_codes_for_batch_grouper <- function(icd_str) {
  #' @title Split ICD Codes
  #' @description Splits ICD codes into a fixed-length vector.
  #' @param icd_str A string of ICD codes.
  #' @return A fixed-length vector of ICD codes.
  codes <- unlist(icd_str)
  length(codes) <- 12
  codes
}

# Helper function to split RVS codes
split_rvs_codes_for_batch_grouper <- function(rvs_str) {
  #' @title Split RVS Codes
  #' @description Splits RVS codes into a fixed-length vector.
  #' @param rvs_str A string of RVS codes.
  #' @return A fixed-length vector of RVS codes.
  codes <- unlist(rvs_str)
  length(codes) <- 20
  codes
}

# Helper function to prepare and write output data.table
prepare_and_write_output <- function(output_dt, output_txt_file) {
  #' @title Prepare and Write Output
  #' @description Prepares and writes the output data.table to a file.
  #' @param output_dt The data.table to write.
  #' @param output_txt_file The path of the output text file.
  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- '--'
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  # Write the data.table to a file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}

# Main function to export data for batch grouper processing
export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #' @description Exports data for batch grouper processing.
  #' @param dt A data.table to export.
  #' @param year_to_load The year to load for the export.
  #' @param output_txt_file The path of the output text file.
  
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


process_chunk <- function(chunk) {
  #' @title Process Data Chunk
  #' @description Processes a data chunk by cleaning, mapping codes, replacing empty values, and finding the primary diagnosis (PDX).
  #' @param chunk A data.table chunk to process.
  #' @return The processed data.table chunk.
  #'
  #' @details
  #' This function performs the following operations on the data chunk:
  #' - Suppresses console output to keep the environment clean.
  #' - Cleans the data using the clean_data function.
  #' - Maps RVS and ICD-10 codes.
  #' - Replaces empty strings with NA values.
  #' - Finds the primary diagnosis (PDX) using the apply_find_pdx function.
  #'
  #' @examples
  #' library(data.table)
  #' chunk <- data.table(...) # Load your data chunk
  #' processed_chunk <- process_chunk(chunk)
  #' print(processed_chunk) # Should print the processed data chunk
  
  # Suppress output
  sink(tempfile())
  on.exit(sink(), add = TRUE)
  
  # Clean data
  chunk <- clean_data(chunk)
  
  # Map codes
  chunk <- process_rvs_code_mapping(chunk, rvs_icd9)
  chunk <- process_icd10_mapping(chunk)
  
  # Replace empty strings with NA values
  chunk <- replace_empty_with_na(chunk)
  
  # Find PDX
  chunk <- apply_find_pdx(chunk, acc_pdx)
  
  return(chunk)
}
