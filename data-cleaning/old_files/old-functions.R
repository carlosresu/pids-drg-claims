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
#' 
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
#' 
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