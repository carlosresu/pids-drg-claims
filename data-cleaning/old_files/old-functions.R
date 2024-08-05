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
#'
#' replace_empty_with_na <- function(dt) {
#'   #' @title Replace Empty Strings with NA
#'   #' @description Replaces empty strings, "NA" strings, and "character(0)" with NA values in character, factor, and list columns of a data.table.
#'   #' @param dt A data.table to process.
#'   #' @return The modified data.table with empty strings, "NA" strings, and "character(0)" replaced by NA values.
#'   #'
#'   #' @details
#'   #' This function processes all character, factor, and list columns in the data.table, replacing empty strings, "NA" strings, and "character(0)" with actual NA values.
#'   #'
#'   #' @examples
#'   #' library(data.table)
#'   #' dt <- data.table(col1 = c("A", "", "C"), col2 = factor(c("X", "", "Z")), col3 = list("NA", "", "B"))
#'   #' dt <- replace_empty_with_na(dt)
#'   #' print(dt)  # Should print modified data.table with NA values
#'
#'   char_factor_cols <- names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col) || is.list(col))]
#'   dt[, (char_factor_cols) := lapply(.SD, function(x) {
#'     x[x == "" | x == "NA" | x == "character(0)"] <- NA_character_
#'     if (is.factor(x)) {
#'       levels(x) <- c(levels(x), NA)
#'     }
#'     return(x)
#'   }), .SDcols = char_factor_cols]
#'   return(dt)
# }

#' append_and_remove_rvs <- function(clin_rvs, col) {
#'   #' @title Append and Remove 5-Digit Codes
#'   #' @description Appends and removes 5-digit numeric codes
#'   #' (i.e. 5-digit RVS procedure codes) from a specified column.
#'   #' @param clin_rvs A list of character vectors representing the clin_rvs column.
#'   #' @param col A character vector representing the column to process.
#'   #' @return A list containing the modified clin_rvs and the modified col.
#'   #'
#'   #' @details
#'   #' This function performs the following operations:
#'   #' - Ensures the clin_rvs column is a list of characters.
#'   #' - Finds and appends 5-digit numeric codes from the specified column to the
#'   #' clin_rvs column.
#'   #' - Removes 5-digit numeric codes from the specified column.
#'   #'
#'   #' @examples
#'   #' clin_rvs <- list(c("A", "B"), c("C", "D"))
#'   #' col <- c("12345 E", "67890 F")
#'   #' result <- append_and_remove_rvs(clin_rvs, col)
#'   #' print(result$clin_rvs)  # Should print modified clin_rvs with 5-digit codes
#'   #' appended
#'   #' print(result$col)  # Should print the modified col with 5-digit codes
#'   #' removed
#'
#'   # Ensure the clin_rvs column is a list of characters
#'   clin_rvs <- lapply(clin_rvs, function(x) if (is.null(x)) character() else x)
#'
#'   # Regular expression to match 5-digit numeric codes
#'   regex_5_digit <- "\\b\\d{5}\\b"
#'
#'   # Find and append 5-digit codes, and remove them from the specified column
#'   modified_clin_rvs <- mapply(find_and_append_codes, clin_rvs, col, MoreArgs = list(regex_5_digit = regex_5_digit), SIMPLIFY = FALSE)
#'   modified_col <- lapply(col, remove_5_digit_codes, regex_5_digit = regex_5_digit)
#'
#'   return(list(clin_rvs = modified_clin_rvs, col = unlist(modified_col)))
#' }
#'

# # Helper function to remove 5-digit codes
# remove_5_digit_codes <- function(col, regex_5_digit) {
#   col <- gsub(regex_5_digit, "", col)
#   return(col)
# }

# # Helper function to find and append 5-digit codes
# find_and_append_codes <- function(clin_rvs, col, regex_5_digit) {
#   codes_to_append <- regmatches(col, gregexpr(regex_5_digit, col))[[1]]
#   clin_rvs <- c(clin_rvs, codes_to_append)
#   return(clin_rvs)
# }

# map_then_compare_icd_mappings <- function(tdrg_icd10, rows_to_show = Inf, invalid_rows_to_show = Inf) {
#   # Ensure the dt variable is in the global environment
#   if (!exists("dt", envir = .GlobalEnv)) {
#     stop("The global variable 'dt' does not exist.")
#   }

#   # Store the original data for comparison
#   original_dt <- data.table::copy(dt)

#   # Process ICD-10 mappings
#   mapped_columns <- implement_icd10_mapping(
#     original_dt$clin_c1, original_dt$clin_c2,
#     original_dt$clin_icd, tdrg_icd10,
#     rows_to_show = rows_to_show
#   )

#   # Update the global dt with mapped columns
#   dt$clin_c1 <- mapped_columns$clin_c1
#   dt$clin_c2 <- mapped_columns$clin_c2
#   dt$clin_icd <- mapped_columns$clin_icd

#   # Ensure unique ICD codes
#   unique_icd_codes <- ensure_unique_icd_codes(
#     dt$clin_c1, dt$clin_c2, dt$clin_icd
#   )
#   dt$clin_c1 <- unique_icd_codes$clin_c1
#   dt$clin_c2 <- unique_icd_codes$clin_c2
#   dt$clin_icd <- unique_icd_codes$clin_icd

#   # Pad lists to ensure they have the same length
#   padded_c1 <- pad_list_elements(original_dt$clin_c1, dt$clin_c1)
#   original_dt$clin_c1 <- padded_c1[[1]]
#   dt$clin_c1 <- padded_c1[[2]]

#   padded_c2 <- pad_list_elements(original_dt$clin_c2, dt$clin_c2)
#   original_dt$clin_c2 <- padded_c2[[1]]
#   dt$clin_c2 <- padded_c2[[2]]

#   padded_icd <- pad_list_elements(original_dt$clin_icd, dt$clin_icd)
#   original_dt$clin_icd <- padded_icd[[1]]
#   dt$clin_icd <- padded_icd[[2]]

#   # Generate comparison table
#   comparison_table <- rbind(
#     generate_comparison_table(original_dt$clin_c1, dt$clin_c1),
#     generate_comparison_table(original_dt$clin_c2, dt$clin_c2),
#     generate_comparison_table(original_dt$clin_icd, dt$clin_icd)
#   )

#   # Sort the comparison table by count in descending order
#   comparison_table <- comparison_table[order(-count)]

#   # Print the kable output with a specified number of rows
#   print(kable(head(comparison_table, rows_to_show),
#     format = "markdown",
#     caption = "Comparison of ICD Codes Before and After Mapping"
#   ))

#   # Check if all resulting ICD codes are in either the Thai library or the PhilHealth library
#   all_icds <- unique(
#     c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd))
#   )
#   valid_icds <- unique(c(tdrg_icd10$CODE, rvs_icd9$icd9cm))
#   invalid_icds <- setdiff(all_icds, valid_icds)
#   invalid_icds <- invalid_icds[!is.na(invalid_icds) & invalid_icds != "NA"]

#   if (length(invalid_icds) > 0) {
#     invalid_icds_table <- data.table(
#       code = invalid_icds,
#       count = sapply(
#         invalid_icds,
#         function(icd) {
#           sum(c(
#             unlist(dt$clin_c1),
#             unlist(dt$clin_c2),
#             unlist(dt$clin_icd)
#           ) == icd, na.rm = TRUE)
#         }
#       )
#     )

#     invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
#     invalid_icds_table <- invalid_icds_table[order(-count)]

#     print(kable(head(invalid_icds_table, invalid_rows_to_show),
#       format = "markdown",
#       caption = "Invalid ICD Codes Not Found in Thai or PhilHealth Libraries"
#     ))
#   } else {
#     cat("All resulting ICD codes are valid and present in the libraries.\n")
#   }
# }
# compute_statistics <- function(dt, rvs_icd9, rvs_map_list) {
#   with_thai <- rvs_icd9[is_thai == TRUE]
#   without_thai <- rvs_icd9[!rvs %in% with_thai$rvs]

#   cat(sprintf(
#     "There are %d RVS codes without an",
#     length(unique(without_thai$rvs))
#   ), "ICD-9CM equivalent recognized by the TDRG ICD9CM\n")

#   rvss <- unique(unlist(dt$clin_rvs))
#   cat(sprintf(
#     "There are %d unique RVS codes that appear in the claims.\n",
#     length(rvss)
#   ))

#   mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
#   cat(sprintf(
#     "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
#     length(mappable_rvs), (length(mappable_rvs) * 100 / length(rvss))
#   ))

#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   cat(sprintf(
#     "Of these, there are %d (%.2f%%) with more than one ICD9",
#     length(multi_mapped_rvs),
#     (length(multi_mapped_rvs) * 100 / length(mappable_rvs))
#   ), "equivalent recognized by the Thai ICD9 library.\n")

#   unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
#   cat(sprintf(
#     "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n",
#     length(unmappable_rvs), (length(unmappable_rvs) * 100 / length(rvss))
#   ))
# }
# read_and_save_partial <- function(start_row, end_row, part_num) {
#   partial_file_path <- full_claims_file(part = part_num, fileext = TRUE)
#   header <- fread(full_claims_file(), nrows = 1, header = TRUE) # Always read the header
#   if (!file_exists(partial_file_path)) {
#     skip_rows <- if (part_num == 1) start_row else start_row - 1
#     dt <- fread(full_claims_file(),
#       na.strings = na_values,
#       colClasses = "character",
#       nrows = end_row - start_row + 1,
#       skip = skip_rows,
#       header = FALSE
#     )
#     setnames(dt, colnames(header))
#     if (nrow(dt) > 0) {
#       print(paste("Saving partial file:", partial_file_path))
#       fwrite(dt, partial_file_path, quote = TRUE)
#     } else {
#       print(paste("No rows to save for part:", part_num))
#     }
#   } else {
#     print(paste("File already exists, skipping creation:", partial_file_path))
#   }
#   if (to_sample) {
#     sampled_file_path <- sampled_claims_file(part_num)
#     if (!file_exists(sampled_file_path)) {
#       print(paste("Creating sampled file:", sampled_file_path))
#       if (exists("dt") && nrow(dt) > 0) {
#         sampled_dt <- dt[sample(.N, min(sample_size, .N))]
#       } else {
#         # Read the partial file again if dt doesn't exist
#         dt <- fread(partial_file_path, na.strings = na_values, colClasses = "character")
#         sampled_dt <- dt[sample(.N, min(sample_size, .N))]
#       }
#       setnames(sampled_dt, colnames(header))
#       fwrite(sampled_dt, sampled_file_path, quote = TRUE)
#     } else {
#       print(paste("Sampled file already exists, skipping creation:", sampled_file_path))
#     }
#   }
# }
# implement_icd10_mapping <- function(
#     clin_c1, clin_c2, clin_icd, tdrg_icd10, rows_to_show = Inf) {
#   icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

#   thai_icd10_env <- create_thai_icd10_environment(
#     unique(tdrg_icd10$CODE)
#   )
#   neoplasms_env <- create_thai_icd10_environment(
#     unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
#   )

#   direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
#   cat(
#     sprintf(
#       "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
#       length(icds), length(direct_match_codes),
#       length(direct_match_codes) * 100 / length(icds)
#     ),
#     " are directly in the Thai ICD-10 library\n"
#   )

#   icd_mapping_info <- generate_icd10_mapping(
#     icds, thai_icd10_env, neoplasms_env
#   )
#   icd_mapping <- icd_mapping_info$icd_mapping
#   modified_count <- icd_mapping_info$modified_count
#   cat(sprintf(
#     "The modifications led to a total of %d",
#     length(icd_mapping)
#   ), " codes being mapped to an equivalent in the Thai ICD10 library.\n")
#   cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

#   unmatched_icds <- setdiff(icds, names(icd_mapping))
#   if (length(unmatched_icds) > 0) {
#     cat(sprintf(
#       "There are %d codes that could not",
#       length(unmatched_icds)
#     ), "be mapped to the Thai ICD10 library:\n")
#     unmatched_sources <- data.table(
#       code = unmatched_icds, source = NA_character_, count = 0
#     )

#     for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
#       col_values <- get(col_name)
#       unmatched_sources[
#         code %in% unlist(col_values),
#         source := col_name
#       ]
#       unmatched_sources[
#         code %in% unlist(col_values),
#         count := count + table(unlist(col_values))[code]
#       ]
#     }

#     unmatched_sources <- unmatched_sources[order(-count)]
#   } else {
#     unmatched_sources <- data.table()
#   }

#   icd10_map <- data.table(
#     phl_icd10 = names(icd_mapping),
#     tdrg_icd10 = unlist(icd_mapping)
#   )
#   fwrite(icd10_map, paste0(
#     "cache/icd10_map_file_",
#     year_to_load, ".csv"
#   ))
#   icd10_env <- list2env(setNames(
#     as.list(icd10_map$tdrg_icd10),
#     icd10_map$phl_icd10
#   ))

#   mapped_columns <- apply_icd10_mapping_to_columns(
#     clin_c1, clin_c2, clin_icd, icd10_env
#   )

#   # Generate comparison table
#   original_data <- list(
#     clin_c1 = clin_c1,
#     clin_c2 = clin_c2, clin_icd = clin_icd
#   )
#   modified_data <- list(
#     clin_c1 = mapped_columns$clin_c1,
#     clin_c2 = mapped_columns$clin_c2,
#     clin_icd = mapped_columns$clin_icd
#   )

#   padded_data <- lapply(
#     names(original_data),
#     function(name) {
#       pad_list_elements(
#         original_data[[name]],
#         modified_data[[name]]
#       )
#     }
#   )

#   comparison_table <- rbind(
#     generate_comparison_table(padded_data[[1]][[1]], padded_data[[1]][[2]]),
#     generate_comparison_table(padded_data[[2]][[1]], padded_data[[2]][[2]]),
#     generate_comparison_table(padded_data[[3]][[1]], padded_data[[3]][[2]])
#   )

#   comparison_table <- comparison_table[order(-count)]

#   # Check if all resulting ICD codes are in either the
#   # Thai library or the PhilHealth library
#   all_icds <- unique(c(
#     unlist(mapped_columns$clin_c1),
#     unlist(mapped_columns$clin_c2), unlist(mapped_columns$clin_icd)
#   ))
#   valid_icds <- unique(c(tdrg_icd10$CODE, rvs_icd9$icd9cm))
#   invalid_icds <- setdiff(all_icds, valid_icds)
#   invalid_icds <- invalid_icds[!is.na(invalid_icds) & invalid_icds != "NA"]

#   if (length(invalid_icds) > 0) {
#     invalid_icds_table <- data.table(
#       code = invalid_icds,
#       count = sapply(
#         invalid_icds,
#         function(icd) {
#           sum(c(
#             unlist(mapped_columns$clin_c1),
#             unlist(mapped_columns$clin_c2),
#             unlist(mapped_columns$clin_icd)
#           ) == icd, na.rm = TRUE)
#         }
#       )
#     )

#     invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
#     invalid_icds_table <- invalid_icds_table[order(-count)]
#   } else {
#     invalid_icds_table <- data.table()
#   }

#   return(list(
#     clin_c1 = mapped_columns$clin_c1,
#     clin_c2 = mapped_columns$clin_c2,
#     clin_icd = mapped_columns$clin_icd,
#     var1 = length(icds),
#     var2 = length(direct_match_codes),
#     var4 = length(icd_mapping),
#     var5 = modified_count,
#     var6 = length(unmatched_icds),
#     var7 = unmatched_sources,
#     comparison_table = comparison_table,
#     invalid_icds_table = invalid_icds_table
#   ))
# }
# generate_comparison_table <- function(original, modified) {
#   original_unlisted <- unlist(original, use.names = FALSE)
#   modified_unlisted <- unlist(modified, use.names = FALSE)

#   comparison <- data.table(
#     old_code = original_unlisted,
#     new_code = modified_unlisted
#   )

#   comparison <- comparison[old_code != new_code,
#     .(count = .N),
#     by = .(old_code, new_code)
#   ]

#   return(comparison)
# }

# pad_list_elements <- function(list1, list2) {
#   max_length <- max(lengths(list1), lengths(list2))

#   pad_with_na <- function(lst, max_length) {
#     lapply(lst, function(x) {
#       if (length(x) < max_length) {
#         x <- c(x, rep(NA, max_length - length(x)))
#       }
#       return(x)
#     })
#   }

#   list1 <- pad_with_na(list1, max_length)
#   list2 <- pad_with_na(list2, max_length)

#   return(list(list1, list2))
# }

# map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
#   split_codes <- split_rvs_codes(rvs_icd9)
#   rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

#   rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
#   icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

#   return(list(icd9_list = icd9_list, rvs_map_list = rvs_maps$rvs_map_list))
# }

# parallelize_and_summarize_data <- function(
#     dt, num_cores, to_view_checks, global_seed, intermediate_rows_to_show,
#     rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize) {
#   chunk_size <- ceiling(nrow(dt) / num_cores)
#   chunks <- split(dt, rep(1:num_cores,
#     each = chunk_size, length.out = nrow(dt)
#   ))

#   if (to_parallelize) {
#     # Plan for parallel processing
#     plan(multisession, workers = num_cores)

#     # Process each chunk in parallel
#     parallel_results <- future_lapply(
#       chunks, process_chunk,
#       to_view_checks = to_view_checks,
#       rvs_icd9 = rvs_icd9,
#       tdrg_icd10 = tdrg_icd10,
#       acc_pdx = acc_pdx,
#       future.seed = global_seed
#     )
#   } else {
#     # Process each chunk sequentially
#     parallel_results <- lapply(
#       chunks, process_chunk,
#       to_view_checks = to_view_checks,
#       rvs_icd9 = rvs_icd9,
#       tdrg_icd10 = tdrg_icd10,
#       acc_pdx = acc_pdx
#     )
#   }

#   # Combine processed chunks
#   processed_chunks <- lapply(parallel_results, function(res) res$chunk)
#   dt <- rbindlist(processed_chunks)

#   # Combine summaries of the chunks
#   combined_summary <- combine_chunk_summaries(parallel_results, intermediate_rows_to_show)

#   return(list(
#     dt = dt,
#     combined_summary = combined_summary
#   ))
# }
# print_summary_tables <- function(final_combined_summaries, rows_to_show) {
#   # Print the consolidated summary
#   cat("Rename Success:\n", final_combined_summaries$final_rename_success, "\n\n")

#   if (nrow(final_combined_summaries$final_ICD_replacements_1) > 0) {
#     print(kable(head(final_combined_summaries$final_ICD_replacements_1, rows_to_show),
#       format = "markdown",
#       caption = "ICD Replacements 1"
#     ))
#   } else {
#     cat("\nNo ICD replacements found in the first set.\n\n")
#   }

#   if (nrow(final_combined_summaries$final_ICD_replacements_2) > 0) {
#     print(kable(head(final_combined_summaries$final_ICD_replacements_2, rows_to_show),
#       format = "markdown",
#       caption = "ICD Replacements 2"
#     ))
#   } else {
#     cat("\nNo ICD replacements found in the second set.\n\n")
#   }

#   if (is.null(final_combined_summaries$final_pat_type_unmapped)) {
#     cat("Patient Type Unmapped: NULL\n\n")
#   } else {
#     cat(
#       "Patient Type Unmapped:\n",
#       final_combined_summaries$final_pat_type_unmapped, "\n\n"
#     )
#   }

#   if (is.null(final_combined_summaries$final_memcat_parent_unmapped)) {
#     cat("Memcat Parent Unmapped: NULL\n\n")
#   } else {
#     cat(
#       "Memcat Parent Unmapped:\n",
#       final_combined_summaries$final_memcat_parent_unmapped, "\n\n"
#     )
#   }

#   if (is.null(final_combined_summaries$final_memcat_child_unmapped)) {
#     cat("Memcat Child Unmapped: NULL\n\n")
#   } else {
#     cat(
#       "Memcat Child Unmapped:\n",
#       final_combined_summaries$final_memcat_child_unmapped, "\n\n"
#     )
#   }

#   if (is.null(final_combined_summaries$final_discharge_unmapped)) {
#     cat("Discharge Unmapped: NULL\n\n")
#   } else {
#     cat(
#       "Discharge Unmapped:\n",
#       final_combined_summaries$final_discharge_unmapped, "\n\n"
#     )
#   }

#   if (nrow(final_combined_summaries$final_discard_rvs_one) > 0) {
#     print(kable(head(final_combined_summaries$final_discard_rvs_one, rows_to_show),
#       format = "markdown",
#       caption = "Discarded RVS Codes One"
#     ))
#   } else {
#     cat("\nNo RVS codes discarded in the first set.\n\n")
#   }

#   if (nrow(final_combined_summaries$final_discard_rvs_two) > 0) {
#     print(kable(head(final_combined_summaries$final_discard_rvs_two, rows_to_show),
#       format = "markdown",
#       caption = "Discarded RVS Codes Two"
#     ))
#   } else {
#     cat("\nNo RVS codes discarded in the second set.\n\n")
#   }

#   if (nrow(final_combined_summaries$final_empty_strings_replaced_1) > 0) {
#     print(kable(
#       head(
#         final_combined_summaries$final_empty_strings_replaced_1, rows_to_show
#       ),
#       format = "markdown",
#       caption = "Empty Strings Replaced (First Set)"
#     ))
#   } else {
#     cat("\nNo empty strings replaced in the first set.\n\n")
#   }

#   if (nrow(final_combined_summaries$final_empty_strings_replaced_2) > 0) {
#     print(kable(
#       head(
#         final_combined_summaries$final_empty_strings_replaced_2, rows_to_show
#       ),
#       format = "markdown",
#       caption = "Empty Strings Replaced (Second Set)"
#     ))
#   } else {
#     cat("\nNo empty strings replaced in the second set.\n\n")
#   }

#   cat(
#     sprintf(
#       "There are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
#       final_combined_summaries$final_unique_icds,
#       final_combined_summaries$final_direct_matches,
#       (final_combined_summaries$final_direct_matches /
#       final_combined_summaries$final_unique_icds) * 100
#     ), "are directly in the Thai ICD-10 library.\n"
#   )

#   cat(
#     sprintf(
#       "The modifications led to a total of %d codes",
#       final_combined_summaries$final_unique_icds -
#       final_combined_summaries$final_unmatched
#     ), "being mapped to an equivalent in the Thai ICD10 library.\n"
#   )

#   cat(
#     sprintf(
#       "Out of these, %d were modified to match.\n",
#       final_combined_summaries$final_unique_icds -
#       final_combined_summaries$final_unmatched -
#       final_combined_summaries$final_direct_matches
#     )
#   )

#   cat(
#     sprintf(
#       "There are %d codes that could not",
#       final_combined_summaries$final_unmatched
#     ), "be mapped to the Thai ICD10 library.\n"
#   )


#   if (nrow(final_combined_summaries$final_unmatched_sources) > 0) {
#     print(
#       kable(
#         head(
#           final_combined_summaries$final_unmatched_sources,
#           rows_to_show),
#       format = "markdown",
#       caption = "Invalid ICD-10 Codes Not Found in Thai Library"
#     ))
#   } else {
#     cat("\nAll resulting ICD-10 codes are valid and present in the Thai library.\n\n")
#   }
# }
# combine_summaries <- function(summaries, intermediate_rows_to_show) {
#   combined_summary <- list(
#     rename_success = all(unlist(sapply(summaries, function(summary) summary$rename_success)), na.rm = TRUE),
#     ICD_replacements_1 = combine_comparison_tables(summaries, "ICD_replacements_1", intermediate_rows_to_show),
#     ICD_replacements_2 = combine_comparison_tables(summaries, "ICD_replacements_2", intermediate_rows_to_show),
#     pat_type_unmapped = unique(unlist(lapply(summaries, function(summary) summary$pat_type_unmapped))),
#     memcat_parent_unmapped = unique(unlist(lapply(summaries, function(summary) summary$memcat_parent_unmapped))),
#     memcat_child_unmapped = unique(unlist(lapply(summaries, function(summary) summary$memcat_child_unmapped))),
#     discharge_unmapped = unique(unlist(lapply(summaries, function(summary) summary$discharge_unmapped))),
#     discard_rvs_one = combine_discarded_rvs_tables(summaries, "discard_rvs_one", intermediate_rows_to_show),
#     discard_rvs_two = combine_discarded_rvs_tables(summaries, "discard_rvs_two", intermediate_rows_to_show),
#     empty_strings_replaced_1 = combine_replace_empty_tables(summaries, "empty_strings_replaced_1", intermediate_rows_to_show),
#     empty_strings_replaced_2 = combine_replace_empty_tables(summaries, "empty_strings_replaced_2", intermediate_rows_to_show),
#     unique_icds_count = unique(unlist(lapply(summaries, function(summary) summary$unique_icds))),
#     direct_matches_count = unique(unlist(lapply(summaries, function(summary) summary$direct_matches))),
#     unmatched_count = unique(unlist(lapply(summaries, function(summary) summary$unmatched))),
#     unmatched_sources = combine_unmatched_icd10_codes(summaries, "unmatched_sources", intermediate_rows_to_show)
#   )

#   return(combined_summary)
# }

# combine_parts_summaries <- function(combined_summary, rows_to_show) {
#   final_combined_summaries <- list(
#     final_rename_success = all(unlist(sapply(
#       combined_summary,
#       function(summary) summary$rename_success
#     )), na.rm = TRUE),
#     final_ICD_replacements_1 = combine_comparison_tables(
#       combined_summary, "ICD_replacements_1", rows_to_show
#     ),
#     final_ICD_replacements_2 = combine_comparison_tables(
#       combined_summary, "ICD_replacements_2", rows_to_show
#     ),
#     final_pat_type_unmapped = unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$pat_type_unmapped
#     ))),
#     final_memcat_parent_unmapped = unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$memcat_parent_unmapped
#     ))),
#     final_memcat_child_unmapped = unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$memcat_child_unmapped
#     ))),
#     final_discharge_unmapped = unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$discharge_unmapped
#     ))),
#     final_discard_rvs_one = combine_discarded_rvs_tables(
#       combined_summary, "discard_rvs_one", rows_to_show
#     ),
#     final_discard_rvs_two = combine_discarded_rvs_tables(
#       combined_summary, "discard_rvs_two", rows_to_show
#     ),
#     final_empty_strings_replaced_1 = combine_replace_empty_tables(
#       combined_summary, "empty_strings_replaced_1", rows_to_show
#     ),
#     final_empty_strings_replaced_2 = combine_replace_empty_tables(
#       combined_summary, "empty_strings_replaced_2", rows_to_show
#     ),
#     final_unique_icds = length(unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$unique_icds_count
#     )))),
#     final_direct_matches = length(unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$direct_matches_count
#     )))),
#     final_unmatched = length(unique(unlist(lapply(
#       combined_summary,
#       function(summary) summary$unmatched_count
#     )))),
#     final_unmatched_sources = combine_unmatched_icd10_codes(
#       combined_summary, "unmatched_sources", rows_to_show)
#   )

#   return(final_combined_summaries)
# }

# library(foreach)
# library(doParallel)
# library(parallel)

# library(rprojroot)
# library(conflicted)
# library(tidyverse)
# process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
#   #' @title Process a chunk of data
#   #'
#   #' @description This function processes a chunk of data by cleaning it,
#   #' mapping RVS and ICD codes, replacing empty strings, and finding PDX codes.
#   #'
#   #' @param chunk data.table. The chunk of data to be processed.
#   #' @param to_view_checks logical. Whether to view checks and print statements.
#   #' @param rvs_icd9 data.table. The RVS to ICD-9 mapping data.
#   #' @param tdrg_icd10 data.table. The Thai DRG ICD-10 mapping data.
#   #' @param acc_pdx character. A vector of acceptable PDX codes.
#   #'
#   #' @return list. A list containing the processed chunk and a summary of the processing.

#   if (to_view_checks) {
#     # print("Viewing checks")
#   } else {
#     sink(tempfile())
#     on.exit(sink(), add = TRUE)
#   }

#   clean_result <- clean_data(chunk)
#   chunk <- clean_result$data

#   summary <- list(
#     rename_success = clean_result$rename_success,
#     ICD_replacements_1 = clean_result$ICD_replacements_1,
#     ICD_replacements_2 = clean_result$ICD_replacements_2,
#     pat_type_unmapped = clean_result$pat_type_unmapped,
#     memcat_parent_unmapped = clean_result$memcat_parent_unmapped,
#     memcat_child_unmapped = clean_result$memcat_child_unmapped,
#     discharge_unmapped = clean_result$discharge_unmapped,
#     discard_rvs_one = clean_result$discard_rvs_one,
#     discard_rvs_two = clean_result$discard_rvs_two,
#     empty_strings_replaced_1 = clean_result$empty_strings_replaced_1
#   )

#   rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
#   chunk[, icd9_list := rvs_mapping_result$icd9_list]

#   clin_c1 <- chunk$clin_c1
#   clin_c2 <- chunk$clin_c2
#   clin_icd <- chunk$clin_icd

#   icd10_mapping_result <- implement_icd10_mapping(
#     clin_c1, clin_c2, clin_icd, tdrg_icd10
#   )
#   chunk[, clin_c1 := icd10_mapping_result$clin_c1]
#   chunk[, clin_c2 := icd10_mapping_result$clin_c2]
#   chunk[, clin_icd := icd10_mapping_result$clin_icd]

#   chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
#   chunk <- chunk_replace_result$data
#   summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

#   pdx_result <- apply_find_pdx(
#     chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
#   )
#   chunk$pdx <- pdx_result$pdx
#   chunk$pdx_code <- pdx_result$pdx_code

#   # Ensure consistent lengths of clin_rvs and icd9_list
#   clin_rvs_len <- lengths(chunk$clin_rvs)
#   icd9_list_len <- lengths(chunk$icd9_list)

#   max_len <- max(c(clin_rvs_len, icd9_list_len))
#   chunk$clin_rvs <- lapply(chunk$clin_rvs, function(x) {
#     length(x) <- max_len
#     x
#   })
#   chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
#     length(x) <- max_len
#     x
#   })

#   summary$unique_icds <- icd10_mapping_result$unique_icds
#   summary$direct_matches <- icd10_mapping_result$direct_matches
#   summary$unmatched <- icd10_mapping_result$unmatched
#   summary$unmatched_sources <- icd10_mapping_result$unmatched_sources

#   summary$rvss <- rvs_mapping_result$rvss
#   summary$mappable_rvs <- rvs_mapping_result$mappable_rvs
#   summary$unmappable_rvs <- rvs_mapping_result$unmappable_rvs
#   summary$multi_mapped_rvs <- rvs_mapping_result$multi_mapped_rvs
#   summary$without_drg <- rvs_mapping_result$without_drg

#   return(list(chunk = chunk, summary = summary))
# }

# clean_data <- function(dt) {
#   #' @title Clean and preprocess the data table
#   #'
#   #' @description This function performs various cleaning and
#   #' preprocessing steps on the input data table, including
#   #' renaming columns, collapsing columns, cleaning specific columns,
#   #' and remapping certain categorical variables.
#   #'
#   #' @param dt data.table. The data table to be cleaned and preprocessed.
#   #'
#   #' @return list. A list containing the cleaned data table and
#   #' various summary information.

#   # Add year column
#   dt[, SRC_YR := as.integer(year_to_load)]

#   # Rename columns
#   setnames(dt, old = old_colnames, new = new_colnames)

#   # Check if all columns were successfully renamed
#   if (!all(new_colnames %in% colnames(dt))) {
#     rename_success <- FALSE
#   } else {
#     rename_success <- TRUE
#   }

#   # Collapse columns clin_icd1 to clin_icd12 into clin_icd
#   dt[, clin_icd := collapse_columns(
#     mget(paste0("clin_icd", 1:12), envir = as.environment(dt)),
#     na_like_strings
#   )]
#   dt[, paste0("clin_icd", 1:12) := NULL]

#   # Collapse columns clin_rvs1 to clin_rvs20 into clin_rvs
#   dt[, clin_rvs := collapse_columns(
#     mget(paste0("clin_rvs", 1:20), envir = as.environment(dt)),
#     na_like_strings
#   )]
#   dt[, paste0("clin_rvs", 1:20) := NULL]

#   # Remove lumped ICD codes from clin_icd
#   dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]

#   # Turn clin_icd and clin_rvs into lists
#   dt[, clin_icd := split_to_vector(clin_icd)]
#   dt[, clin_rvs := split_to_vector(clin_rvs)]

#   # Ensure clean_column function and na_like_strings are
#   # correctly defined and applied
#   dt[, clin_c1_orig := dt$clin_c1]
#   dt[, clin_c1 := clean_column(clin_c1, na_like_strings)] # Clean clin_c1

#   # Generate cleaning comparison table
#   clin_c1_cleaning_comparison <- dt[
#     clin_c1 != clin_c1_orig,
#     .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
#     by = .(clin_c1_orig, clin_c1)
#   ]

#   # Optionally remove clin_c1_orig from dt if no longer needed
#   dt[, clin_c1_orig := NULL]

#   dt[, clin_c2_orig := dt$clin_c2] # Capture original clin_c2

#   # Clean clin_c2 within the data.table context
#   dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]

#   # Create cleaning comparison table
#   clin_c2_cleaning_comparison <- dt[
#     clin_c2 != clin_c2_orig, # Compare cleaned clin_c2 with original
#     .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
#     by = .(clin_c2_orig, clin_c2)
#   ]

#   # Optionally remove clin_c2_orig from dt if no longer needed
#   dt[, clin_c2_orig := NULL]

#   dt[, clin_c1 := remove_lumped_icd_codes(dt$clin_c1)]
#   dt[, clin_c2 := remove_lumped_icd_codes(dt$clin_c2)]

#   dt[, clin_c1 := split_to_vector(clin_c1)]
#   clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
#     dt$clin_icd, dt$clin_c1
#   )
#   dt[, clin_icd := clin_c1_result$clin_icd]
#   dt[, clin_c1 := clin_c1_result$col_first]

#   dt[, clin_c2 := split_to_vector(clin_c2)]
#   clin_c2_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c2)
#   dt[, clin_icd := clin_c2_result$clin_icd]
#   dt[, clin_c2 := clin_c2_result$col_first]

#   clin_c1_rvs_results <- append_and_remove_rvs(
#     dt$clin_rvs, dt$clin_c1, rvs_icd9
#   )
#   dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
#   dt[, clin_c1 := clin_c1_rvs_results$col]
#   clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

#   clin_c2_rvs_results <- append_and_remove_rvs(
#     dt$clin_rvs, dt$clin_c2, rvs_icd9
#   )
#   dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
#   dt[, clin_c2 := clin_c2_rvs_results$col]
#   clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

#   dt[, clin_rvs := lapply(clin_rvs, unique)]
#   dedup_result <- ensure_unique_icd_codes(
#     dt$clin_c1, dt$clin_c2, dt$clin_icd
#   )
#   dt[, clin_c1 := dedup_result$clin_c1]
#   dt[, clin_c2 := dedup_result$clin_c2]
#   dt[, clin_icd := dedup_result$clin_icd]

#   # Replace empty strings in character and factor columns with NA
#   replace_result <- replace_empty_with_na(dt, to_view_checks)
#   dt <- replace_result$data
#   empty_strings_replaced_1 <- replace_result$replacement_summary

#   pat_unmap <- NULL
#   parent_unmap <- NULL
#   child_unmap <- NULL
#   discharge_unmap <- NULL

#   warning_thrown <- FALSE

#   # Remap and check for patient type
#   result <- remap_patient_type(dt$pat_type)
#   dt$pat_type <- result$remapped
#   if (length(result$unmapped) > 0 && to_view_checks) {
#     warning_thrown <- TRUE
#     pat_unmap <- result$unmapped
#   }

#   warning_thrown <- FALSE

#   # Remap and check for member category parent
#   result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
#   dt$pat_memcat_parent <- result$remapped
#   if (length(result$unmapped) > 0 && to_view_checks) {
#     warning_thrown <- TRUE
#     parent_unmap <- result$unmapped
#   }

#   warning_thrown <- FALSE

#   # Remap and check for member category child
#   result <- remap_memcat_child_desc(dt$pat_memcat_child)
#   dt$pat_memcat_child <- result$remapped
#   if (length(result$unmapped) > 0 && to_view_checks) {
#     warning_thrown <- TRUE
#     child_unmap <- result$unmapped
#   }

#   warning_thrown <- FALSE

#   # Remap and check for clinical discharge disposition
#   result <- remap_disposition(dt$clin_discharge)
#   dt$clin_discharge <- result$remapped
#   if (length(result$unmapped) > 0 && to_view_checks) {
#     warning_thrown <- TRUE
#     discharge_unmap <- result$unmapped
#   }

#   return(list(
#     data = dt,
#     rename_success = rename_success,
#     ICD_replacements_1 = clin_c1_cleaning_comparison,
#     ICD_replacements_2 = clin_c2_cleaning_comparison,
#     pat_type_unmapped = pat_unmap,
#     memcat_parent_unmapped = parent_unmap,
#     memcat_child_unmapped = child_unmap,
#     discharge_unmapped = discharge_unmap,
#     discard_rvs_one = clin_c1_discarded_rvs,
#     discard_rvs_two = clin_c2_discarded_rvs,
#     empty_strings_replaced_1 = empty_strings_replaced_1
#   ))
# }
### START OF LAST KNOWN WORKING RVS CODE ###
# # Function to split RVS codes
# split_rvs_codes <- function(rvs_icd9) {
#   #' @title Split RVS Codes
#   #'
#   #' @description This function splits RVS codes into
#   #' those with and without DRG.
#   #'
#   #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
#   #'
#   #' @return list. A list containing RVS codes with DRG and without DRG.

#   with_drg <- rvs_icd9[is_drg == TRUE]
#   without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
#   return(list(with_drg = with_drg, without_drg = without_drg))
# }

# # Function to create RVS map lists
# create_rvs_map_lists <- function(with_drg) {
#   #' @title Create RVS Map Lists
#   #'
#   #' @description This function creates lists for RVS mapping with DRG.
#   #'
#   #' @param with_drg data.table. The RVS codes with DRG.
#   #'
#   #' @return list. A list containing RVS map list and RVS map solo.

#   # Order the data by rvs and -is_drg
#   setorder(with_drg, rvs, -is_drg)

#   # Create a list of unique rvs
#   unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

#   # Split into solo and list mappings
#   solo <- unique_rvs[lengths(icd9cm_list) == 1]
#   list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

#   rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
#   rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

#   return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
# }

# # Function to get ICD-9 codes from clinical RVS
# get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
#   #' @title Get ICD-9 Codes from Clinical RVS
#   #'
#   #' @description This function retrieves ICD-9 codes for clinical RVS.
#   #'
#   #' @param clin_rvs list. The clinical RVS codes.
#   #' @param rvs_map_solo_env environment. The environment
#   #' with RVS map solo codes.
#   #'
#   #' @return list. The ICD-9 codes for clinical RVS.

#   lapply(clin_rvs, function(x) {
#     codes <- unlist(x)
#     mappable <- codes[
#       !is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))
#     ]
#     if (length(mappable) > 0) {
#       unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
#     } else {
#       NA_character_
#     }
#   })
# }

# # Function to map RVS to ICD-9
# map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
#   #' @title Map RVS to ICD-9
#   #'
#   #' @description This function maps RVS codes to ICD-9 codes.
#   #'
#   #' @param clin_rvs list. The clinical RVS codes.
#   #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
#   #'
#   #' @return list. A list containing the
#   #' mapped ICD-9 codes and related information.

#   split_codes <- split_rvs_codes(rvs_icd9)
#   rvs_maps <- create_rvs_map_lists(split_codes$with_drg)
#   rvs_map_list <- rvs_maps$rvs_map_list

#   rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
#   icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

#   rvss <- unique(unlist(clin_rvs))
#   mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
#   unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
#   multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
#   without_drg <- unique(rvs_icd9[!rvs %in% names(rvs_map_list)]$rvs)

#   return_list <- list(
#     icd9_list = icd9_list,
#     rvs_map_list = rvs_maps$rvs_map_list,
#     rvss = rvss,
#     mappable_rvs = mappable_rvs,
#     unmappable_rvs = unmappable_rvs,
#     multi_mapped_rvs = multi_mapped_rvs,
#     without_drg = without_drg
#   )

#   return(return_list)
# }

# # Function to find and append valid RVS codes
# find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
#   #' @title Find and Append Valid RVS Codes
#   #'
#   #' @description This function finds and appends valid
#   #' RVS codes to the clinical RVS.
#   #'
#   #' @param datatable data.table. The data table with clinical RVS codes.
#   #' @param valid_rvs_codes character. The valid RVS codes.
#   #'
#   #' @return None. The function modifies the input data.table in place.

#   regex_5_digit <- "\\b\\d{5}\\b"
#   valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
#   for (code in valid_rvs_codes) {
#     assign(code, TRUE, envir = valid_rvs_env)
#   }

#   datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]

#   # Use lapply for improved performance
#   datatable[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]

#   datatable[, clin_rvs := mapply(
#     function(rvs, matches) unique(c(rvs, matches)),
#     clin_rvs, valid_matches,
#     SIMPLIFY = FALSE
#   )]
# }

# # Function to remove 5-digit codes
# remove_5_digit_codes <- function(col) {
#   #' @title Remove 5-Digit Codes
#   #'
#   #' @description This function removes 5-digit codes from
#   #' the given column.
#   #'
#   #' @param col character. The column to be processed.
#   #'
#   #' @return list. The column with 5-digit codes removed.

#   regex_5_digit <- "\\b\\d{5}\\b"
#   lapply(col, function(x) gsub(regex_5_digit, "", x))
# }

# # Function to warn about invalid RVS codes
# warn_invalid_rvs <- function(matches, valid_rvs_codes) {
#   #' @title Warn About Invalid RVS Codes
#   #'
#   #' @description This function warns about invalid RVS codes
#   #' and creates a table of discarded codes.
#   #'
#   #' @param matches list. The matched RVS codes.
#   #' @param valid_rvs_codes character. The valid RVS codes.
#   #'
#   #' @return data.table. A table of discarded codes.

#   valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
#   for (code in valid_rvs_codes) {
#     assign(code, TRUE, envir = valid_rvs_env)
#   }

#   invalid_matches <- lapply(
#     matches,
#     function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
#   )
#   discarded_codes <- unlist(invalid_matches)
#   if (length(discarded_codes) > 0) {
#     discarded_table <- data.table(
#       CODE = discarded_codes
#     )[, .N, by = CODE][order(-N)]
#     setnames(discarded_table, c("CODE", "count"))
#   } else {
#     discarded_table <- data.table()
#   }
#   return(discarded_table)
# }

# # Function to append and remove RVS codes
# append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
#   #' @title Append and Remove RVS Codes
#   #'
#   #' @description This function appends valid RVS codes and
#   #' removes invalid RVS codes.
#   #'
#   #' @param clin_rvs list. The clinical RVS codes.
#   #' @param col character. The column with RVS codes.
#   #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
#   #'
#   #' @return list. A list containing the updated clinical RVS,
#   #' column, and discarded RVS codes.

#   datatable <- data.table(clin_rvs = clin_rvs, col = col)
#   valid_rvs_codes <- rvs_icd9$rvs

#   find_and_append_valid_rvs(datatable, valid_rvs_codes)
#   datatable[, col := remove_5_digit_codes(col)]
#   discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

#   return(
#     list(
#       clin_rvs = datatable$clin_rvs,
#       col = datatable$col,
#       discarded_rvs = discarded_rvs
#     )
#   )
# }

# ### END OF LAST KNOWN WORKING RVS CODE ###

# read_part <- function(part) {
#   #' @title Read and process a part of the data
#   #'
#   #' @description This function reads and processes a part
#   #' of the data from a file.
#   #'
#   #' @param part integer. The part number of the file to read.
#   #'
#   #' @return data.table. The processed part of the data.

#   chunk_file <- if (to_sample) {
#     sampled_claims_file(part)
#   } else {
#     full_claims_file(part)
#   }

#   if (!file.exists(chunk_file)) {
#     stop(paste("File does not exist:", chunk_file))
#   }

#   dt <- fread(chunk_file, na.strings = na_values, colClasses = col_classes)

#   if (to_sample) {
#     dt <- handle_sampling(dt, part)
#   }

#   return(dt)
# }

# # Function to handle sampling
# handle_sampling <- function(dt = NULL, part) {
#   #' @title Handle Sampling
#   #'
#   #' @description This function handles the sampling of data.
#   #' If a sampled file exists, it reads the file and checks if
#   #' the number of rows matches the sample size. If not, it resamples the data.
#   #'
#   #' @param dt data.table. The data table to be sampled.
#   #' Default is NULL.
#   #' @param part integer. The part number of the file.
#   #'
#   #' @return data.table. The sampled data table.

#   sampled_file <- sampled_claims_file(part)

#   if (file.exists(sampled_file)) {
#     dt <- read_sampled_file(sampled_file)
#     if (nrow(dt) != sample_size) {
#       dt <- resample_data(part)
#     }
#   } else {
#     dt <- resample_data(part)
#   }
#   return(dt)
# }

# # Function to resample data
# resample_data <- function(part) {
#   #' @title Resample Data
#   #'
#   #' @description This function resamples the data from the full
#   #' claims file and writes the sampled data to a new file.
#   #'
#   #' @param part integer. The part number of the file.
#   #'
#   #' @return data.table. The resampled data table.

#   dt <- read_entire_file(full_claims_file(part),
#     initial_read = is_partial_file(part)
#   )
#   dt <- sample_data(dt)
#   if (to_write) {
#     fwrite(dt, sampled_claims_file(part))
#   }
#   return(dt)
# }

# # Function to read the full file with all columns
# read_entire_file <- function(file, initial_read = TRUE) {
#   #' @title Read Entire File
#   #'
#   #' @description This function reads the entire file with all
#   #' columns. It handles the initial read to get the header and
#   #' then reads the data based on the header.
#   #'
#   #' @param file character. The file path to read.
#   #' @param initial_read logical. Whether this is the initial
#   #' read to get the header. Default is TRUE.
#   #'
#   #' @return data.table. The data table read from the file.

#   header <- fread(file, nrows = 1, header = TRUE)
#   if (initial_read) {
#     dt <- fread(file,
#       na.strings = na_values,
#       colClasses = "character", header = FALSE, skip = 1
#     )
#     setnames(dt, names(header))
#   } else {
#     dt <- fread(file,
#       na.strings = na_values,
#       colClasses = "character", header = FALSE, skip = 1
#     )
#     setnames(dt, names(header))
#     dt <- dt[, (drop_cols) := NULL]
#     for (col in names(col_classes)) {
#       dt[[col]] <- switch(col_classes[[col]],
#         "character" = as.character(dt[[col]]),
#         "factor" = as.factor(dt[[col]]),
#         "integer" = as.integer(dt[[col]]),
#         "numeric" = as.numeric(dt[[col]]),
#         dt[[col]]
#       )
#     }
#   }
#   return(dt)
# }

# # Function to read the sampled file
# read_sampled_file <- function(file) {
#   #' @title Read Sampled File
#   #'
#   #' @description This function reads the sampled file with the
#   #' header and handles the data based on the specified column classes.
#   #'
#   #' @param file character. The file path to read.
#   #'
#   #' @return data.table. The data table read from the sampled file.

#   header <- fread(file, nrows = 1)
#   dt <- fread(file,
#     na.strings = na_values,
#     colClasses = "character", header = FALSE, skip = 1
#   )
#   setnames(dt, names(header))
#   dt <- dt[, (drop_cols) := NULL]
#   for (col in names(col_classes)) {
#     dt[[col]] <- switch(col_classes[[col]],
#       "character" = as.character(dt[[col]]),
#       "factor" = as.factor(dt[[col]]),
#       "integer" = as.integer(dt[[col]]),
#       "numeric" = as.numeric(dt[[col]]),
#       dt[[col]]
#     )
#   }
#   return(dt)
# }

# # Function to sample data
# sample_data <- function(dt) {
#   #' @title Sample Data
#   #'
#   #' @description This function samples data from the input data table.
#   #'
#   #' @param dt data.table. The data table to be sampled.
#   #'
#   #' @return data.table. The sampled data table.

#   sampled_dt <- dt[sample(.N, min(sample_size, .N))]
#   return(sampled_dt)
# }

# # Main function to read and process chunks
# main_read_function <- function(file = NA) {
#   #' @title Main Read Function
#   #'
#   #' @description This function reads and processes chunks of data from a file.
#   #'
#   #' @param file character. The file path to read.
#   #' Default is NA.
#   #'
#   #' @return data.table. The processed data table.

#   if (is.na(file)) {
#     if (to_read) {
#       file <- full_claims_file(part)
#       dt <- read_entire_file(file, initial_read = is_partial_file(part))

#       if (to_sample) {
#         dt <- handle_sampling(dt, part)
#       }
#     } else if (to_sample) {
#       dt <- handle_sampling()
#     } else {
#       stop("Cannot proceed: to_read is FALSE and to_sample is FALSE.
#       At least one must be TRUE.")
#     }
#   } else {
#     dt <- read_entire_file(file, initial_read = is_partial_file(part))
#   }

#   return(dt)
# }

# combine_comparison_tables <- function(
#     summaries, comparison_field, tmp_nrow = 10) {
#   #' @title Combine Comparison Tables
#   #'
#   #' @description This function combines comparison tables from
#   #' multiple summaries into one.
#   #'
#   #' @param summaries list. A list of summary tables.
#   #' @param comparison_field character. The field in the summaries to compare.
#   #' @param tmp_nrow integer. The number of
#   #' rows to show in the intermediate summary.
#   #'
#   #' @return data.table. The combined comparison table.

#   comparison_list <- lapply(summaries, function(summary) {
#     summary_data <- summary[[comparison_field]]
#     if (!is.null(summary_data) && nrow(summary_data) > 0) {
#       summary_data <- summary_data[, .(old_code, new_code, count)]
#     }
#     return(summary_data)
#   })

#   combined_comparison <- rbindlist(comparison_list, fill = TRUE)

#   if (nrow(combined_comparison) == 0) {
#     return(data.table(
#       old_code = character(),
#       new_code = character(),
#       count = integer()
#     ))
#   }

#   combined_comparison <- combined_comparison[,
#     .(count = sum(count, na.rm = TRUE)),
#     by = .(old_code, new_code)
#   ]
#   combined_comparison <- combined_comparison[order(-count)]
#   combined_comparison <- head(combined_comparison, tmp_nrow)

#   return(combined_comparison)
# }