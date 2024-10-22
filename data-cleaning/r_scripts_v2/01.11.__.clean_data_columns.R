# Function to clean data by handling missing values and replacing invalid entries
clean_data_columns <- function(data) {
  # Identify and process character columns
  char_cols <- names(data)[sapply(data, is.character)]
  # If running on Unix, apply parallel processing for character columns
  data[, (char_cols) := mclapply(.SD, function(col) {
    # Replace "None" and empty strings with NA in character columns
    col[col %in% c("None", "")] <- NA_character_
    return(col) # Return modified column
  }, mc.cores = nthreads), .SDcols = char_cols]
  # Identify and process numeric columns
  num_cols <- names(data)[sapply(data, is.numeric)]
  data[, (num_cols) := mclapply(.SD, function(col) {
    # Replace NaN values with NA in numeric columns
    col[is.nan(col)] <- NA_real_
    return(col) # Return modified column
  }, mc.cores = nthreads), .SDcols = num_cols]
  # Identify and process list columns
  list_cols <- names(data)[sapply(data, is.list)]
  data[, (list_cols) := mclapply(.SD, function(col) {
    # For each list element, check if it contains characters and replace "None" and empty strings with NA
    lapply(col, function(x) {
      if (is.character(x)) x[x %in% c("None", "")] <- NA_character_
      return(x) # Return modified list element
    })
  }, mc.cores = nthreads), .SDcols = list_cols]
  return(data) # Return the cleaned data.table
}
