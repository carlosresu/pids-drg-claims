# Clean and collapse columns
collapse_columns <- function(cols_to_process) {
  ## Combines multiple string columns into one and cleans the result using clean_column.
  # cols_to_process: a list of columns to concatenate.
  # na_like_strings: strings considered as NA.
  # str(cols_to_process)
  # Clean and split each column in cols_to_process
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_and_split(col)
  })

  # Collapse the cleaned columns into a single column, combining them with "||"
  collapsed_column <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
    # Combine corresponding rows from all columns and remove empty strings or NA-like values
    combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
    combined <- combined[!combined %in% na_like_strings & combined != ""]

    # Collapse the cleaned and combined values using "||" as the final separator
    if (length(combined) > 0) {
      return(paste(combined, collapse = "||"))
    } else {
      return(NA_character_)
    }
  })
  
  # Return the collapsed and cleaned column
  return(collapsed_column)
}
