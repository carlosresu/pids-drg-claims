# Function to check and display unmatched ICD codes
check_unmatched_icd_codes <- function(master_dt, icd_mapping) {
  # Create an empty list to store unmatched codes
  unmatched_list <- list()

  # Define the columns to check
  columns_to_check <- c("c1", "c2", "clin_icd")

  # Loop through each column and check for unmatched codes
  for (col in columns_to_check) {
    # Flatten the list column and unlist it for vectorized operations
    flattened_column <- unlist(
      master_dt[[col]],
      recursive = TRUE,
      use.names = FALSE
    )

    # Remove both NA values and the literal "NA" strings
    flattened_column <- flattened_column[
      !is.na(flattened_column) & flattened_column != "NA"
    ]

    # Find unmatched codes by checking if each element is not in icd_mapping
    unmatched_codes <- flattened_column[
      !flattened_column %in% names(icd_mapping)
    ]

    # If there are unmatched codes, store them with the column name
    if (length(unmatched_codes) > 0) {
      unmatched_list[[col]] <- data.table(
        column = col,
        code = unmatched_codes
      )
    }
  }

  # Combine all unmatched codes from different columns into one data table
  if (length(unmatched_list) > 0) {
    final_unmatched_sources <- rbindlist(unmatched_list, fill = TRUE)

    # Group by column and code, calculate the count,
    # and sort by count in decreasing order
    final_unmatched_sources <- final_unmatched_sources[
      , .(count = .N),
      by = .(column, code)
    ][order(-count)]

    # Print the result using kable, showing up to end_nrow rows
    print(
      knitr::kable(
        final_unmatched_sources,
        format = "markdown",
        caption = "Invalid ICD-10 Codes Not Found in Thai Library"
      )
    )
  } else {
    # If no unmatched codes are found, print the success message
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }
}
