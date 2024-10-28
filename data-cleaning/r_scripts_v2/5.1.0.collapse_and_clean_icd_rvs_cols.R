collapse_and_clean_icd_rvs_cols <- function(clin_icd_cols = NULL, clin_rvs_cols = NULL) {
  ## Processes and collapses given ICD and RVS columns
  # clin_icd_cols: list of ICD columns to process
  # clin_rvs_cols: list of RVS columns to process

  # Initialize a list to store the results
  result <- list()

  # Process clin_icd_cols if provided
  if (!is.null(clin_icd_cols) && length(clin_icd_cols) > 0) {
    # Clean each column using the clean_column function
    cleaned_results <- lapply(clin_icd_cols, clean_column)
    # Extract cleaned_col from the results
    cleaned_columns <- lapply(cleaned_results, function(res) res$cleaned_col)

    # Collapse the cleaned columns into a single column
    collapsed_icd <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
      # Combine corresponding rows and remove empty strings or NA-like values
      combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
      combined <- combined[!combined %in% na_like_strings & combined != ""]

      # Collapse the cleaned values using "||" as the separator
      if (length(combined) > 0) {
        paste(combined, collapse = "||")
      } else {
        NA_character_
      }
    })

    # Handle any lumped ICD codes
    collapsed_icd <- remove_lumped_icd_codes(collapsed_icd)
    # Convert cleaned string to vector
    processed_icd <- split_to_vector(collapsed_icd)

    # Store the processed ICD column in the result
    result$clin_icd <- processed_icd
  }

  # Process clin_rvs_cols if provided
  if (!is.null(clin_rvs_cols) && length(clin_rvs_cols) > 0) {
    # Clean each column using the clean_column function
    cleaned_results <- lapply(clin_rvs_cols, clean_column)
    # Extract cleaned_col from the results
    cleaned_columns <- lapply(cleaned_results, function(res) res$cleaned_col)

    # Collapse the cleaned columns into a single column
    collapsed_rvs <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
      # Combine corresponding rows and remove empty strings or NA-like values
      combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
      combined <- combined[!combined %in% na_like_strings & combined != ""]

      # Collapse the cleaned values using "||" as the separator
      if (length(combined) > 0) {
        paste(combined, collapse = "||")
      } else {
        NA_character_
      }
    })

    # Convert cleaned string to vector
    processed_rvs <- split_to_vector(collapsed_rvs)

    # Store the processed RVS column in the result
    result$clin_rvs <- processed_rvs
  }

  # Return the list of processed columns
  return(result)
}
