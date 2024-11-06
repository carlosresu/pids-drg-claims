# NEW REFACTORED CODE:
collapse_and_clean_icd_rvs_cols <- function(clin_icd_cols = NULL, clin_rvs_cols = NULL) {
  result <- list()

  # Helper function to clean, collapse, and split columns
  process_columns <- function(cols, is_icd = TRUE) {
    # cat("Input to clean column\n")
    # str(cols)
    # Step 1: Clean each column using `clean_column()`
    cleaned_results <- lapply(cols, clean_column)
    cleaned_columns <- lapply(cleaned_results, function(res) res$cleaned_col)
    # cat("After clean column\n")
    # str(cols)
    # Step 2: Collapse cleaned columns into a single string with "||" separators
    collapsed <- sapply(seq_along(cleaned_columns[[1]]), function(i) {
      combined <- unique(unlist(lapply(cleaned_columns, function(col) col[[i]])))
      combined <- combined[!combined %in% na_like_strings & combined != ""]

      # Collapse the cleaned values with "||" as a separator
      if (length(combined) > 0) {
        paste(combined, collapse = "||")
      } else {
        NA_character_
      }
    })
    # cat("After collapsing\n")
    # str(collapsed)
    # Step 3: First split using COVID/RVS/neoplasm codes
    split <- split_to_vector(collapsed)
    # cat("After split_to_vector\n")
    # str(split)

    if (is_icd) unlumped <- remove_lumped_icd_codes(split) # # Step 4: Further split any remaining lumped ICD-10 codes
    if (!is_icd) unlumped <- split
    # cat("After unlumping\n")
    # str(unlumped)
    return(unlumped)
  }

  # Process clin_icd_cols if provided
  if (!is.null(clin_icd_cols) && length(clin_icd_cols) > 0) {
    result$clin_icd <- process_columns(clin_icd_cols, is_icd = TRUE)
  }

  # Process clin_rvs_cols if provided
  if (!is.null(clin_rvs_cols) && length(clin_rvs_cols) > 0) {
    result$clin_rvs <- process_columns(clin_rvs_cols, is_icd = FALSE)
  }

  return(result)
}
