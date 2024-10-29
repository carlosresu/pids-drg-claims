# Function to collapse and clean ICD/RVS columns
collapse_and_clean_icd_rvs_cols <- function(clin_icd_cols = NULL, clin_rvs_cols = NULL) {
  result <- list()

  # Helper to clean, collapse, and split columns
  process_columns <- function(cols, is_icd = TRUE) {
    # Step 1: Clean each column using `clean_column()`
    cleaned_results <- lapply(cols, clean_column)
    cleaned_columns <- lapply(cleaned_results, function(res) res$cleaned_col)

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

    # Step 3: Apply `remove_lumped_icd_codes()` only if it's an ICD column
    if (is_icd) {
      collapsed <- remove_lumped_icd_codes(collapsed)
    }

    # Step 4: Split into vectors using the "||" separator
    split_to_vector(collapsed)
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
