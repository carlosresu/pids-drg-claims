# Helper function to clean, collapse, and split columns
collapse_and_clean_icd_rvs_cols <- function(cols, is_icd = TRUE) {
  # Step 1: Clean each column using `clean_column()`
  cleaned_results <- lapply(cols, clean_column)
  cleaned_columns <- lapply(cleaned_results, \(res) res$cleaned_col)
  # Step 2: Collapse cleaned columns into a single string with "||" separators
  collapsed <- sapply(seq_along(cleaned_columns[[1]]), \(i) {
    combined <- unique(unlist(lapply(cleaned_columns, \(col) col[[i]])))
    combined <- combined[!combined %chin% na_like_strings & combined != ""]

    # Collapse the cleaned values with "||" as a separator
    if (length(combined) > 0) {
      paste(combined, collapse = "||")
    } else {
      NA_character_
    }
  })

  # Step 3: First split using COVID/RVS/neoplasm codes
  split <- split_to_vector(collapsed)

  # Step 4: Further split any remaining lumped ICD-10 codes
  if (is_icd) {
    return(remove_lumped_icd_codes(split))
  } else if (!is_icd) {
    return(split)
  }
}
