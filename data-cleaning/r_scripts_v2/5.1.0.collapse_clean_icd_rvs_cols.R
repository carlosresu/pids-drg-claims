collapse_clin_cols <- function(cols, is_icd) {
  # Step 2: Collapse cleaned columns into a single string with "||" separators
  collapsed <- lapply(seq_along(cols[[1]]), function(i) {
    combined <- unique(unlist(lapply(cols, function(col) col[[i]])))
    combined <- combined[!combined %chin% na_like_strings & combined != ""]

    # Return the cleaned values as a vector (avoid collapsing to string)
    if (length(combined) > 0) {
      combined
    } else {
      character(0) # Return an empty vector instead of NA
    }
  })

  # Step 3: First split using COVID/RVS/neoplasm codes
  split <- split_to_vector(collapsed)

  # Step 4: Further split any remaining lumped ICD-10 codes
  unlumped <- if (is_icd) remove_lumped_icd_codes(split) else split

  return(unlumped) # Always return a list of vectors
}
