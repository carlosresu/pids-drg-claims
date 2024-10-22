# Define process_chunk (not to be confused with process_part)
# that processes each part in nthreads chunks
process_chunk <- function(chunk,
                          to_view_checks = to_view_checks,
                          rvs_icd9 = rvs_icd9,
                          tdrg_icd10 = tdrg_icd10,
                          acc_pdx = acc_pdx) {
  # Step 1: Define main clean data function,
  # which does majority of the data cleaning on the claims file

  # Step 2: Clean the data in the 'chunk'
  # See function(s) above
  clean_result <- clean_data(chunk)
  chunk <- clean_result$return_data # Update chunk with cleaned data

  # Step 3: DEPRECATED

  # Define function to map RVS codes to ICD9 codes

  # Step 4:
  # Map clinical RVS (Relative Value Scale) codes to ICD9 using 'rvs_icd9'
  # See function(s) above
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs)
  # Store the mapped ICD9 list into the chunk
  chunk[, icd9_list := rvs_mapping_result$icd9_list]

  # Step 5: DEPRECATED

  # Step 6: Extract columns c1, c2, and clin_icd for ICD10 mapping
  c1 <- chunk$c1
  c2 <- chunk$c2
  clin_icd <- chunk$clin_icd

  # Step 7:
  # Perform ICD10 mapping using the extracted columns
  # and 'tdrg_icd10' mapping data
  # See function(s) above
  icd10_mapping_result <- implement_icd10_mapping(
    c1, c2, clin_icd
  )

  # Update chunk with the mapped ICD10 codes
  chunk[, c1 := icd10_mapping_result$c1]
  chunk[, c2 := icd10_mapping_result$c2]
  chunk[, clin_icd := icd10_mapping_result$clin_icd]

  # Step 8:
  # Replace any empty strings with NA values,
  # returning a summary of replacements
  # See cleaning-functions.R
  res2 <- replace_empty_with_na(dt = chunk)
  # Update chunk with cleaned data
  chunk <- res2$return_data
  # Store replacement summary
  empty_strings_replaced_2 <- res2$return_replacement_summary

  # Step 9: Define a function to remove all whitespace from character vectors

  # Step 10:
  # Apply the remove_whitespace function to the
  # list columns 'c1', 'c2', and 'clin_icd'
  # See function(s) above
  chunk[, c1 := lapply(c1, remove_whitespace)]
  chunk[, c2 := lapply(c2, remove_whitespace)]
  chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]

  # Step 11: DEPRECATED

  # Step 12: Apply a function to find the primary
  # diagnosis (pdx) based on 'c1', 'c2', and 'clin_icd'
  # See function(s) above
  pdx_result <- apply_find_pdx(
    chunk$c1, chunk$c2, chunk$clin_icd
  )

  # Step 13: Store the primary diagnosis (pdx) and its code into the chunk
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Step 14: Define a function to remove the primary
  # diagnosis (pdx) from list columns (c1, c2, clin_icd)

  # Step 15: Apply the 'remove_pdx_from_list' function
  # to each row of 'c1', 'c2', and 'clin_icd'
  chunk[, c1 := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], c1[[i]]))
  )]
  chunk[, c2 := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], c2[[i]]))
  )]
  chunk[, clin_icd := lapply(
    seq_len(.N),
    function(i) as.character(remove_pdx_from_list(pdx[i], clin_icd[[i]]))
  )]

  # Step 16: Create a summary by combining clean
  # results and ICD10 mapping information
  chunk_summary <- modifyList(
    clean_result$return_summary,
    list(
      unique_icds = icd10_mapping_result$unique_icds,
      direct_matches = icd10_mapping_result$direct_matches,
      unmatched = icd10_mapping_result$unmatched,
      unmatched_sources = icd10_mapping_result$unmatched_sources,
      icd10_map_dt = icd10_mapping_result$icd10_map_dt,
      rvss = rvs_mapping_result$rvss,
      mappable_rvs = rvs_mapping_result$mappable_rvs,
      unmappable_rvs = rvs_mapping_result$unmappable_rvs,
      multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
      without_drg = rvs_mapping_result$without_drg
    )
  )

  # Step 17: Optionally trigger garbage collection to reduce memory usage
  gc()

  # Step 18: Return the processed chunk and summary information
  return(
    list(
      # Return the processed chunk data
      return_chunk = chunk,
      # Return the summary for checks and outputs
      return_summary = chunk_summary
    )
  )
}
