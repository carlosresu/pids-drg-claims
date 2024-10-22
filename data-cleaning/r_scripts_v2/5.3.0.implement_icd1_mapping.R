implement_icd10_mapping <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10) {
  # Step 1: Get all unique ICD codes from the provided
  # columns (c1, c2, and clin_icd)
  icds <- get_unique_icd_codes(c1, c2, clin_icd)

  # Step 2: Create an environment for Thai ICD10 codes for faster lookup
  # This uses the unique set of ICD10 codes in the thai_icd10 table.
  thai_icd10_env <- create_thai_icd10_environment(
    unique(thai_icd10$CODE)
  )

  # Step 3: Create a second environment for Thai ICD10
  # neoplasm codes (those with slashes '/')
  neoplasms_env <- create_thai_icd10_environment(
    unique(thai_icd10[grepl("/", thai_icd10$CODE), "CODE"])
  )

  # Step 4: Find direct matches between the provided ICD
  # codes (icds) and the Thai ICD10 environment
  direct_match_codes <- find_direct_icd_matches(
    icds, thai_icd10_env
  )

  # Step 5: Generate the full ICD10 mapping for the ICD codes,
  # considering both Thai ICD10 environment and neoplasms environment.
  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env, covid_rvs
  )
  # Extract the mapping and the count of modified mappings
  icd_mapping <- icd_mapping_info$icd_mapping_res

  modified_count <- icd_mapping_info$modified_count

  # Step 6: Identify ICD codes that were not successfully mapped.
  unmatched_icds <- setdiff(icds, names(icd_mapping))

  # Step 7: If there are unmatched ICD codes, gather
  # their source information (c1, c2, clin_icd)
  # and the count of occurrences in each column.
  if (length(unmatched_icds) > 0) {
    unmatched_sources <- data.table(
      code = unmatched_icds, source = NA_character_, count = 0
    )
    # Loop over the columns (c1, c2, clin_icd) to fill
    # in source and count details for unmatched codes.
    for (col_name in c("c1", "c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[
        code %in% unlist(col_values),
        source := col_name
      ]
      unmatched_sources[
        code %in% unlist(col_values),
        count := count + table(unlist(col_values))[code]
      ]
    }
    # Order unmatched codes by their occurrence count in descending order
    unmatched_sources <- unmatched_sources[order(-count)]
  } else {
    # If there are no unmatched codes, return an empty data.table.
    unmatched_sources <- data.table()
  }

  # Step 8: Create a data.table containing the
  # mapping between PHL (input) ICD10 codes
  # and Thai DRG ICD10 codes.
  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    thai_icd10 = unlist(icd_mapping)
  )

  # Step 9: DEPRECATED

  # Step 10: Create an environment from the ICD10
  # mapping for fast lookup during column mapping.
  icd10_env <- list2env(
    setNames(as.list(icd10_map$thai_icd10), icd10_map$phl_icd10)
  )

  # Step 11: Apply the ICD10 mapping to the columns c1, c2, and clin_icd
  # This updates these columns based on the generated ICD10 environment.
  mapped_columns <- apply_icd10_mapping_to_columns(
    c1, c2, clin_icd, icd10_env
  )

  # Step 12: Return a list containing the mapped columns
  # and other information for further checks and outputs:
  # - The updated columns (c1, c2, clin_icd)
  # - The full ICD10 map (icd10_map_dt)
  # - The unique ICD codes
  # - Direct matches found
  # - Unmatched ICDs and their source information
  return(
    list(
      c1 = mapped_columns$c1,
      c2 = mapped_columns$c2,
      clin_icd = mapped_columns$clin_icd,
      icd10_map_dt = icd10_map,
      unique_icds = icds,
      direct_matches = direct_match_codes,
      unmatched = unmatched_icds,
      unmatched_sources = unmatched_sources,
      icd_mapping_res = icd_mapping
    )
  )
}
