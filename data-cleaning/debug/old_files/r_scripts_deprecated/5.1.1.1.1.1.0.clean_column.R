clean_column <- function(column_to_clean) {
  # Convert the column to uppercase and ASCII format
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Remove non-letter and non-digit characters from the string (except delimiters like commas and pipes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d,|]+", "")

  # Replace any NA-like strings (as defined) with actual NA values
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # # Save whether any COVID-related RVS is found (TRUE if found, FALSE otherwise)
  # covid_rvs_pattern <- paste0(covid_rvs, collapse = "|")
  # is_covid <- stri_detect_regex(cleaned_col, covid_rvs_pattern)

  # Restore slashes for certain neoplasm ICD-10 codes, where slashes are important
  neopl <- setNames(neoplasms_dt_actual$icd10, gsub("/", "", neoplasms_dt_actual$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column and is_covid flag
  return(
    # list(
    cleaned_col # = cleaned_col
    # , is_covid = is_covid
    # )
  )
}
