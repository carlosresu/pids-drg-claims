clean_column <- function(col) {
  # Clean the column
  column_to_clean <- as.character(col)
  cleaned_col <- stri_trans_general(column_to_clean, "Latin-ASCII")
  cleaned_col <- toupper(cleaned_col)

  # Remove non-letter and non-digit characters (except delimiters like commas and pipes)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d,|]+", "")

  # Replace any NA-like strings with actual NA values
  cleaned_col[cleaned_col %in% na_like_strings] <- NA_character_

  # Save whether any COVID-related RVS is found
  covid_rvs_pattern <- paste0(covid_rvs, collapse = "|")
  is_covid <- stri_detect_regex(cleaned_col, covid_rvs_pattern)
  is_covid[is.na(is_covid)] <- FALSE # Ensure is_covid is logical and handle NAs

  # Restore slashes for certain neoplasm ICD-10 codes
  neopl <- setNames(neoplasms_dt_actual$icd10, gsub("/", "", neoplasms_dt_actual$icd10))
  matched_indices <- match(cleaned_col, names(neopl))
  cleaned_col[!is.na(matched_indices)] <- neopl[matched_indices[!is.na(matched_indices)]]

  # Return the cleaned column and is_covid flag
  return(list(
    cleaned_col = cleaned_col,
    is_covid = is_covid
  ))
}
