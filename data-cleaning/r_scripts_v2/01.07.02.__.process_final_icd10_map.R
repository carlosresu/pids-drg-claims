process_final_icd10_map <- function(icd10_map_dt) {
  str(icd10_map_dt)
  icd10_map_dt <- icd10_map_dt[phl_icd10 != thai_icd10]

  if (nrow(icd10_map_dt) == 0) {
    return(data.table(
      phl_icd10 = character(),
      thai_icd10 = character(),
      char_diff = numeric()
    ))
  }

  # Calculate the absolute difference in character length and add char_diff column
  icd10_map_dt[, char_diff := abs(nchar(phl_icd10) - nchar(thai_icd10))]

  # Sort by descending absolute difference in character length
  icd10_map_dt <- icd10_map_dt[order(-char_diff)]

  # Select the top rows based on tmp_nrow
  final_icd10_map <- icd10_map_dt

  return(final_icd10_map)
}
