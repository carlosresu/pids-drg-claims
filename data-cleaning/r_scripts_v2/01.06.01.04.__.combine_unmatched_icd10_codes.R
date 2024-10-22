combine_unmatched_icd10_codes <- function(
    summaries, field) {
  #' @title Combine Unmatched ICD-10 Codes
  #'
  #' @description This function combines unmatched ICD-10 codes
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains unmatched ICD-10 codes.
  #' @return data.table. The combined unmatched ICD-10 codes table.

  combined_list <- lapply(summaries, function(summary) summary[[field]])
  combined_table <- rbindlist(combined_list, fill = TRUE)
  combined_table <- combined_table[, .(count = sum(count)),
    by = .(code, source)
  ]
  combined_table <- combined_table[order(-count)]
  return(combined_table)
}
