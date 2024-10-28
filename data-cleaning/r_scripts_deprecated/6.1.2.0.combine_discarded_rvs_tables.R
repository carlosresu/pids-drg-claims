combine_discarded_rvs_tables <- function(
    summaries, field) {
  #' @title Combine Discarded RVS Tables
  #'
  #' @description This function combines discarded RVS tables
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains discarded RVS codes.
  #' @return data.table. The combined discarded RVS table.

  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- combined_discarded

  return(combined_discarded)
}
