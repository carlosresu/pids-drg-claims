combine_comparison_tables <- function(
    summaries, comparison_field) {
  #' @title Combine Comparison Tables
  #'
  #' @description This function combines comparison tables from
  #' multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param comparison_field character. The field in the summaries to compare.
  #' @return data.table. The combined comparison table with absolute differences in character lengths.

  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code)]
    }
    return(summary_data)
  })

  # Combine all the data
  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      diff_chars = integer()
    ))
  }

  # Trim whitespaces and calculate the absolute difference in character lengths for unique pairs
  combined_comparison[, `:=`(
    old_code = gsub("\\s", "", iconv(old_code, to = "UTF-8")),
    new_code = gsub("\\s", "", iconv(new_code, to = "UTF-8"))
  )]
  combined_comparison[, diff_chars := abs(nchar(old_code) - nchar(new_code))]

  # Keep only unique old_code to new_code combinations
  unique_combinations <- unique(combined_comparison)

  # Order by the absolute character difference and limit the number of rows
  unique_combinations <- unique_combinations[order(-diff_chars)]
  unique_combinations <- unique_combinations

  return(unique_combinations)
}
