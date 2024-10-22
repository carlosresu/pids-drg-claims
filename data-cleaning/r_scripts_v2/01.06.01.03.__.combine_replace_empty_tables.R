combine_replace_empty_tables <- function(
    summaries, field) {
  #' @title Combine Replace Empty Tables
  #'
  #' @description This function combines tables for replaced
  #' empty values from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains information on replaced empty values.
  #' @return data.table. The combined replace empty tables.

  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]
  combined_replace_empty <- combined_replace_empty[
    order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)
  ]
  combined_replace_empty <- head(
    combined_replace_empty,
    Inf
  )

  return(combined_replace_empty)
}
