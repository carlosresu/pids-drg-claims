# Aggregates empty replacements and returns counts.
combine_replace_empty_tables <- function(rboundlist) {
  combined <- rboundlist
  if (nrow(combined) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }
  return(combined[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)])
}
