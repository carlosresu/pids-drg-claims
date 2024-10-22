final_combine_replace_empty_tables <- function(
    summaries, field, tmp_nrow = 10) {
  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Total_Empty_Replaced = integer(),
      Total_NA_Replaced = integer(),
      Total_Character0_Replaced = integer(),
      Total_Elements = integer(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE),
    Total_Elements = if (to_sample) sample_size * split_parts else total_rows
  ), by = Column]

  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = (Total_Empty_Replaced / Total_Elements) * 100,
    NA_Replaced_Percentage = (Total_NA_Replaced / Total_Elements) * 100,
    Character0_Replaced_Percentage = (Total_Character0_Replaced / Total_Elements) * 100
  )]

  # Format percentages as "XX.X%"
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    NA_Replaced_Percentage = sprintf("%.2f%%", NA_Replaced_Percentage),
    Character0_Replaced_Percentage = sprintf("%.2f%%", Character0_Replaced_Percentage)
  )]

  combined_replace_empty <- combined_replace_empty[
    order(
      -as.numeric(gsub("%", "", Empty_Replaced_Percentage)),
      -as.numeric(gsub("%", "", NA_Replaced_Percentage)),
      -as.numeric(gsub("%", "", Character0_Replaced_Percentage))
    )
  ]

  combined_replace_empty <- combined_replace_empty

  return(combined_replace_empty[, .(
    Column,
    Empty_Replaced_Percentage,
    NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}
