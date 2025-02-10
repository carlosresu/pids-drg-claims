final_combine_replace_empty_tables <- function(rboundlist, samplesizedivisor = sample_size_divisor, splitparts = split_parts, totalrows = total_rows) {
  # Calculate the sample size based on the divisor
  samplesize <- ceiling(totalrows / samplesizedivisor)

  combined_replace_empty <- rboundlist

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced_Percentage = character(),
      NA_Replaced_Percentage = character(),
      Character0_Replaced_Percentage = character()
    ))
  }

  # Ensure numeric conversion for all relevant columns
  combined_replace_empty[, `:=`(
    Empty_Replaced = as.numeric(Empty_Replaced),
    NA_Replaced = as.numeric(NA_Replaced),
    Character0_Replaced = as.numeric(Character0_Replaced)
  )]

  # Calculate total elements: use sample size * splitparts or totalrows as fallback
  total_elements <- samplesize * splitparts

  # Aggregate replacement counts by column
  combined_replace_empty <- combined_replace_empty[, .(
    Total_Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    Total_NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Total_Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]

  # Calculate and cap percentages at 100%
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = pmin((Total_Empty_Replaced / total_elements) * 100, 100),
    NA_Replaced_Percentage = pmin((Total_NA_Replaced / total_elements) * 100, 100),
    Character0_Replaced_Percentage = pmin((Total_Character0_Replaced / total_elements) * 100, 100)
  )]

  # Format percentages with two decimal places
  combined_replace_empty[, `:=`(
    Empty_Replaced_Percentage = sprintf("%.2f%%", Empty_Replaced_Percentage),
    NA_Replaced_Percentage = sprintf("%.2f%%", NA_Replaced_Percentage),
    Character0_Replaced_Percentage = sprintf("%.2f%%", Character0_Replaced_Percentage)
  )]

  # Return the formatted table
  return(combined_replace_empty[, .(
    Column, Empty_Replaced_Percentage, NA_Replaced_Percentage,
    Character0_Replaced_Percentage
  )])
}
