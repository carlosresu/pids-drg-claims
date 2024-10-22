replace_empty_with_na <- function(dt, to_view_checks = TRUE) {
  ## Replaces empty strings with NA across an entire data.table.
  # dt: input data.table
  # to_view_checks: flag to track the replacement count for checks.
  # Identify columns that are character, factor, or list
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  # Create a summary table for tracking replacements
  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  # Loop through each identified column
  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      # Count how many empty, "NA", or "character(0)" entries exist
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Replace all empty, "NA", and "character(0)" values with actual NA
    dt[
      get(
        col_name
      ) == "" | get(col_name) == "NA" | get(col_name) == "character(0)",
      (col_name) := NA_character_
    ]

    # If the column is a factor, ensure that NA is a valid level
    if (is.factor(col)) {
      set(dt,
        j = col_name,
        value = factor(dt[[col_name]],
          levels = c(levels(col), NA)
        )
      )
    }
    # Update the replacement summary
    replacement_summary <- rbind(replacement_summary, data.table(
      Column = col_name,
      Empty_Replaced = empty_count,
      NA_Replaced = na_count,
      Character0_Replaced = char0_count
    ))
  }
  # Filter out columns where no replacements were made
  replacement_summary <- replacement_summary[
    Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
  ]

  return(
    list(
      # Return the modified data.table
      return_data = dt,
      # Return the summary of replacements
      return_replacement_summary = replacement_summary
    )
  )
}
