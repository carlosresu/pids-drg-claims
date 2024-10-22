clean_clinical_column <- function(column) {
  # Store the original column for comparison purposes
  original_col <- column

  # Apply the clean_column function to clean the input column
  cleaned_data <- clean_column(column)

  # Extract cleaned column and is_covid flag
  cleaned_col <- cleaned_data$cleaned_col
  is_covid_flag <- cleaned_data$is_covid

  # Convert both original and cleaned columns to strings for comparison
  original_col_str <- sapply(original_col, toString)
  cleaned_col_str <- sapply(cleaned_col, toString)

  # Initialize the is_covid flag as logical (defaulting to FALSE if NA)
  is_covid <- ifelse(is.na(is_covid_flag), FALSE, as.logical(is_covid_flag))

  # Create a comparison DataFrame for rows where the values differ
  comparison <- data.table::data.table(
    old_code = original_col_str,
    new_code = cleaned_col_str
  )[old_code != new_code,
    .(old_code, new_code, count = .N),
    by = .(old_code, new_code)
  ]

  # Return the comparison result
  return(list(
    original_col = original_col,
    cleaned_col = cleaned_col,
    is_covid = is_covid,
    comparison = comparison
  ))
}
