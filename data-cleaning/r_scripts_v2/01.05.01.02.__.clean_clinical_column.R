# # Helper function to clean and compare clinical columns
# clean_clinical_column <- function(dt, col_name) {
#   # Store the original column
#   dt[, (paste0(col_name, "_orig")) := dt[[col_name]]]

#   # Apply the clean_column function,
#   # which now returns a list of cleaned_col and is_covid
#   cleaned_data <- clean_column(dt[[col_name]])

#   # Extract cleaned column and is_covid flag
#   cleaned_col <- cleaned_data$cleaned_col
#   is_covid_flag <- cleaned_data$is_covid

#   # Update the cleaned column
#   dt[, (col_name) := cleaned_col]

#   # Store the original column as a string (for comparison purposes)
#   dt[, (paste0(col_name, "_orig")) := sapply(
#     get(paste0(col_name, "_orig")), toString
#   )]

#   # Convert the cleaned column to a string for comparison
#   dt[, (col_name) := sapply(get(col_name), toString)]

#   # Convert is_covid to logical (if it's currently a factor)
#   dt[, is_covid := as.logical(as.character(is_covid))]

#   # If is_covid is FALSE and is_covid_flag is TRUE, set is_covid to TRUE
#   dt[, is_covid := ifelse(
#     is_covid == FALSE & is_covid_flag == TRUE, TRUE, is_covid
#   )]

#   # Ensure is_covid is logical and replace any NA with FALSE
#   dt[is.na(is_covid), is_covid := FALSE]

#   # Compare cleaning results
#   # (only rows where the cleaned column differs from the original)
#   comparison <- dt[
#     !is.na(
#       get(paste0(
#         col_name,
#         "_orig"
#       ))
#     ) & get(col_name) != get(paste0(
#       col_name,
#       "_orig"
#     )),
#     .(
#       old_code = get(paste0(col_name, "_orig")),
#       new_code = get(col_name), count = .N
#     ),
#     by = .(get(paste0(col_name, "_orig")), get(col_name))
#   ]

#   return(comparison)
# }

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
