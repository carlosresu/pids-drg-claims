remove_lumped_icd_codes <- function(column) {
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  # Use regex to add "||" between letters and digits
  # in the ICD codes (e.g., A123B456 -> A123||B456)
  modified_column <- stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Z]\\d{2,4})", "||",
    # Specify regex options for the replacement
    opts_regex = stri_opts_regex()
  )

  # Return the modified column with ICD codes split
  return(modified_column)
}
