# Function to clean and split the column by different delimiters
clean_and_split <- function(col) {
  # Clean the column using the clean_column function
  result <- clean_column(col)
  is_covid <- result$is_covid
  cleaned_col <- result$cleaned_col

  # Split by multiple delimiters (comma, single pipe, or double pipe) while handling spaces
  split_col <- strsplit(cleaned_col, "\\s*,\\s*|\\|\\||\\|")

  # Return the split column
  return(split_col)
}
